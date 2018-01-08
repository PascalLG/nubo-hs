-----------------------------------------------------------------------------
-- Nubo Client Application
-- Copyright (c) 2017, Pascal Levy
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-----------------------------------------------------------------------------

module Database (
      nuboDatabase
    , findNuboDB
    , createDBAndRun
    , openDBAndRun
    , saveConfig
    , getConfig
    , deleteConfig
    , Config(..)
    , getPatternList
    , addPattern
    , deletePattern
    , getFileList
    , updateFileInfo
    , deleteFileInfo
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (doesFileExist)
import System.FilePath
import Data.Binary (Binary, encode, decode)
import Data.List (sortBy)
import Control.Exception (bracket)
import Control.Monad.State
import qualified Data.ByteString as B
import Config
import Environment
import Misc
import Error
import Matching
import FileHelper (UFilePath)

-----------------------------------------------------------------------------
-- Database connection and initialization.

-- | Try to find a valid Nubo database by browsing directories
-- up from the current one to the root. Return the folder where
-- the database was found.
--
findNuboDB :: IO (Maybe FilePath)
findNuboDB = getWorkingDirectory >>= find . strip
    where
        find :: FilePath -> IO (Maybe FilePath)
        find dir = do
            let path = dir </> nuboDatabase
            ok <- doesFileExist path <&&> isReady path
            if ok then return $ Just dir
                  else if isDrive dir then return Nothing
                                      else find $ takeDirectory dir

        strip :: FilePath -> FilePath
        strip path
            | null path                                         = []
            | isPathSeparator (last path) && not (isDrive path) = (strip . init) path
            | otherwise                                         = path

        isReady :: FilePath -> IO Bool
        isReady path = maybe False id <$> bracket (connectSqlite3 path) 
                                                  (disconnect) 
                                                  (getConfigIO CfgReady)

-- | Create or open the Nubo database and prepare it by creating the
-- required tables. If the database already exists, all data are
-- dropped except for the computer UUID. Then, execute the specified
-- action in a Reader monad with the database handle.
--
createDBAndRun :: EnvIO ExitStatus -> EnvIO ExitStatus
createDBAndRun action = bracketEnvIO create cleanup exec
    where
        create :: IO Connection
        create = do
            db <- connectSqlite3 nuboDatabase
            _ <- run db "CREATE TABLE IF NOT EXISTS config(config_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL UNIQUE, value BLOB)" []
            _ <- run db "CREATE TABLE IF NOT EXISTS file(file_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, filename TEXT NOT NULL UNIQUE, hash TEXT NOT NULL)" []
            _ <- run db "CREATE TABLE IF NOT EXISTS ignore(ignore_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, pattern BLOB)" []
            _ <- run db "DELETE FROM config WHERE name <> ?" [toSql (show CfgComputerUUID)]
            _ <- run db "DELETE FROM file" []
            _ <- run db "DELETE FROM ignore" []
            commit db
            return db

        cleanup :: Connection -> IO ()
        cleanup db = do
            disconnect db
            setDatabaseAttributes nuboDatabase

        exec :: Connection -> EnvIO ExitStatus
        exec db = do
            env <- get
            put env { dbConn = Just db }
            action

-- | Open the Nubo database and execute the specified action in the
-- Reader monad. Print an error message if the database does not
-- exist.
--
openDBAndRun :: EnvIO ExitStatus -> EnvIO ExitStatus
openDBAndRun action = do
    optpath <- liftIO findNuboDB
    case optpath of
        Nothing    -> putErr ErrDatabaseNotFound >> return StatusDatabaseNotFound
        Just path  -> bracketEnvIO (connectSqlite3 (path </> nuboDatabase))
                                   (disconnect)
                                   (\db -> do
                                       env <- get
                                       put env { dbConn = Just db, rootPath = Just path }
                                       action)

-----------------------------------------------------------------------------
-- Config management.

-- | Enumeration of all configuration parameters that are stored
-- in database.
--
data Config = 
      CfgReady
    | CfgComputerUUID
    | CfgRemoteURL
    | CfgMasterKey
    | CfgAuthToken
    deriving (Eq)

-- | Implement Show to convert a config parameter to a string that
-- is suitable for SQL.
--
instance Show Config where
    show CfgReady        = "ready"
    show CfgComputerUUID = "computer"
    show CfgRemoteURL    = "url"
    show CfgMasterKey    = "key"
    show CfgAuthToken    = "auth"
 
-- | Save a configuration parameter.
--
saveConfigIO :: (Binary a) => Config -> a -> Connection -> IO ()
saveConfigIO config value db = do
    let blob = toSql (encode value)
    let name = toSql (show config)
    r <- quickQuery' db "SELECT config_id FROM config WHERE name=? LIMIT 1" [name]
    _ <- case r of
            [[i]] -> run db "UPDATE config SET value=? WHERE config_id=?" [blob, i]
            _     -> run db "INSERT INTO config(name, value) VALUES (?, ?)" [name, blob]
    commit db

-- | Retrieve a configuration parameter.
--
getConfigIO :: (Binary a) => Config -> Connection -> IO (Maybe a)
getConfigIO config db = do
    r <- quickQuery' db "SELECT value FROM config WHERE name=? LIMIT 1" [toSql (show config)]
    return $ case r of
                [[blob]] -> Just $ decode (fromSql blob)
                _        -> Nothing

-- | Save a configuration parameter in the EnvIO monad.
--
saveConfig :: Binary a => Config -> a -> EnvIO ()
saveConfig config value = getDbConn >>= \db -> liftIO $ saveConfigIO config value db

-- | Retrieve a configuration parameter in the EnvIO monad.
--
getConfig :: Binary a => Config -> EnvIO (Maybe a)
getConfig config = getDbConn >>= \db -> liftIO $ getConfigIO config db

-- | Delete a configuration parameter in the EnvIO monad.
--
deleteConfig :: Config -> EnvIO ()
deleteConfig config = getDbConn >>= \db ->
    liftIO $ run db "DELETE FROM config WHERE name=?" [toSql (show config)] >>
             commit db

-----------------------------------------------------------------------------
-- Ignored file pattern management.

-- | Return a sorted list of all ignored file patterns with their
-- raw identifier.
--
getPatternList :: EnvIO [(Int, Pattern)]
getPatternList = getDbConn >>= \db -> sortBy comp 
                                  <$> map dec 
                                  <$> liftIO (quickQuery' db "SELECT ignore_id, pattern FROM ignore" [])
    where
        dec :: [SqlValue] -> (Int, Pattern)
        dec [f, h] = (fromSql f, decode (fromSql h))
        dec _      = error "unexpected pattern"

        comp :: (Int, Pattern) -> (Int, Pattern) -> Ordering
        comp (_, p1) (_, p2) = compare p1 p2

-- | Add a pattern to the list of ignored files. There is no check
-- for uniqueness. It is up to the caller to avoid adding already
-- existing patterns.
--
addPattern :: Pattern -> EnvIO ()
addPattern pattern = getDbConn >>= \db ->
    liftIO $ run db "INSERT INTO ignore(pattern) VALUES (?)" [toSql (encode (pattern))] >>
             commit db

-- | Delete a pattern from its raw identifier.
--
deletePattern :: Int -> EnvIO ()
deletePattern rawid = getDbConn >>= \db ->
    liftIO $ run db "DELETE FROM ignore WHERE ignore_id=? LIMIT 1" [toSql rawid] >>
             commit db

-----------------------------------------------------------------------------
-- File list management.

-- | Retrieve the list of files and their hashes.
--
getFileList :: EnvIO [(UFilePath, B.ByteString)]
getFileList = do
    db <- getDbConn
    result <- liftIO $ quickQuery' db "SELECT filename, hash FROM file" []
    return $ map dec result
    where
        dec :: [SqlValue] -> (UFilePath, B.ByteString)
        dec [f, h] = (fromSql f, fromSql h)
        dec _      = error "unexpected pattern"

-- | Create or update information about a file.
--
updateFileInfo :: UFilePath -> B.ByteString -> EnvIO (Either Error ())
updateFileInfo filename hash = do
    db <- getDbConn
    r <- liftIO $ quickQuery' db "SELECT file_id FROM file WHERE filename=? LIMIT 1" [toSql filename]
    _ <- case r of
            [[f]] -> liftIO $ run db "UPDATE file SET hash=? WHERE file_id=?" [toSql hash, f]
            _     -> liftIO $ run db "INSERT INTO file(filename, hash) VALUES(?,?)" [toSql filename, toSql hash]
    liftIO $ commit db
    return $ Right ()

-- | Delete information about a file.
--
deleteFileInfo :: UFilePath -> EnvIO (Either Error ())
deleteFileInfo filename = do
    db <- getDbConn
    _ <- liftIO $ run db "DELETE FROM file WHERE filename=?" [toSql filename]
    liftIO $ commit db
    return $ Right ()

-----------------------------------------------------------------------------
