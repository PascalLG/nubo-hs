-----------------------------------------------------------------------------
-- Nubo Client Application
-- Copyright (c) 2018, Pascal Levy
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

module CmdList (
      cmdList
    , helpList
) where

import Control.Monad (forM, forM_)
import Control.Monad.Trans (liftIO)
import System.Directory (getHomeDirectory, getDirectoryContents, doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
import System.FilePath ((</>))
import System.Info (os)
import System.IO.Error (catchIOError)
import Config
import Misc
import PrettyPrint
import Environment
import Error
import Database

-----------------------------------------------------------------------------

-- | Parse arguments for the 'list' command.
--
cmdList :: [String] -> EnvIO ExitStatus
cmdList args = do
    result <- parseArgsM args [OptionCSV]
    case result of
        Left errs        -> mapM_ putErr errs >> return StatusInvalidCommand
        Right (_, (a:_)) -> putErr (ErrExtraArgument a) >> return StatusInvalidCommand
        Right (opts, []) -> doList (OptionCSV `elem` opts)

-- | Execute the 'list' command.
--
doList :: Bool -> EnvIO ExitStatus
doList csv = liftIO $ do
    list <- getHomeDirectory >>= findNuboDrives 5 ""
    forM_ list $ if csv then printCsv 
                        else let maxlen = maximum $ map (length . fst) list
                             in printStd maxlen
    return StatusOK

    where
        printStd :: Int -> (String, String) -> IO ()
        printStd maxlen (path, url) = putStrLn (path ++ replicate (maxlen - length path) ' ' ++ " -> " ++ url)

        printCsv :: (String, String) -> IO ()
        printCsv (path, url) = putStrF $ toCSV [path, url]

-- | Recursively browse directories to find nubo drives. To save
-- time, only the first sublevels of the user's home folder are
-- explored. Moreover, some specifics folders that are very
-- unlikely to contain a drive are ignored.
--
findNuboDrives :: Int -> FilePath -> FilePath -> IO [(FilePath, String)]
findNuboDrives level curr home = do
    let abspath = home </> curr
    names <- getDirectoryContents abspath `catchIOError` (\_ -> return [])
    r <- concat <$> forM names (\name -> do
        let path = curr </> name
        isdir <- doesDirectoryExist (home </> path)
        islink <- pathIsSymbolicLink (home </> path) `catchIOError` (\_ -> return True)
        if isdir &&
           not islink &&
           level > 0 &&
           any (/= '.') name &&
           path `notElem` excluded then findNuboDrives (level - 1) path home
                                   else return [])

    u <- isNuboDrive (abspath </> nuboDatabase)
    return $ if null u then r
                       else (abspath, u):r

    where
        isNuboDrive :: FilePath -> IO String
        isNuboDrive path = do
            b <- doesFileExist path `catchIOError` (\_ -> return False)
            if b then isValidNuboDB path else return ""

        excluded :: [FilePath]
        excluded = case os of
            "darwin"  -> [ ".config", ".cache", ".local", ".Trash", "Applications", "Library", "Pictures/iPhoto Library", "Music/iTunes" ]
            "linux"   -> [ ".config", ".cache", ".local" ]
            "mingw32" -> [ "AppData", "MicrosoftEdgeBackups" ]
            _         -> [ ]

-----------------------------------------------------------------------------

-- | Print usage for the remote command.
--
helpList :: EnvIO ()
helpList = do
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:nubo list}} [{y:options}}]"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    Recursively search the user’s home directory and its descendent folders"
    putLine $ "    for all {y:nubo}} synced folders. For each entry it founds, the command"
    putLine $ "    prints the absolute path of the folder and the URL of the server it"
    putLine $ "    synchonises with."
    putLine $ ""
    putLine $ "    For performance reasons, only the five first levels of subdirectories"
    putLine $ "    are explored. Locations that are very unlikely to contain such a synced"
    putLine $ "    folder are ignored as well. The exact list of these ignored locations"
    putLine $ "    depends on your system."
    putLine $ ""
    putLine $ "{*:OPTIONS}}"
    putLine $ "    {y:-a}}, {y:--no-ansi}}   Do not use ANSI escape sequences in output messages."
    putLine $ "    {y:-c}}, {y:--csv}}       Format the command output as CSV."
    putLine $ ""

-----------------------------------------------------------------------------
