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

module CmdSync.Actions (
      getActionInfo
) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Monad.Trans (liftIO)
import Control.Exception (catch)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (removeFile, removeDirectory, createDirectory)
import System.IO.Error
import PrettyPrint
import Environment
import Error
import MsgPack
import Database
import FileHelper
import WebService
import Archive
import CmdSync.Logic

-----------------------------------------------------------------------------
-- Synchronisation actions.

-- | Given a FileAction value, return descriptive strings, a color, and
-- a function that performs that action.
--
getActionInfo :: UFilePath -> FilePath -> Action -> Bool -> (String, Maybe String, AnsiColor, EnvIO (Either Error ()))
getActionInfo upath path (ActionSendFile hash)    csv = ("send", Nothing, AnsiDefault, doSendFile hash upath path csv)
getActionInfo upath path (ActionSendDir)          _   = ("send", Nothing, AnsiDefault, doSendDir upath path)
getActionInfo upath path (ActionGetFile hash)     csv = ("get", Nothing, AnsiDefault, doGetFile hash upath path csv)
getActionInfo upath path (ActionGetDir)           _   = ("get", Nothing, AnsiDefault, doGetDir upath path)
getActionInfo upath path (ActionDeleteLocalFile)  _   = ("delete local", Nothing, AnsiDefault, doDeleteLocalFile upath path)
getActionInfo upath path (ActionDeleteLocalDir)   _   = ("delete local", Nothing, AnsiDefault, doDeleteLocalDir upath path)
getActionInfo upath _    (ActionDeleteRemote)     _   = ("delete remote", Nothing, AnsiDefault, doDeleteRemote upath)
getActionInfo upath _    (ActionUpdateCache hash) _   = ("update cache", Nothing, AnsiDefault, updateFileInfo upath hash)
getActionInfo upath _    (ActionDeleteFromCache)  _   = ("update cache", Nothing, AnsiDefault, deleteFileInfo upath)
getActionInfo _     _    (ActionConflict ctype)   _   = ("conflict", Just (show ctype), AnsiRed, return $ Right ())
getActionInfo upath _    (ActionError)            _   = ("error", Just "internal inconsistency", AnsiRed, deleteFileInfo upath)

-- | Upload a file to the cloud.
--
doSendFile :: B.ByteString -> UFilePath -> FilePath -> Bool -> EnvIO (Either Error ())
doSendFile hash upath path csv = getMasterKey >>=? liftIO . archive path >>=? send >>? updateFileInfo upath hash
    where
        send :: B.ByteString -> EnvIO (Either Error MsgValue)
        send content = do
            mtime <- liftIO $ getFileModificationTime path
            let payload = MsgObject [ ("name", MsgString upath)
                                    , ("hash", MsgString (decodeUtf8 hash))
                                    , ("mtime", MsgInteger mtime)
                                    , ("content", MsgBinary content) ]
            let bar = if csv then ProgressNone else ProgressSend
            callWebService "put" payload bar

-- | Upload a directory to the cloud. The logic is basically the
-- same as uploading a file, except that both the hash and content
-- are empty.
--
doSendDir :: UFilePath -> FilePath -> EnvIO (Either Error ())
doSendDir upath path = send >>? updateFileInfo upath B.empty
    where
        send :: EnvIO (Either Error MsgValue)
        send = do
            mtime <- liftIO $ getFileModificationTime path
            let payload = MsgObject [ ("name", MsgString upath)
                                    , ("hash", MsgString T.empty)
                                    , ("mtime", MsgInteger mtime) ]
            callWebService "put" payload ProgressNone

-- | Download a file from the cloud.
--
doGetFile :: B.ByteString -> UFilePath -> FilePath -> Bool -> EnvIO (Either Error ())
doGetFile hash upath path csv = receive >>=? return . decode >>=? save
    where
        receive :: EnvIO (Either Error MsgValue)
        receive = do
            let payload = MsgObject [("name", MsgString upath)]
            let bar = if csv then ProgressNone else ProgressReceive
            callWebService "get" payload bar

        decode :: MsgValue -> Either Error (Int, B.ByteString)
        decode msg = case (msg !? "mtime", msg !? "content", msg !? "hash") of
            (Just (MsgInteger t), Just (MsgBinary c), Just (MsgString h))
                 | h == decodeUtf8 hash -> Right (t, c)
                 | otherwise            -> Left (ErrCloudGeneric "inconsistent file hash")
            _                           -> Left (ErrCloudGeneric "invalid payload")

        save :: (Int, B.ByteString) -> EnvIO (Either Error ())
        save (mtime, content) = getMasterKey >>=?
                                liftIO . unarchive content path >>? do
                                    liftIO $ setFileModificationTime path mtime
                                    updateFileInfo upath hash

-- | Download a directory from the cloud. The logic is basically the same
-- as downloading a file, except that we ignore the hash and content
-- returned by the server.
--
doGetDir :: UFilePath -> FilePath -> EnvIO (Either Error ())
doGetDir upath path = receive >>=? return . decode >>=? save
    where
        receive :: EnvIO (Either Error MsgValue)
        receive = do
            let payload = MsgObject [("name", MsgString upath)]
            callWebService "get" payload ProgressNone

        decode :: MsgValue -> Either Error Int
        decode msg = case (msg !? "mtime") of
                        (Just (MsgInteger t)) -> Right t
                        _                     -> Left (ErrCloudGeneric "invalid payload")

        save :: Int -> EnvIO (Either Error ())
        save mtime = handleIOException createDirectory path >>? do
                        liftIO $ setFileModificationTime path mtime
                        updateFileInfo upath B.empty

-- | Delete a local file.
--
doDeleteLocalFile :: UFilePath -> FilePath -> EnvIO (Either Error ())
doDeleteLocalFile upath path = handleIOException removeFile path >>? deleteFileInfo upath

-- | Delete a local directory. Synchronisation actions being sorted, this
-- function can only be called after the directory content is removed.
--
doDeleteLocalDir :: UFilePath -> FilePath -> EnvIO (Either Error ())
doDeleteLocalDir upath path = handleIOException removeDirectory path >>? deleteFileInfo upath

-- | Delete a file on the cloud.
--
doDeleteRemote :: UFilePath -> EnvIO (Either Error ())
doDeleteRemote upath = delete >>? deleteFileInfo upath
    where
        delete :: EnvIO (Either Error MsgValue)
        delete = do
            let payload = MsgObject [("name", MsgString upath)]
            callWebService "delete" payload ProgressNone

-- | Retrieve the master key used to cipher/uncipher archives.
--
getMasterKey :: EnvIO (Either Error B.ByteString)
getMasterKey = do
    k <- getConfig CfgMasterKey
    case k of
        Just key -> return $ Right key
        Nothing  -> return $ Left ErrMissingMasterKey

-- | Process I/O exceptions. Those that do not result in data loss
-- are silently ignored while other exceptions are translated into
-- error messages.
--
handleIOException :: (FilePath -> IO ()) -> FilePath -> EnvIO (Either Error ())
handleIOException action file = liftIO $ catch (action file >> return (Right ())) (return . handler)
    where
        handler :: IOError -> Either Error ()
        handler e
            | isAlreadyExistsError e = Right ()
            | isDoesNotExistError e  = Right ()
            | isAlreadyInUseError e  = Left $ ErrIOException file "already in use"
            | isFullError e          = Left $ ErrIOException file "device is full"
            | isIllegalOperation e   = Left $ ErrIOException file "illegal operation"
            | isPermissionError e    = Left $ ErrIOException file "permission denied"
            | otherwise              = Left $ ErrIOException file "unspecified error"

-- | Operator to chain EnvIO actions returning Either values. (Improvement: use EitherT.)
--
infixl 1 >>=?
(>>=?) :: EnvIO (Either Error a) -> (a -> EnvIO (Either Error b)) -> EnvIO (Either Error b)
(>>=?) f g = f >>= \r -> case r of
    Left e  -> return $ Left e
    Right v -> g v

-- | Operator to chain EnvIO action returning Either values, where
-- the result of the first action is discarded.  (Improvement: use
-- EitherT.)
--
infixl 1 >>?
(>>?) :: EnvIO (Either Error a) -> EnvIO (Either Error b) -> EnvIO (Either Error b)
(>>?) f g = f >>= \r -> case r of
    Left e  -> return $ Left e
    Right _ -> g

-----------------------------------------------------------------------------
