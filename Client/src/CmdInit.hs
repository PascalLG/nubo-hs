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

module CmdInit (
      cmdInit
    , helpInit
) where

import qualified Data.ByteString as B
import qualified Crypto.MAC.HMAC as M
import qualified Crypto.Hash as H
import qualified Data.Text as T
import Network.HostName (getHostName)
import Data.ByteArray (convert)
import Data.Maybe (isNothing, fromJust)
import Control.Monad.Trans (liftIO)
import Data.Text.Encoding (encodeUtf8)
import PrettyPrint
import Misc
import Error
import UUID
import MsgPack
import Database
import WebService

-----------------------------------------------------------------------------

-- | Parse arguments for the 'init' command.
--
cmdInit :: [String] -> IO ExitStatus
cmdInit args = case parseArgs args [OptionForce] of
    Left err                 -> putErr err >> return StatusInvalidCommand
    Right (opts, [])         -> exec opts Nothing Nothing
    Right (opts, [url])      -> exec opts (Just url) Nothing
    Right (opts, [url, pwd]) -> exec opts (Just url) (Just pwd)
    Right (_, (_:_:a:_))     -> putErr (ErrExtraArgument a) >> return StatusInvalidCommand

    where
        exec :: [Option] -> Maybe String -> Maybe String -> IO ExitStatus
        exec options url pass = findNuboDB >>= \previous ->
            if isNothing previous || elem OptionForce options then createDBAndRun (doInit url pass)
                                                              else putErr (ErrAlreadySyncFolder (fromJust previous)) >>
                                                                   return StatusDatabaseAlreadyExists

-- | Execute the 'init' command. Create the local database (if
-- necessary), reset all parameters to their default values, ask
-- the user a cloud URL and a password, and try to authenticate on
-- the specified server.
--
doInit :: Maybe String -> Maybe String -> EnvIO ExitStatus
doInit opturl optpass = do
    hostname <- liftIO $ getHostName
    computer <- getConfig CfgComputerUUID
    token <- case computer of
                Just uuid -> return uuid
                Nothing -> do
                    uuid <- liftIO $ nextRandomUUID
                    saveConfig CfgComputerUUID uuid
                    return uuid

    url <- case opturl of
        Just u  -> return u
        Nothing -> liftIO $ promptUserInput "Cloud URL?" True
    saveConfig CfgRemoteURL url

    pass <- case optpass of
        Just p  -> return p
        Nothing -> liftIO $ promptUserInput "Password?" False

    let json = MsgObject [ ("computer", msgString (show token))
                         , ("hostname", msgString hostname)
                         , ("password", msgString pass)
                         , ("salt", MsgBool True)]
    result <- callWebService "init" json ProgressNone
    
    case result of
        Left err -> liftIO $ putErr err >> return StatusConnectionFailed
        Right resp -> case resp !? "salt" of
            Just (MsgBinary salt) | B.length salt >= 32 -> do
                let utf8pass = encodeUtf8 (T.pack pass)
                let hmac = M.hmac utf8pass salt :: M.HMAC H.SHA256
                saveConfig CfgMasterKey (convert (M.hmacGetDigest hmac) :: B.ByteString)
                saveConfig CfgReady True
                return StatusOK
            _ -> liftIO $ putErr ErrInvalidMasterKey >> return StatusConnectionFailed

-----------------------------------------------------------------------------

-- | Print usage for the init command.
--
helpInit :: IO ()
helpInit =  do
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:nubo init}} [{y:-f}} | {y:--force}}] [{y:url}} [{y:password}}]]"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    Associate the current folder with a cloud and validate your credentials."
    putLine $ "    After this operation succeeds, you can start sharing and synchronising all"
    putLine $ "    the files in this folder and its subfolders."
    putLine $ ""
    putLine $ "    If the current folder is already associated with a cloud, an error occurs."
    putLine $ "    You can bypass this behaviour by specifying the {y:--force}} option. If the"
    putLine $ "    current folder is the cloud root, the previous association is replaced by"
    putLine $ "    the new one. Otherwise, if it is a subfolder of the cloud root, you end up"
    putLine $ "    with a cloud in a cloud, which is probably not something you want to do."
    putLine $ ""
    putLine $ "    To initialise nubo, you must provide the URL of the server where your"
    putLine $ "    personal cloud resides, as well as your user password. If you don’t"
    putLine $ "    specify these elements on the command line, nubo will prompt you to enter"
    putLine $ "    them later."
    putLine $ ""
    putLine $ "    To ensure your data safety and privacy:"
    putLine $ ""
    putLine $ "    • Only access your server over a secured connection, which typically means"
    putLine $ "      providing here a URL that starts with {m:https://}}. Note that this requires"
    putLine $ "      that your server supports TLS and is properly installed with valid"
    putLine $ "      certificates."
    putLine $ ""
    putLine $ "    • Do not provide your password on the command line, since it appears as"
    putLine $ "      clear text on your terminal and can be recalled later by cycling over the"
    putLine $ "      shell history. Instead, only enter your password when nubo prompts you."
    putLine $ ""
    putLine $ "    After you provide all the required information, {y:nubo}} calls the server to"
    putLine $ "    validate your password, generate an authentication token and download the"
    putLine $ "    cloud master encryption key. It then creates a small hidden file in the"
    putLine $ "    current folder to store some configuration information."
    putLine $ ""
    putLine $ "    This command does not initiate any file transfer whatsoever. It only"
    putLine $ "    authenticates your computer on the server and configures your local folder."
    putLine $ "    Use the {y:nubo sync}} command to perform an actual synchronisation."
    putLine $ ""
    putLine $ "{*:OPTIONS}}"
    putLine $ "    {y:-f}}, {y:--force}}    Overwrite previous configuration, if any. By default,"
    putLine $ "                   initialising a nubo cloud in a folder that is already in a"
    putLine $ "                   nubo cloud raises an error."
    putLine $ ""

-----------------------------------------------------------------------------
