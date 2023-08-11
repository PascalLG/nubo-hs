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
import Control.Monad.Trans (liftIO)
import Data.Text.Encoding (encodeUtf8)
import Data.Char (toLower)
import PrettyPrint
import Misc
import Environment
import Error
import UUID
import MsgPack
import Database
import WebService

-----------------------------------------------------------------------------

-- | Parse arguments for the 'init' command.
--
cmdInit :: [String] -> EnvIO ExitStatus
cmdInit args = do
    result <- parseArgsM args [OptionForce, OptionTest, OptionNoTLS]
    case result of
        Left errs                -> mapM_ putErr errs >> return StatusInvalidCommand
        Right (opts, [])         -> exec opts Nothing Nothing
        Right (opts, [url])      -> exec opts (Just url) Nothing
        Right (opts, [url, pwd]) -> exec opts (Just url) (Just pwd)
        Right (_, (_:_:a:_))     -> putErr (ErrExtraArgument a) >> return StatusInvalidCommand

    where
        exec :: [Option] -> Maybe String -> Maybe String -> EnvIO ExitStatus
        exec options url pass = liftIO findNuboDB >>= \previous ->
            case previous of
                Just p | OptionForce `notElem` options -> putErr (ErrAlreadySyncFolder p) >>
                                                          return StatusDatabaseAlreadyExists
                _                                      -> createDBAndRun $ doInit url pass
                                                                                  (OptionTest `elem` options)
                                                                                  (OptionNoTLS `elem` options)

-- | Execute the 'init' command. Create the local database (if
-- necessary), reset all parameters to their default values, ask
-- the user a cloud URL and a password, and try to authenticate on
-- the specified server.
--
doInit :: Maybe String -> Maybe String -> Bool -> Bool -> EnvIO ExitStatus
doInit opturl optpass test notls = do
    setupTlsManager
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

    secure <- let https = map toLower (take 6 url) == "https:"
                  ask = promptUserInput "This server is not secured by TLS/SSL. Proceed anyway? (yes/no) " True
              in if notls || https then return True
                                   else (== "yes") <$> liftIO ask

    if not secure then return StatusAborted else do
        pass <- case optpass of
            Just p  -> return p
            Nothing -> liftIO $ promptUserInput "Password?" False

        let json = MsgObject [ ("computer", msgString (show token))
                             , ("hostname", msgString hostname)
                             , ("password", msgString pass)
                             , ("salt", MsgBool True)]
        result <- callWebService "init" json ProgressNone

        case result of
            Left err -> putErr err >> return StatusConnectionFailed
            Right resp -> case resp !? "salt" of
                Just (MsgBinary salt) | B.length salt >= 32 -> do
                    let utf8pass = encodeUtf8 (T.pack pass)
                    let hmac = M.hmac utf8pass salt :: M.HMAC H.SHA256
                    saveConfig CfgMasterKey (convert (M.hmacGetDigest hmac) :: B.ByteString)
                    saveConfig CfgReady True
                    if test then doSanityChecks
                            else return StatusOK
                _ -> putErr ErrInvalidMasterKey >> return StatusConnectionFailed

-- | Perform basic sanity checks. The goal is to ensure the communication
-- layer works as expected by testing: calling the server, encoding and
-- decoding MsgPack data, sending and receiving authentication tokens.
-- A failure may indicate the server runs an exotic PHP configuration
-- our code is not compatible with.
--
doSanityChecks :: EnvIO ExitStatus
doSanityChecks = do
    liftIO $ putStrF "Running sanity checks... "
    result <- run testCase
    if result then putLine "{g:OK.}}" >>
                   return StatusOK
              else putLine "{r:Failed.}}" >>
                   return StatusInvalidServerResponse
    where
        run :: MsgValue -> EnvIO Bool
        run testcase = do
            result <- callWebService "test" testcase ProgressNone
            case result of
                Left _     -> return False
                Right resp -> case resp !? "test" of
                    Just msg -> return $ msg == testcase
                    _        -> return False

-- | Test data for the basic sanity checks. The goal is not to cover all
-- possible cases (we know this would fail due to PHP limitations) but
-- only cases that are actually used in nubo.
--
testCase :: MsgValue
testCase = MsgObject [ ("null",   MsgNull)
                     , ("bool",   MsgArray testBool)
                     , ("int",    MsgArray testInt)
                     , ("float",  MsgArray testFloat)
                     , ("string", MsgArray testString)
                     , ("binary", MsgArray testBinary)
                     , ("empty",  MsgArray [])
                     , ("big1",   testBigArray)
                     , ("big2",   testBigMap) ]

    where
        testBool    = [ MsgBool False
                      , MsgBool True]

        testInt     = [ MsgInteger (-2147483648)
                      , MsgInteger (-2147483647)
                      , MsgInteger (-1234567890)
                      , MsgInteger (-32769)
                      , MsgInteger (-32768)
                      , MsgInteger (-32767)
                      , MsgInteger (-129)
                      , MsgInteger (-128)
                      , MsgInteger (-127)
                      , MsgInteger (-33)
                      , MsgInteger (-32)
                      , MsgInteger (-31)
                      , MsgInteger (-1)
                      , MsgInteger 0
                      , MsgInteger 1
                      , MsgInteger 127
                      , MsgInteger 128
                      , MsgInteger 129
                      , MsgInteger 255
                      , MsgInteger 256
                      , MsgInteger 257
                      , MsgInteger 32767
                      , MsgInteger 32768
                      , MsgInteger 32769
                      , MsgInteger 65535
                      , MsgInteger 65536
                      , MsgInteger 65537
                      , MsgInteger 1234567890
                      , MsgInteger 2147483647 ]

        testFloat   = [ MsgFloat (-3.1415926535)
                      , MsgFloat (-1.0)
                      , MsgFloat 0.0
                      , MsgFloat 1.0
                      , MsgFloat 3.1415926535 ]

        testString   = [ MsgString T.empty
                       , MsgString (sampleText 1)
                       , MsgString (sampleText 31)
                       , MsgString (sampleText 32)
                       , MsgString (sampleText 33)
                       , MsgString (sampleText 255)
                       , MsgString (sampleText 256)
                       , MsgString (sampleText 257)
                       , MsgString (sampleText 65535)
                       , MsgString (sampleText 65536)
                       , MsgString (sampleText 65537) ]

        testBinary   = [ MsgBinary (sampleBin 1)
                       , MsgBinary (sampleBin 255)
                       , MsgBinary (sampleBin 256)
                       , MsgBinary (sampleBin 257)
                       , MsgBinary (sampleBin 65535)
                       , MsgBinary (sampleBin 65536)
                       , MsgBinary (sampleBin 65537)
                       , MsgBinary (sampleBin 1000000) ]

        testBigArray = MsgArray $ map MsgInteger [1..1000]
        testBigMap   = MsgObject $ map (\x -> ("k" ++ show x, MsgInteger x)) [1..1000]

-- | Generate a sample string of the specified length.
--
sampleText :: Int -> T.Text
sampleText n = T.take n $ T.replicate ((n + 31) `div` 32) (T.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456")

-- | Generate sample binary data of the specified length. (We start
-- at 150 to avoid we accidentally generate a valid UTF-8 string, which
-- would confuse PHP on the server side.)
--
sampleBin :: Int -> B.ByteString
sampleBin n  = fst $ B.unfoldrN n (\x -> Just (x, x + 1)) 150

-----------------------------------------------------------------------------

-- | Print usage for the init command.
--
helpInit :: EnvIO ()
helpInit =  do
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:nubo init}} [{y:options}}] [{y:url}} [{y:password}}]]"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    Associate the current folder with a cloud and validate your credentials."
    putLine $ "    After this operation succeeds, you can start sharing and synchronising"
    putLine $ "    files in this folder and its subfolders."
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
    putLine $ "      certificates. By default, {y:nubo}} asks for confirmation before proceeding if"
    putLine $ "      it detects your server is not secured."
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
    putLine $ "    The first time you connect to a freshly installed cloud, it is good"
    putLine $ "    practice to add the {y:--test}} option to run basic sanity checks after"
    putLine $ "    initialisation. These tests ensure the server works as expected and"
    putLine $ "    interfaces properly with the client. Should they fail, it is advised not"
    putLine $ "    to use {y:nubo}} on this server and to report a bug at {m:https://github.com/}}"
    putLine $ "    {m:PascalLG/nubo-hs}}. This will help in making the application compatible"
    putLine $ "    with exotic PHP configurations."
    putLine $ ""
    putLine $ "    This command does not initiate any file transfer whatsoever. It only"
    putLine $ "    authenticates your computer on the server and configures your local folder."
    putLine $ "    Use the {y:nubo sync}} command to perform an actual synchronisation."
    putLine $ ""
    putLine $ "{*:OPTIONS}}"
    putLine $ "    {y:-f}}, {y:--force}}     Overwrite previous configuration, if any. By default,"
    putLine $ "                    initialising a nubo cloud in a folder that is already in a"
    putLine $ "                    nubo cloud raises an error."
    putLine $ "    {y:-u}}, {y:--test}}      Run sanity checks after the authentication succeeds, to"
    putLine $ "                    ensure the server works as expected and interfaces properly"
    putLine $ "                    with the client."
    putLine $ "    {y:-s}}, {y:--no-tls}}    Do not ask for confirmation if the connection to the"
    putLine $ "                    server is not secured."
    putLine $ "    {y:-a}}, {y:--no-ansi}}   Do not use ANSI escape sequences in output messages."
    putLine $ ""

-----------------------------------------------------------------------------
