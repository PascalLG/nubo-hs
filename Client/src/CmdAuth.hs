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

module CmdAuth (
      cmdAuth
    , helpAuth
) where

import Data.Maybe (fromJust)
import Control.Monad.Trans (liftIO)
import Network.HostName (getHostName)
import Environment
import PrettyPrint
import Misc
import Error
import UUID
import MsgPack
import Database
import WebService

-----------------------------------------------------------------------------

-- | Parse arguments for the 'auth' command.
--
cmdAuth :: [String] -> EnvIO ExitStatus
cmdAuth args = do
    result <- parseArgsM args []
    case result of
        Left err            -> putErr (ErrUnsupportedOption err) >> return StatusInvalidCommand
        Right (_, [])       -> exec Nothing
        Right (_, [pwd])    -> exec (Just pwd)
        Right (_, (_:a:_))  -> putErr (ErrExtraArgument a) >> return StatusInvalidCommand

    where
        exec :: Maybe String -> EnvIO ExitStatus
        exec pwd = openDBAndRun $ doAuth pwd

-- | Execute the 'auth' command. This is basically the same operation
-- as 'init', except that we get the cloud URL from the database instead
-- of prompting the user, and we don't need to store the master key.
--
doAuth :: Maybe String -> EnvIO ExitStatus
doAuth optpass = do
    setupTlsManager
    deleteConfig CfgAuthToken

    hostname <- liftIO $ getHostName
    token <- getConfig CfgComputerUUID :: EnvIO (Maybe UUID)
    pass <- case optpass of
        Just p  -> return p
        Nothing -> liftIO $ promptUserInput "Password?" False

    let json = MsgObject [ ("computer", msgString (show (fromJust token)))
                         , ("hostname", msgString hostname)
                         , ("password", msgString pass)
                         , ("salt", MsgBool False)]
    result <- callWebService "init" json ProgressNone

    case result of
        Left err -> putErr err >> return StatusConnectionFailed
        Right _  -> return StatusOK

-----------------------------------------------------------------------------

-- | Print usage for the 'auth' command.
--
helpAuth :: EnvIO ()
helpAuth = do
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:nubo auth}} [{y:password}}]"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    Validate your credential with the cloud server. Authentication is usually"
    putLine $ "    performed once with the {y:init}} command. {y:Nubo}} then uses a temporary token that"
    putLine $ "    changes after each transaction to ensure subsequent authentications."
    putLine $ "    However, that token may become invalid, either because it expires or because"
    putLine $ "    of an unexpected problem. In such a case, you can authenticate again with"
    putLine $ "    this command."
    putLine $ ""
    putLine $ "    To ensure your data safety and privacy, it is best not to provide your"
    putLine $ "    password on the command line, since it appears as clear text on your"
    putLine $ "    terminal and can be recalled later by cycling over the shell history."
    putLine $ "    Instead, only enter your password when {y:nubo}} prompts you."
    putLine $ ""
    putLine $ "{*:OPTIONS}}"
    putLine $ "    None."
    putLine $ ""

-----------------------------------------------------------------------------
