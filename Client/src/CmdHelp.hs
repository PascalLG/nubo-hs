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

module CmdHelp (
      cmdHelp
    , printUsage
) where

import Data.Version (showVersion)
import Paths_nubo
import PrettyPrint
import Environment
import Error

-----------------------------------------------------------------------------

-- | Execute the 'help' command.
--
cmdHelp :: EnvIO () -> [String] -> EnvIO ExitStatus
cmdHelp help args = do
    result <- parseArgsM args []
    case result of
        Left errs        -> mapM_ putErr errs >> return StatusInvalidCommand
        Right (_, (a:_)) -> putErr (ErrExtraArgument a) >> return StatusInvalidCommand
        Right (_, [])    -> help >> return StatusOK

-----------------------------------------------------------------------------

-- | Print application usage.
--
printUsage :: EnvIO ()
printUsage = do
    putLine $ "{c:Nubo command line client - Version " ++ showVersion version ++ "}}"
    putLine $ "{c:(c) 2017-2018, Æquans}}"
    putLine $ ""
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:nubo}} <{y:command}}> [{y:options}}] [{y:parameters}}]"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    {y:nubo}} is the command-line client for your {y:nubo}} self-hosted cloud. It allows"
    putLine $ "    synchronisation between your local computer and your cloud. See below for"
    putLine $ "    available commands."
    putLine $ ""
    putLine $ "    For more information about {y:nubo}} and detailed instructions on installing the"
    putLine $ "    server side application, refer to: {_:{m:https://github.com/PascalLG/Nubo}}}}."
    putLine $ ""
    putLine $ "{*:COMMANDS}}"
    putLine $ "    {y:init}}    Initialise the current folder as a nubo synced folder"
    putLine $ "    {y:ignore}}  Add or delete patterns to the list of ignored files"
    putLine $ "    {y:sync}}    Synchronise the local computer with the remote server"
    putLine $ "    {y:auth}}    Re-authenticate the user"
    putLine $ "    {y:remote}}  Print the URL of the remote server"
    putLine $ "    {y:help}}    Print help about a command"
    putLine $ ""
    putLine $ "    For more information about options and parameters a command supports,"
    putLine $ "    type: {y:nubo help}} <{y:command}}>"
    putLine $ ""

-----------------------------------------------------------------------------
