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

module CmdRemote (
      cmdRemote
    , helpRemote
) where

import Control.Monad.Trans (liftIO)
import PrettyPrint
import Environment
import Error
import Database

-----------------------------------------------------------------------------

-- | Parse arguments for the 'remote' command.
--
cmdRemote :: [String] -> EnvIO ExitStatus
cmdRemote args = do
    result <- parseArgsM args []
    case result of
        Left errs        -> mapM_ putErr errs >> return StatusInvalidCommand
        Right (_, (a:_)) -> putErr (ErrExtraArgument a) >> return StatusInvalidCommand
        Right (_, [])    -> openDBAndRun doRemote

-- | Execute the 'remote' command.
--
doRemote :: EnvIO ExitStatus
doRemote = do
    Just url <- getConfig CfgRemoteURL
    liftIO $ putStrLn url
    return StatusOK

-----------------------------------------------------------------------------

-- | Print usage for the remote command.
--
helpRemote :: EnvIO ()
helpRemote = do
    putLine $ "{*:USAGE}}"
    putLine $ "    {y:nubo remote}}"
    putLine $ ""
    putLine $ "{*:DESCRIPTION}}"
    putLine $ "    Print the URL of the cloud the current folder synchronises with. An error"
    putLine $ "    occurs if neither the current folder nor one of its parent is associated"
    putLine $ "    with a {y:nubo}} cloud."
    putLine $ ""
    putLine $ "{*:OPTIONS}}"
    putLine $ "    {y:-a}}, {y:--no-ansi}}   Do not use ANSI escape sequences in output messages."
    putLine $ ""

-----------------------------------------------------------------------------
