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

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Control.Monad.State (evalStateT)
import Environment
import Config
import Error
import CmdAuth
import CmdHelp
import CmdIgnore
import CmdInit
import CmdRemote
import CmdSync

-----------------------------------------------------------------------------

-- | Application entry point.
--
main :: IO ()
main = do
    status <- getArgs >>= run
    exitWith $ if status == StatusOK then ExitSuccess
                                     else ExitFailure (fromEnum status)

-- | Do the actual work. This function is also a convenient entry point
-- when debugging in the REPL: it takes a raw list of arguments and it
-- runs in the IO monad.
--
run :: [String] -> IO ExitStatus
run args = do
    (console, previous) <- setupConsoleMode
    status <- evalStateT (dispatch args) Env { dbConn = Nothing
                                             , tlsManager = Nothing
                                             , rootPath = Nothing
                                             , consoleMode = console }
    restoreConsoleMode previous
    return status

-- | Parse the command line and return the action to execute,
-- depending on the arguments.
--
dispatch :: [String] -> EnvIO ExitStatus
dispatch args = case args of
    ("init":xs)          -> cmdInit xs
    ("ignore":xs)        -> cmdIgnore xs
    ("remote":xs)        -> cmdRemote xs
    ("sync":xs)          -> cmdSync xs
    ("auth":xs)          -> cmdAuth xs
    ("help":"init":xs)   -> cmdHelp helpInit xs
    ("help":"ignore":xs) -> cmdHelp helpIgnore xs
    ("help":"remote":xs) -> cmdHelp helpRemote xs
    ("help":"sync":xs)   -> cmdHelp helpSync xs
    ("help":"auth":xs)   -> cmdHelp helpAuth xs
    ("help":xs)          -> cmdHelp printUsage xs -- give a chance to print usage with --ansi option
    _                    -> printUsage >> return StatusInvalidCommand

-----------------------------------------------------------------------------
