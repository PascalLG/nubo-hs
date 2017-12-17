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
    status <- getArgs >>= dispatch
    exitWith $ if status == StatusOK then ExitSuccess
                                     else ExitFailure (fromEnum status)

-- | Parse the command line, start the right command and return
-- its exit status code. 
--
dispatch :: [String] -> IO ExitStatus
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
    _                    -> printUsage >> return StatusInvalidCommand

-----------------------------------------------------------------------------
