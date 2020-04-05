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

module PrettyPrint (
      ConsoleMode(..)
    , AnsiColor(..)
    , foreColor
    , showCursor
    , eraseEOL
    , putLine
) where

import Control.Monad.State (get)
import Control.Monad.Trans (liftIO)
import Environment
import PrettyPrint.Internal

-----------------------------------------------------------------------------

-- | Surround text with the required ANSI escape sequences to print
-- it in the specified colour. (This is not reentrant, due to the
-- way ANSI escape sequences work.)
--
foreColor :: ConsoleMode -> AnsiColor -> String -> String
foreColor ModeBasic _     text = text
foreColor _         color text = (ansiCodeForColor color) ++ text ++ "\ESC[39m"

-- | Return the ANSI escape sequence that shows/hides the cursor
-- on the terminal.
--
showCursor :: ConsoleMode -> Bool -> String
showCursor ModeBasic _ = ""
showCursor _         b = if b then "\ESC[?25h" else "\ESC[?25l"

-- | Return the ANSI escape sequence that erases the terminal to
-- the end of the current line.
--
eraseEOL :: ConsoleMode -> String
eraseEOL ModeBasic = ""
eraseEOL _         = "\ESC[K"

-- | Print a string containing formatting tags. See @processTags@ in the
-- PrettyPrint.Internal module for more information.
--
putLine :: String -> EnvIO ()
putLine text = do
    cm <- consoleMode <$> get
    liftIO $ putStrLn $ processTags cm [State { isBold = False
                                              , isUnderline = False
                                              , textColor = AnsiDefault }] text

-----------------------------------------------------------------------------
