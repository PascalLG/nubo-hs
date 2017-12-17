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
      AnsiColor(..)
    , foreColor
    , showCursor
    , eraseEOL
    , putLine
) where

import Data.Maybe (isJust, fromJust)
import Data.List (uncons)
import Config

-----------------------------------------------------------------------------

-- | Supported colours.
--
data AnsiColor = AnsiBlack
               | AnsiRed
               | AnsiGreen
               | AnsiYellow
               | AnsiBlue
               | AnsiMagenta
               | AnsiCyan
               | AnsiWhite
               deriving(Eq, Enum)

-- | Associate each colour with a letter. (Except for black
-- since we probably won't print anything in that colour.)
--
colors :: [(Char, AnsiColor)]
colors = [ ('c', AnsiCyan)
         , ('m', AnsiMagenta)
         , ('y', AnsiYellow)
         , ('r', AnsiRed)
         , ('g', AnsiGreen)
         , ('b', AnsiBlue)
         , ('w', AnsiWhite)
         ]

-- | Surround text with the required ANSI escape sequences to print
-- it in the specified colour. (This is not reentrant, due to the
-- way ANSI escape sequences work.)
--
foreColor :: AnsiColor -> String -> String
foreColor color text
    | hasAnsiSupport = (ansiCodeForColor color) ++ text ++ "\ESC[39m"
    | otherwise      = text

-- | Return the ANSI escape sequence for the specified foreground
-- colour.
--
ansiCodeForColor :: AnsiColor -> String
ansiCodeForColor color = "\ESC[" ++ (show (30 + fromEnum color)) ++ "m"

-- | Return the ANSI escape sequence that shows/hides the cursor
-- on the terminal.
--
showCursor :: Bool -> String
showCursor b
    | hasAnsiSupport = if b then "\ESC[?25h" else "\ESC[?25l"
    | otherwise      = ""

-- | Return the ANSI escape sequence that erases the terminal to
-- the end of the current line.
--
eraseEOL :: String
eraseEOL
    | hasAnsiSupport = "\ESC[K"
    | otherwise      = ""

-----------------------------------------------------------------------------

-- | Text attributes.
--
data State = State { isBold :: !Bool
                   , isUnderline :: !Bool
                   , textColor :: !AnsiColor
                   }

-- | Print a string containing formatting tags. See @processTags@ below
-- for more information about supported tags.
--
putLine :: String -> IO ()
putLine = putStrLn . processTags [State { isBold = False, isUnderline = False, textColor = AnsiWhite }]

-- | Process formatting tags and generate a string containing ANSI escape
-- sequences. Tags follow the pattern {x:text}} where "x" is a character
-- indicating a text attribute (colour, bold or underline) and "text" is 
-- the string that attribute applies to.
--
processTags :: [State] -> String -> String
processTags _ "" = ""

processTags stack ('{':'*':':':cs) = emitEscapeCode old new ++ processTags (new:stack) cs
    where old = head stack
          new = old { isBold = True }

processTags stack ('{':'_':':':cs) = emitEscapeCode old new ++ processTags (new:stack) cs
    where old = head stack
          new = old { isUnderline = True }

processTags stack ('{':t:':':cs) | isJust color = emitEscapeCode old new ++ processTags (new:stack) cs
    where  color = lookup t colors
           old = head stack
           new = old { textColor = fromJust color }

processTags stack ('}':'}':cs) | length stack > 1 = emitEscapeCode old new ++ processTags rest cs
    where Just (old, rest) = uncons stack
          new = head rest

processTags stack (c:cs) = c : processTags stack cs

-- | Emit ANSI escape code corresponding to a transition between
-- two states of text attributes.
--
emitEscapeCode :: State -> State -> String
emitEscapeCode (State b1 u1 c1) (State b2 u2 c2) 
    | hasAnsiSupport = emitBold b1 b2 ++ emitUnderline u1 u2 ++ emitColor c1 c2
    | otherwise      = ""
    where
        emitBold False True  = "\ESC[1m"
        emitBold True  False = "\ESC[22m"
        emitBold _     _     = ""
        
        emitUnderline False True  = "\ESC[4m"
        emitUnderline True  False = "\ESC[24m"
        emitUnderline _     _     = ""
        
        emitColor co1 co2
            | co1 /= co2 = ansiCodeForColor co2
            | otherwise  = ""

-----------------------------------------------------------------------------
