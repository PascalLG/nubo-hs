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

module Misc (
      promptUserInput
    , putStrF
    , randomBytes
    , (<&&>)
    , dropWhileEnd
    , toCSV
) where

import System.IO (hFlush, hGetEcho, hSetEcho, stdout, stdin)
import Crypto.Random (getSystemDRG, randomBytesGenerate)
import Control.Monad (when)
import Control.Exception (bracket_)
import Data.List (intercalate)
import qualified Data.ByteString as B

-----------------------------------------------------------------------------
-- Miscellaneous functions.

-- | Display a message and wait for user input. Echo can be disabled,
-- for example when prompting for a password.
--
promptUserInput :: String -> Bool -> IO String
promptUserInput prompt echo = do
    putStrF (prompt ++ " ")
    old <- hGetEcho stdin
    input <- bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) getLine
    when (not echo) (putChar '\n')
    return input

-- | Put a string and immediately flush stdout.
--
putStrF :: String -> IO ()
putStrF s = putStr s >> hFlush stdout

-- | Generate a string of random bytes, using a cryptographically
-- secure pseudo random number generator.
--
randomBytes :: Int -> IO B.ByteString
randomBytes size = do
    drg <- getSystemDRG
    let (bytes, _) = randomBytesGenerate size drg
    return bytes

-- | Logical conjunction lifted in a monad with preservation of
-- the lazyness.
--
infixr 3 <&&>
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) x y = x >>= \r -> if r then y else return False

-- | Drops the largest suffix of a list in which the given predicate
-- holds for all elements. (Copied from base-4.5.0.0)
--
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

-- | Format a value for a CSV file. The resulting string is enclosed
-- in double quotes.
--
toCSV :: [String] -> String
toCSV vs = intercalate ", " (map row vs) ++ "\n"
    where row v = "\"" ++ concatMap (\c -> if c == '"' then [c, c] else [c]) v ++ "\""

-----------------------------------------------------------------------------
