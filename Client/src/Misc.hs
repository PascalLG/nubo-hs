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
      Env
    , EnvIO
    , newEnv
    , newEnvTLS
    , setEnvConnection
    , setEnvPath
    , Option(..)
    , parseArgs
    , promptUserInput
    , randomBytes
    , (<&&>)
    , dropWhileEnd
) where

import System.IO
import System.FilePath (FilePath)
import Crypto.Random (getSystemDRG, randomBytesGenerate)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT)
import Control.Exception (bracket_)
import Data.List (nub)
import qualified Data.ByteString as B
import qualified Database.HDBC.Sqlite3 as SQL
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import Error

-----------------------------------------------------------------------------
-- Environment.

-- | Application global state that is passed through a Reader monad
-- to all functions that require it.
--
type Env = (Maybe SQL.Connection, Maybe H.Manager, Maybe FilePath)
type EnvIO = ReaderT Env IO

-- | Build an empty environment.
--
newEnv :: Env
newEnv = (Nothing, Nothing, Nothing)

-- | Build an environment containing a TLS manager.
--
newEnvTLS :: IO Env
newEnvTLS = H.newManager H.tlsManagerSettings >>= \m -> return (Nothing, Just m, Nothing)

-- | Set the database connection in an existing environment.
--
setEnvConnection :: SQL.Connection -> Env -> Env
setEnvConnection db (_, x, y) = (Just db, x, y)

-- | Set the database path in an existing environment.
--
setEnvPath :: FilePath -> Env -> Env
setEnvPath path (x, y, _) = (x, y, Just path)

-----------------------------------------------------------------------------
-- GNU style command line option handling.

-- | An enumeration of all possible options the application supports.
--
data Option =
      OptionDry
    | OptionForce
    | OptionDelete
    | OptionTheirs
    | OptionOurs
    deriving (Show, Ord, Eq)

-- | Map between option flags, option letters and option full names.
--
supportedOptions :: [(Option, Char, String)]
supportedOptions = [ ( OptionDry,    'd', "dry"    )
                   , ( OptionForce,  'f', "force"  )
                   , ( OptionDelete, 'd', "delete" )
                   , ( OptionTheirs, 't', "theirs" )
                   , ( OptionOurs,   'o', "ours"   )]

-- | Lookup a short option (from its letter).
--
lookups :: Char -> [(Option, Char, String)] -> Either Error Option
lookups key [] = Left $ ErrUnsupportedOption ('-':key:[])
lookups key ((opt, name, _):opts)
    | key == name = Right opt
    | otherwise   = lookups key opts

-- | Lookup a long option (from its full name).
--
lookupl :: String -> [(Option, Char, String)] -> Either Error Option
lookupl key [] = Left $ ErrUnsupportedOption ("--" ++ key)
lookupl key ((opt, _, name):opts)
    | key == name = Right opt
    | otherwise   = lookupl key opts

-- | Parse the command line. If an unknown or unsupported option is
-- encountered, return an appropriate error message. Otherwise, return
-- the list of recognized options and the list of remaining parameters.
--
parseArgs :: [String] -> [Option] -> Either Error ([Option], [String])
parseArgs cmdline optlist = parse cmdline >>= \(opts, params) -> Right (nub opts, params)
    where
        parse :: [String] -> Either Error ([Option], [String])
        parse []                   = Right ([], [])
        parse (('-':'-':opt):args) = lookupl opt supported >>= \r -> parse args >>= \(xs, ys) -> Right (r:xs, ys)
        parse (('-':opt):args)     = parse' opt >>= \rs -> parse args >>= \(xs, ys) -> Right (rs ++ xs, ys)
        parse args                 = Right ([], args)

        parse' :: String -> Either Error [Option]
        parse' []     = Right []
        parse' (c:cs) = lookups c supported >>= \r -> parse' cs >>= \xs -> Right (r:xs)

        supported :: [(Option, Char, String)]
        supported = filter (\(opt, _, _) -> opt `elem` optlist)  supportedOptions

-----------------------------------------------------------------------------
-- Miscellaneous functions.

-- | Display a message and wait for user input. Echo can be disabled,
-- for example when prompting for a password.
--
promptUserInput :: String -> Bool -> IO String
promptUserInput prompt echo = do
    putStr (prompt ++ " ")
    hFlush stdout
    old <- hGetEcho stdin
    input <- bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) getLine
    when (not echo) (putChar '\n')
    return input

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

-----------------------------------------------------------------------------
