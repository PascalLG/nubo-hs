-----------------------------------------------------------------------------
-- Nubo Client Application
-- Copyright (c) 2018, Pascal Levy
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

module Environment (
      Env(..)
    , EnvIO
    , getDbConn
    , setupTlsManager
    , bracketEnvIO
    , Option(..)
    , parseArgsM
    , parseArgs
) where

import System.FilePath (FilePath)
import Control.Monad.State (StateT(..), get, put, liftIO)
import Control.Exception (bracket)
import Data.List (nub)
import qualified Database.HDBC.Sqlite3 as SQL
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import PrettyPrint.Internal

-----------------------------------------------------------------------------
-- Environment.

-- | Application global environment that is passed through a State
--  monad to all functions that require it.
--
data Env = Env { dbConn      :: Maybe SQL.Connection
               , tlsManager  :: Maybe H.Manager
               , rootPath    :: Maybe FilePath
               , consoleMode :: ConsoleMode }

-- | Our monad stack.
--
type EnvIO = StateT Env IO

-- | Get the database connection from the environment. Assume
-- the database has been initialized before.
--
getDbConn :: EnvIO SQL.Connection
getDbConn = do
    env <- get
    let Just db = dbConn env
    return db

-- | Setup a TLS manager in the environment.
--
setupTlsManager :: EnvIO ()
setupTlsManager = do
    env <- get
    tls <- liftIO $ H.newManager H.tlsManagerSettings
    put env { tlsManager = Just tls }

-- | Lifted version of bracket for our monad stack.
--
bracketEnvIO :: IO a -> (a -> IO b) -> (a -> EnvIO c) -> EnvIO c
bracketEnvIO before after thing = do
    oldenv <- get
    (r, newenv) <- liftIO $ bracket before after (\resource -> runStateT (thing resource) oldenv)
    put newenv
    return r

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
    | OptionNoAnsi
    | OptionTest
    deriving (Show, Ord, Eq)

-- | Map between option flags, option letters and option full names.
--
supportedOptions :: [(Option, Char, String)]
supportedOptions = [ ( OptionDry,    'd', "dry"     )
                   , ( OptionForce,  'f', "force"   )
                   , ( OptionDelete, 'd', "delete"  )
                   , ( OptionTheirs, 't', "theirs"  )
                   , ( OptionOurs,   'o', "ours"    )
                   , ( OptionNoAnsi, 'a', "no-ansi" )
                   , ( OptionTest,   'u', "test"    )]

-- | Lookup a short option (from its letter).
--
lookups :: Char -> [(Option, Char, String)] -> Either String Option
lookups opt list = case lookup opt (map (\(o, c, _) -> (c, o)) list) of
    Nothing -> Left ('-':opt:"")
    Just o  -> Right o

-- | Lookup a long option (from its full name).
--
lookupl :: String -> [(Option, Char, String)] -> Either String Option
lookupl opt list = case lookup opt (map (\(o, _, s) -> (s, o)) list) of
    Nothing -> Left ("--" ++ opt)
    Just o  -> Right o

-- | Parse the command line in our monad stack, to handle the --no-ansi
-- flag at this global level.
--
parseArgsM :: [String] -> [Option] -> EnvIO (Either String ([Option], [String]))
parseArgsM cmdline optlist = do
    let result = parseArgs cmdline (OptionNoAnsi : optlist)
    case result of
        Right (opts, _) | OptionNoAnsi `elem` opts -> get >>= \env -> put env { consoleMode = ModeBasic }
        _                                          -> return ()
    return result

-- | Parse a list of arguments. If an unknown or unsupported option is
-- encountered, return an appropriate error message. Otherwise, return
-- the list of recognized options and the list of remaining parameters.
--
parseArgs :: [String] -> [Option] -> Either String ([Option], [String])
parseArgs cmdline optlist = parse cmdline >>= \(opts, params) -> Right (nub opts, params)
    where
        parse :: [String] -> Either String ([Option], [String])
        parse []                   = Right ([], [])
        parse (('-':'-':opt):args) = lookupl opt supported >>= \r -> parse args >>= \(xs, ys) -> Right (r:xs, ys)
        parse (('-':opt):args)     = parse' opt >>= \rs -> parse args >>= \(xs, ys) -> Right (rs ++ xs, ys)
        parse args                 = Right ([], args)

        parse' :: String -> Either String [Option]
        parse' []     = Right []
        parse' (c:cs) = lookups c supported >>= \r -> parse' cs >>= \xs -> Right (r:xs)

        supported :: [(Option, Char, String)]
        supported = filter (\(opt, _, _) -> opt `elem` optlist) supportedOptions

-----------------------------------------------------------------------------
