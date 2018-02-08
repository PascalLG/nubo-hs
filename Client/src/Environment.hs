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
import Control.Monad.State (StateT(..), get, put, liftIO, when)
import Control.Exception (bracket)
import Data.List (nub)
import Data.Either (partitionEithers)
import qualified Database.HDBC.Sqlite3 as SQL
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Client.TLS as H
import PrettyPrint.Internal
import Error.Internal

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
    | OptionNoTLS
    | OptionCSV
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
                   , ( OptionNoTLS,  's', "no-tls"  )
                   , ( OptionCSV,    'c', "csv"     )
                   , ( OptionTest,   'u', "test"    )]

-- | Lookup a short option (from its letter).
--
lookups :: [(Option, Char, String)] -> Char -> Either Error Option
lookups list opt = case lookup opt (map (\(o, c, _) -> (c, o)) list) of
    Nothing -> Left $ ErrUnsupportedOption ('-':[opt])
    Just o  -> Right o

-- | Lookup a long option (from its full name).
--
lookupl :: [(Option, Char, String)] -> String -> Either Error Option
lookupl list opt = case lookup opt (map (\(o, _, s) -> (s, o)) list) of
    Nothing -> Left $ ErrUnsupportedOption ("--" ++ opt)
    Just o  -> Right o

-- | Parse the command line in our monad stack, to handle the --no-ansi
-- flag at a global level.
--
parseArgsM :: [String] -> [Option] -> EnvIO (Either [Error] ([Option], [String]))
parseArgsM cmdline optlist = do
    let (errs, opts, args) = parseArgs cmdline (OptionNoAnsi : optlist)
    when (OptionNoAnsi `elem` opts) (get >>= \env -> put env { consoleMode = ModeBasic })
    return $ if null errs then Right (opts, args)
                          else Left errs

-- | Parse a list of arguments and return a list of recognised options,
-- a list of errors (if any), and the list of remaining parameters.
--
parseArgs :: [String] -> [Option] -> ([Error], [Option], [String])
parseArgs cmdline optlist = let (errs, opts, params) = parse cmdline
                             in  (nub errs, nub opts, params)
    where
        parse :: [String] -> ([Error], [Option], [String])
        parse (('-':'-':opt):args) = let (errs, opts, params) = parse args
                                     in case lookupl supported opt of
                                            Left err'  -> (err':errs, opts, params)
                                            Right opt' -> (errs, opt':opts, params)
        parse (('-':opt):args)     = let (errs, opts, params) = parse args
                                         (errs', opts') = partitionEithers $ map (lookups supported) opt
                                     in  (errs' ++ errs, opts' ++ opts, params)
        parse args                 = ([], [], args)

        supported :: [(Option, Char, String)]
        supported = filter (\(opt, _, _) -> opt `elem` optlist) supportedOptions

-----------------------------------------------------------------------------
