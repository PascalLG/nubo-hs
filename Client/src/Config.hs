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

{-# LANGUAGE CPP #-}

module Config (
      nuboDatabase
    , rootFolder
    , hasAnsiSupport
    , getWorkingDirectory
) where
    
import System.Environment (lookupEnv)
import System.Directory (getCurrentDirectory, doesDirectoryExist)
import Control.Applicative (empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe

-----------------------------------------------------------------------------
-- Configuration parameters that depend on the platform and the mode (debug
-- or release).

-- | Name of the database.
--
nuboDatabase :: String
#if defined(DEBUG)
nuboDatabase = "nubo_debug.rdb"
#else
nuboDatabase = ".nubo.rdb"
#endif

-- | Switch the synchronised folder to a sandbox when debugging to avoid
-- accidentaly corrupting local or remote data.
--
rootFolder :: FilePath
#if defined(DEBUG)
rootFolder = "../Sandbox"
#else
rootFolder = "."
#endif

-- | Supports for ANSI escape codes.
--
hasAnsiSupport :: Bool
#if defined(mingw32_HOST_OS)
hasAnsiSupport = False
#else
hasAnsiSupport = True
#endif

-----------------------------------------------------------------------------
-- Helper functions whose behaviour depends on the platform.

-- | Retrieve the current working directory. On Unix platforms, we prefer
-- reading the environment variable $PWD rather than calling getcwd(), because
-- the latter resolves symlinks. This causes Database.findNuboDB to fail
-- when nubo is called from a symlink'd directory.
--
getWorkingDirectory :: IO FilePath
#if defined(mingw32_HOST_OS)
getWorkingDirectory = getCurrentDirectory
#else
getWorkingDirectory = do
    r <- runMaybeT $ MaybeT (lookupEnv "PWD") >>= check
    case r of 
        Just pwd -> return pwd
        Nothing  -> getCurrentDirectory  -- fallback in case $PWD is missing

    where
        check :: String -> MaybeT IO String
        check path = do
            ok <- lift $ doesDirectoryExist path
            if ok then return path else empty
#endif

-----------------------------------------------------------------------------
