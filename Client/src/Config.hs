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
    , setDatabaseAttributes
    , setCodePageUTF8
    , restoreCodePage
    , getWorkingDirectory
) where
    
#if defined(mingw32_HOST_OS)
import System.Win32.File (getFileAttributes, setFileAttributes, fILE_ATTRIBUTE_HIDDEN)
import System.Win32.Console (getConsoleOutputCP, setConsoleOutputCP)
import System.IO (hSetEncoding, stdout, stderr, utf8)
import Data.Bits ((.|.))
#else
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import Control.Applicative (empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
#if !defined(DEBUG)
import System.Posix.Files (setFileMode, ownerReadMode, ownerWriteMode)
import Data.Bits ((.|.))
#endif
#endif

import System.Directory (getCurrentDirectory)

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

-- | Set suitable attributes for the database file. On UNIX,
-- we chmod 600 to avoid other users can access sensitive
-- information. On Windows, we hide the file. To help debugging, 
-- we do nothing special when in running from GHCi.
--
setDatabaseAttributes :: FilePath -> IO ()
#if defined(DEBUG)
setDatabaseAttributes _ = return ()
#elif defined(mingw32_HOST_OS)
setDatabaseAttributes path = do
    attr <- getFileAttributes path
    setFileAttributes path (attr .|. fILE_ATTRIBUTE_HIDDEN)
#else
setDatabaseAttributes path = setFileMode path (ownerReadMode  .|. ownerWriteMode)
#endif

-- | Change the console code page to UTF-8, configure the
-- standard outputs and return the previous code page. This
-- operation is only needed when running on Windows.
--
setCodePageUTF8 :: IO Int
#if defined(mingw32_HOST_OS)
setCodePageUTF8 = do
    old <- getConsoleOutputCP
    setConsoleOutputCP 65001
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    return (fromIntegral old)
#else
setCodePageUTF8 = return 0
#endif

-- | Restore the console code page. Windows only.
--
restoreCodePage :: Int -> IO ()
#if defined(mingw32_HOST_OS)
restoreCodePage cp = setConsoleOutputCP (fromIntegral cp)
#else
restoreCodePage _ = return ()
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
