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
    , setDatabaseAttributes
    , getWorkingDirectory
    , setupConsoleMode
    , restoreConsoleMode
) where
    
#if defined(mingw32_HOST_OS)
import System.Win32.Console (getConsoleOutputCP, setConsoleOutputCP)
import System.Win32.Types (BOOL, DWORD, HANDLE)
import System.IO (hIsTerminalDevice, hSetEncoding, hFlush, stdout, stderr, utf8)
import Foreign.Ptr (Ptr)
import Foreign.Marshal (alloca)
import Foreign.Storable (Storable(..))
import Control.Monad (when)
import Data.Bits ((.|.))
#if !defined(DEBUG)
import System.Win32.File (getFileAttributes, setFileAttributes, fILE_ATTRIBUTE_HIDDEN)
#endif
#else
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import Control.Applicative (empty)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import System.IO (hIsTerminalDevice, stdout)
#if !defined(DEBUG)
import System.Posix.Files (setFileMode, ownerReadMode, ownerWriteMode)
import Data.Bits ((.|.))
#endif
#endif

import System.Directory (getCurrentDirectory)
import PrettyPrint.Internal

-----------------------------------------------------------------------------
-- Configuration parameters that depend on the platform and the mode (debug
-- or release).

-- | Name of the database. Hidden in production, but regular file
-- in REPL mode to help debugging.
--
nuboDatabase :: String
#if defined(DEBUG)
nuboDatabase = "nubo_debug.rdb"
#else
nuboDatabase = ".nubo.rdb"
#endif

-- | Synchronised folder. We switch to a sandbox in REPL mode to
-- avoid accidentaly corrupting local or remote data.
--
rootFolder :: FilePath
#if defined(DEBUG)
rootFolder = "../Sandbox"
#else
rootFolder = "."
#endif

-----------------------------------------------------------------------------
-- Helper functions whose behaviour depends on the platform.

-- | Set suitable attributes for the database file. On UNIX, we chmod 600
-- to avoid other users can access sensitive information. On Windows, we 
-- hide the file. We do nothing special when running in REPL mode though,
-- to help debugging.
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

-- | Retrieve the current working directory. On Unix platforms, we prefer
-- reading the environment variable $PWD rather than calling getcwd(), because
-- the latter resolves symlinks, which may cause Database.findNuboDB to fail.
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

-- | Setup the console and try to guess whether it supports ANSI escape
-- sequences or not. On Windows, this consists in changing the code page
-- to UTF8 and enabling virtual processing. On UNIX, this consists in 
-- checking the terminal name. On both, we disable ANSI sequences if the
-- output is not a tty.
--
#if defined(mingw32_HOST_OS)
setupConsoleMode :: IO (ConsoleMode, (Int, Int))
setupConsoleMode = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    istty <- hIsTerminalDevice stdout
    if istty then do
                oldcp <- getConsoleOutputCP
                setConsoleOutputCP 65001 -- code page for UTF-8
                hout <- cGetStdHandle 0xFFFFFFF5 -- STD_OUTPUT_HANDLE
                alloca $ \ptr -> do
                    ok <- cGetConsoleMode hout ptr
                    if ok then do
                            oldmode <- peek ptr
                            ok' <- cSetConsoleMode hout (oldmode .|. 4) -- ENABLE_VIRTUAL_TERMINAL_PROCESSING
                            return (if ok' then ModeWin32 else ModeBasic, (fromIntegral oldcp, fromIntegral oldmode))
                          else
                            return (ModeBasic, (fromIntegral oldcp, 0))
             else
                return (ModeBasic, (0, 0))
#else
setupConsoleMode :: IO (ConsoleMode, ())
setupConsoleMode = do
    istty <- hIsTerminalDevice stdout
    isdumb <- maybe False (== "dumb") <$> lookupEnv "TERM"
    return (if istty && not isdumb then ModeXTerm else ModeBasic, ())
#endif

-- | Restore the console to its initial state. On Windows, it consists
-- in restoring the code page and disabling virtual processing. On UNIX
-- there is nothing to to.
--
#if defined(mingw32_HOST_OS)
restoreConsoleMode :: (Int, Int) -> IO ()
restoreConsoleMode (cp, mode) = do
    hFlush stdout -- avoid UTF-8 or ANSI sequences are 
    hFlush stderr -- emitted *after* we restore CP and mode
    when (cp /= 0) $ do
        setConsoleOutputCP (fromIntegral cp)
    when (mode /= 0) $ do
        hout <- cGetStdHandle 0xFFFFFFF5
        _ <- cSetConsoleMode hout (fromIntegral mode)
        return ()
#else
restoreConsoleMode :: () -> IO ()
restoreConsoleMode _ = return ()
#endif

-----------------------------------------------------------------------------
-- Foreign interface to Win32 SDK. (These functions are missing in the
-- System.Win32 package.)

#if defined(mingw32_HOST_OS)

#if defined(i386_HOST_ARCH)
#define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
#define WINDOWS_CCONV ccall
#else
#error unknown architecture
#endif

foreign import WINDOWS_CCONV unsafe "GetConsoleMode"
    cGetConsoleMode :: HANDLE -> Ptr DWORD -> IO BOOL

foreign import WINDOWS_CCONV unsafe "SetConsoleMode"
    cSetConsoleMode :: HANDLE -> DWORD -> IO BOOL

foreign import WINDOWS_CCONV unsafe "GetStdHandle"
    cGetStdHandle :: DWORD -> IO HANDLE

#endif

-----------------------------------------------------------------------------
