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

{-# LANGUAGE BangPatterns #-}

module FileHelper (
      UFilePath
    , FileMap
    , rebuildFilePath
    , getFileHash
    , buildFileList
    , getFileModificationTime
    , setFileModificationTime
    , atomicWrite
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Crypto.Hash as H
import qualified Data.Map.Strict as M
import Data.Text.Normalize
import System.IO
import System.Directory
import System.FilePath ((</>), takeDirectory, pathSeparator, normalise)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Config
import Error

-----------------------------------------------------------------------------
-- Directory listing.

-- | Universal file path. (Unicode, NFD normalised, path separator: /)

type UFilePath = T.Text

-- | Map associating local file path to universal file path.

data FileMap = FileMap { fileMap :: M.Map UFilePath FilePath
                       , rootPath :: FilePath }

-- | Rebuild a local file path from a universal file path. If the path
-- already exists in the map, it is retrieved from here, otherwise it
-- is rebuilt by replacing '/' by the local path separator.
--
rebuildFilePath :: FileMap -> UFilePath -> FilePath
rebuildFilePath m upath = case M.lookup upath (fileMap m) of
                            Just path -> path
                            Nothing   -> rootPath m </> buildPath upath
    where
        buildPath :: UFilePath -> FilePath
        buildPath = T.unpack . T.intercalate (T.singleton pathSeparator) . T.splitOn (T.singleton '/')

-- | Compute the hash of a file. If this file is actually a directory,
-- return an empty string.
--
getFileHash :: FileMap -> (UFilePath, Bool) -> IO (UFilePath, B.ByteString)
getFileHash _ (upath, True) = return (upath, B.empty)
getFileHash m (upath, False) = do
    content <- L.readFile $ rebuildFilePath m upath
    let !hash = show (H.hashlazy content :: H.Digest H.SHA256)
    return (upath, C.pack (length hash `seq` hash))

-- | Build the list of all files in the synchronised directories and partition them
-- in two sets: one for files that cannot be handled because their name is not portable
-- and/or their size is too big, and one for files that will be synchronised. In that
-- latter set, a flag indicating whether that file is a directory is also returned.
--
buildFileList :: FilePath -> IO ([(UFilePath, Error)], [(UFilePath, Bool)], FileMap)
buildFileList root = do
    let rootpath = normalise $ root </> rootFolder
    names <- sortBy cmp <$> getRecursiveContents rootpath T.empty
    let (errlist, filelist) = foldr partition ([], []) names
    let filemap = foldr buildmap M.empty names
    return (errlist, filelist, FileMap { fileMap = filemap, rootPath = rootpath })
    where
        cmp :: (FilePath, UFilePath, Either Error Bool) -> (FilePath, UFilePath, Either Error Bool) -> Ordering
        cmp (_, f1, _) (_, f2, _) = compare (T.toCaseFold f1) (T.toCaseFold f2)

        partition :: (FilePath, UFilePath, Either Error Bool) -> ([(UFilePath, Error)], [(UFilePath, Bool)]) -> ([(UFilePath, Error)], [(UFilePath, Bool)])
        partition (_, path, Left err) ~(l, r) = ((path, err):l, r)
        partition (_, path, Right isdir) ~(l, r) = (l, (path, isdir):r)

        buildmap :: (FilePath, UFilePath, Either Error Bool) -> M.Map UFilePath FilePath -> M.Map UFilePath FilePath
        buildmap (path, upath, _) m = M.insert upath path m

-- | Recursively browse subdirectories, building a list of all encountered
-- files. Universal file names are build on the fly from local file names
-- during recursion. Small system files are silently ignored. Files with
-- not portable names and files that are too big are tagged with an error
-- code.
--
-- Not portable filenames include: names containing characters that are not
-- allowed on all the supported platforms, names being a reserved device
-- name on Windows, and case conflicts (i.e. several files with identical
-- names but different cases).
--
getRecursiveContents :: FilePath -> UFilePath -> IO [(FilePath, UFilePath, Either Error Bool)]
getRecursiveContents topdir utopdir = do
    names <- filter (`notElem` ignoredFiles) <$> getDirectoryContents topdir
    let normalised = map (normalize NFD . T.pack) names
    let casefolded = map T.toCaseFold normalised
    let freq = M.fromListWith (+) [(x, 1) | x <- casefolded]
    concat <$> mapM (partition freq) (zip3 names normalised casefolded)

    where
        partition :: M.Map UFilePath Int -> (FilePath, UFilePath, UFilePath) -> IO [(FilePath, UFilePath, Either Error Bool)]
        partition freq (name, uname, cname)
            | containsInvalidChars name = return [(path, upath, Left ErrInvalidCharInFilename)]
            | isReservedName name       = return [(path, upath, Left ErrReservedFilename)]
            | occurrences > 1           = return [(path, upath, Left ErrCaseConflictInFilename)]
            | otherwise                 = do
                isdir <- doesDirectoryExist path
                if isdir then getRecursiveContents path upath >>= \x -> return $ (path, upath, Right True):x
                         else getFileSize path >>= \s -> if s < 300 * 1048576 then return [(path, upath, Right False)]
                                                                              else return [(path, upath, Left ErrFileIsTooBig)]
            where
                occurrences = fromJust (M.lookup cname freq)
                path = topdir </> name
                upath = if T.null utopdir then uname
                                          else T.concat [utopdir, T.singleton '/', uname]

        containsInvalidChars :: FilePath -> Bool
        containsInvalidChars = any (`elem` "<>:\"/\\|?*")

        isReservedName :: FilePath -> Bool
        isReservedName file = any (== base) reserved
            where
                base = takeWhile (/= '.') file
                reserved = [ "CON", "PRN", "AUX", "NUL"
                           , "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9"
                           , "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9" ]

        ignoredFiles :: [String]
        ignoredFiles = [ "."
                       , ".."
                       , ".DS_Store"
                       , "Icon\\r"
                       , ".cabal-sandbox"
                       , "cabal.sandbox.config"
                       , ".stack-work"
                       , ".git"
                       , "desktop.ini"
                       , "thumbs.db"
                       , nuboDatabase ]

-----------------------------------------------------------------------------
-- Some helper functions.

-- | Return the date/time of the last modification of a file.
--
getFileModificationTime :: FilePath -> IO Int
getFileModificationTime path = do
    time <- getModificationTime path
    return $ truncate (utcTimeToPOSIXSeconds time)

-- | Set the date/time of the last modification of a file.
--
setFileModificationTime :: FilePath -> Int -> IO ()
setFileModificationTime path time = do
    let utc = posixSecondsToUTCTime (fromIntegral time)
    setModificationTime path utc

-- | Write a file atomically. It also creates all directories
-- along the filepath if they are missing.
--
atomicWrite :: FilePath -> L.ByteString -> IO ()
atomicWrite path content = do
    let directory = takeDirectory path
    exist <- doesFileExist path
    (tmppath, hfile) <- if exist then openBinaryTempFile directory template
                                 else createDirectoryIfMissing True directory >>
                                      openBinaryTempFileWithDefaultPermissions directory template

    _ <- L.hPut hfile content
    _ <- hFlush hfile

    if exist then moveContent hfile >> removeFile tmppath
             else hClose hfile >> renameFile tmppath path

    where
        moveContent :: Handle -> IO ()
        moveContent hsrc = do
            hSeek hsrc AbsoluteSeek 0
            hdest <- openBinaryFile path WriteMode
            L.hGetContents hsrc >>= L.hPut hdest
            hClose hdest

        template :: String
        template = "nubo.atomic"

-----------------------------------------------------------------------------
