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

module Archive (
    archive,
    unarchive,
) where

import Control.Exception (try, catches, Handler(..))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Codec.Compression.Zlib as Z
import qualified Codec.Compression.Zlib.Internal as ZI
import Data.Monoid ((<>))
import Misc
import Error
import FileHelper
import Archive.Internal

-----------------------------------------------------------------------------
-- Archive and unarchive.

-- | Archive a file to a compressed and encrypted byte string. Return either
-- an error or the resulting byte string.
--
archive :: FilePath -> B.ByteString -> IO (Either Error B.ByteString)
archive path password = do
    result <- try process :: IO (Either IOError B.ByteString)
    case result of
        Left _  -> return $ Left (ErrIOException path "cannot read")
        Right c -> return $ Right c

    where
        process :: IO B.ByteString
        process = do
            salt <- randomBytes 32
            plain <- L.readFile path
            let !content = aes256encode password salt
                         $ Z.compressWith Z.defaultCompressParams { Z.compressLevel = Z.bestCompression }
                         $ plain
            return $ content `seq` magicNumberV1 <> salt <> content

-- | Unarchive a compressed and encrypted byte string to a file. Return
-- either an error or the number of bytes written.
--
unarchive :: B.ByteString -> FilePath -> B.ByteString -> IO (Either Error ())
unarchive ciphertext path password = catches process [Handler (handleDecompressError path), Handler (handleIOError path)]
    where
        process :: IO (Either Error ())
        process = case decode (B.splitAt 4 ciphertext) of
            Left e  -> return $ Left e
            Right c -> do
                atomicWrite path c
                return $ Right ()

        decode :: (B.ByteString, B.ByteString) -> Either Error L.ByteString
        decode (hdr, text)
            | hdr == magicNumberV1 = Right $ unarchiveV1 text password
            | otherwise            = Left $ ErrUnknownArchiveFormat path

        handleDecompressError :: FilePath -> ZI.DecompressError -> IO (Either Error ())
        handleDecompressError filepath _ = return $ Left (ErrCorruptedArchive filepath)

        handleIOError :: FilePath -> IOError -> IO (Either Error ())
        handleIOError filepath _ = return $ Left (ErrIOException filepath "cannot write")

-- | Process a v1 archive.
--
unarchiveV1 :: B.ByteString -> B.ByteString -> L.ByteString
unarchiveV1 cipher password = let (salt, text) = B.splitAt 32 cipher
                              in  Z.decompress $ aes256decode password salt text

-----------------------------------------------------------------------------
-- Configuration.

-- | Magic number for archives v1.
--
magicNumberV1 :: B.ByteString
magicNumberV1 = B.pack [0x70, 0x6C, 0x79, 0x30]

-- | Magic number for archives v2. (Not used yet.)
--
-- magicNumberV2 :: B.ByteString
-- magicNumberV2 = B.pack [0x70, 0x6C, 0x79, 0x31]

-----------------------------------------------------------------------------
