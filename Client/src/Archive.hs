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

module Archive (
    archive,
    unarchive,
) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV)
import Crypto.KDF.Scrypt (generate, Parameters(..))
import Crypto.Error (CryptoFailable(..))
import Control.Exception (try, catches, Handler(..))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Codec.Compression.Zlib as Z
import qualified Codec.Compression.Zlib.Internal as ZI
import Data.Monoid ((<>))
import Misc
import Error
import FileHelper

-----------------------------------------------------------------------------
-- Archive and unarchive.

-- | Archive a file to a compressed and encrypted byte string. Return either
-- an error or the resulting byte string.
--
archive :: FilePath -> B.ByteString -> IO (Either Error B.ByteString)
archive path password = do
    content <- try (L.toStrict
                <$> Z.compressWith Z.defaultCompressParams { Z.compressLevel = Z.bestCompression }
                <$> L.readFile path) :: IO (Either IOError B.ByteString)
    case content of
        Left _  -> return $ Left (ErrIOException path "cannot read")
        Right c -> do
            salt <- randomBytes 32
            return $ Right (magicNumberV1 <> salt <> aes256 password salt c)

-- | Unarchive a compressed and encrypted byte string to a file. Return
-- either an error or the number of bytes written.
--
unarchive :: B.ByteString -> FilePath -> B.ByteString -> IO (Either Error Int)
unarchive ciphertext path password = catches unarchive' [Handler (handleDecompressError path), Handler (handleIOError path)]
    where
        unarchive' :: IO (Either Error Int)
        unarchive' = case decode (B.splitAt 4 ciphertext) of
            Left e  -> return $ Left e
            Right c -> do
                atomicWrite path c
                return $ Right (fromIntegral (L.length c))

        decode :: (B.ByteString, B.ByteString) -> Either Error L.ByteString
        decode (hdr, text) = if      hdr == magicNumberV1 then Right $ unarchiveV1 text password
                             else if hdr == magicNumberV2 then Right $ undefined
                             else                              Left $ ErrUnknownArchiveFormat path

        handleDecompressError :: FilePath -> ZI.DecompressError -> IO (Either Error Int)
        handleDecompressError filepath _ = return $ Left (ErrCorruptedArchive filepath)

        handleIOError :: FilePath -> IOError -> IO (Either Error Int)
        handleIOError filepath _ = return $ Left (ErrIOException filepath "cannot write")

-- | Process a v1 archive.
--
unarchiveV1 :: B.ByteString -> B.ByteString -> L.ByteString
unarchiveV1 cipher password =
    let
        (salt, text) = B.splitAt 32 cipher
        u = aes256 password salt text
    in 
        Z.decompress (L.fromStrict u)

-----------------------------------------------------------------------------
-- Cipher.

-- | Cipher or decipher a chunk of data using a given password and salt. Data
-- are ciphered using AES256 in CTR mode. The actual key is derived from
-- the password and the salt using SCrypt.
--
aes256 :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
aes256 password salt plaintext = ctrCombine ctx nullIV plaintext
    where
        ctx :: AES256
        ctx = case cipherInit key of
            CryptoPassed a -> a
            CryptoFailed e -> error (show e) -- should not happen IRL

        key :: B.ByteString
        key = generate Parameters {n = 16384, r = 8, p = 1, outputLength = 32} password salt

-----------------------------------------------------------------------------
-- Configuration.

-- | Magic number for archives v1.
--
magicNumberV1 :: B.ByteString
magicNumberV1 = B.pack [0x70, 0x6C, 0x79, 0x30]

-- | Magic number for archives v2. (Not used yet.)
--
magicNumberV2 :: B.ByteString
magicNumberV2 = B.pack [0x70, 0x6C, 0x79, 0x31]

-----------------------------------------------------------------------------
