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

module Archive.Internal (
      aes256encode
    , aes256decode
    , chunkLazy
    , chunkStrict
    , ivInc
) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..))
import Crypto.KDF.Scrypt (generate, Parameters(..))
import Crypto.Error (CryptoFailable(..))
import Data.ByteArray (Bytes, zero, xor, copyAndFreeze, length)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, poke)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
-- import Data.Monoid ((<>))

-----------------------------------------------------------------------------
-- Ciphering. The cryptonite package already provides similar methods, but
-- they can only operate on strict ByteString. For better space efficiency,
-- this implementation is designed to take advantage of lazy IO.

-- | Cipher a chunk of data using a given password and salt with AES256
-- in CTR mode. The input is lazy, the output is strict.
--
aes256encode :: B.ByteString -> B.ByteString -> L.ByteString -> B.ByteString
aes256encode password salt text = mconcat $ combine (zero bsize) $ chunkLazy bsize text
    where
        cipher :: AES256
        cipher = makeCipher password salt

        bsize :: Int
        bsize = blockSize cipher

        combine :: Bytes -> [B.ByteString] -> [B.ByteString]
        combine _  []     = []
        combine iv (b:bs) = let mask = ecbEncrypt cipher iv
                            in  xor b mask : combine (ivInc iv) bs

-- | Uncipher a chunk of data using a given password and salt with AES256
-- in CTR mode. The input is strict, the output is lazy.
--
aes256decode :: B.ByteString -> B.ByteString -> B.ByteString -> L.ByteString
aes256decode password salt text = mconcat $ combine (zero bsize) $ chunkStrict bsize text
    where
        cipher :: AES256
        cipher = makeCipher password salt

        bsize :: Int
        bsize = blockSize cipher

        combine :: Bytes -> [B.ByteString] -> [L.ByteString]
        combine _  []     = []
        combine iv (b:bs) = let mask = ecbEncrypt cipher iv
                            in  (L.fromStrict (xor b mask)) : combine (ivInc iv) bs

-- | Initialise the AES256 cipher. The actual key is derived from the
-- password and salt using the SCrypt algorithm. Note: changing the
-- parameters breaks compatibility.
--
makeCipher :: B.ByteString -> B.ByteString -> AES256
makeCipher password salt = case cipherInit key of
                               CryptoPassed a -> a
                               CryptoFailed e -> error (show e) -- should not happen IRL
    where
        key :: B.ByteString
        key = generate Parameters {n = 16384, r = 8, p = 1, outputLength = 32} password salt

-- | Split a lazy ByteString into strict chunks of the specified size.
--
chunkLazy :: Int -> L.ByteString -> [B.ByteString]
chunkLazy size = split
    where
        split :: L.ByteString -> [B.ByteString]
        split bs = let (b1, b2) = L.splitAt (fromIntegral size) bs
                   in if L.null b2 then [L.toStrict b1]
                                   else L.toStrict b1 : split b2

-- | Split a strict ByteString into strict chunks of the specified size.
--
chunkStrict :: Int -> B.ByteString -> [B.ByteString]
chunkStrict size = split
    where
        split :: B.ByteString -> [B.ByteString]
        split bs = let (b1, b2) = B.splitAt (fromIntegral size) bs
                   in if B.null b2 then [b1]
                                   else b1 : split b2

-- | Increment an initialisation vector by one. This IV is treated
-- as a big endian integer value.
--
ivInc :: Bytes -> Bytes
ivInc bs = copyAndFreeze bs $ loop (Data.ByteArray.length bs - 1)
    where
        loop :: Int -> Ptr Word8 -> IO ()
        loop offset ptr
            | offset < 0 = return ()
            | otherwise  = do
                let addr = ptr `plusPtr` offset
                v <- peek addr :: IO Word8
                if v == 0xFF then poke addr (0 :: Word8) >> loop (offset - 1) ptr
                             else poke addr (v + 1)

-----------------------------------------------------------------------------
