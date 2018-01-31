-----------------------------------------------------------------------------
-- Nubo Client Application - Unit Testing
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

module ArchiveSpec (
    spec
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteArray as A
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV)
import Crypto.KDF.Scrypt (generate, Parameters(..))
import Crypto.Error (CryptoFailable(..))
import System.Directory (removeFile)
import Data.Word (Word8)
import Data.Bits ((.&.), shiftR)
import Test.Hspec
import Archive
import Archive.Internal

-----------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "AES256" $ do
        it "increments initialisation vector" $ do
            ivInc (A.pack [0x00, 0x00, 0x00, 0x00]) `shouldBe` (A.pack [0x00, 0x00, 0x00, 0x01])
            ivInc (A.pack [0x00, 0x00, 0x00, 0x01]) `shouldBe` (A.pack [0x00, 0x00, 0x00, 0x02])
            ivInc (A.pack [0x00, 0x00, 0x00, 0x02]) `shouldBe` (A.pack [0x00, 0x00, 0x00, 0x03])
            ivInc (A.pack [0x01, 0x02, 0x03, 0x04]) `shouldBe` (A.pack [0x01, 0x02, 0x03, 0x05])
            ivInc (A.pack [0x00, 0x00, 0x00, 0xFF]) `shouldBe` (A.pack [0x00, 0x00, 0x01, 0x00])
            ivInc (A.pack [0x00, 0x00, 0xFF, 0xFF]) `shouldBe` (A.pack [0x00, 0x01, 0x00, 0x00])
            ivInc (A.pack [0x00, 0xFF, 0xFF, 0xFF]) `shouldBe` (A.pack [0x01, 0x00, 0x00, 0x00])
            ivInc (A.pack [0xFF, 0xFF, 0xFF, 0xFF]) `shouldBe` (A.pack [0x00, 0x00, 0x00, 0x00])

        it "splits lazy streams in blocks" $ do
            chunkLazy' 4 []       `shouldBe` [[]]
            chunkLazy' 4 [1]      `shouldBe` [[1]]
            chunkLazy' 4 [1..2]   `shouldBe` [[1..2]]
            chunkLazy' 4 [1..3]   `shouldBe` [[1..3]]
            chunkLazy' 4 [1..4]   `shouldBe` [[1..4]]
            chunkLazy' 4 [1..5]   `shouldBe` [[1..4], [5]]
            chunkLazy' 4 [1..8]   `shouldBe` [[1..4], [5..8]]
            chunkLazy' 5 [1..20]  `shouldBe` [[1..5], [6..10], [11..15], [16..20]]
            chunkLazy' 5 [1..21]  `shouldBe` [[1..5], [6..10], [11..15], [16..20], [21]]
            chunkLazy' 5 [1..24]  `shouldBe` [[1..5], [6..10], [11..15], [16..20], [21..24]]
            chunkLazy' 5 [1..25]  `shouldBe` [[1..5], [6..10], [11..15], [16..20], [21..25]]
            chunkLazy' 16 [1..99] `shouldBe` [[1..16], [17..32], [33..48], [49..64], [65..80], [81..96], [97..99]]

        it "splits strict streams in blocks" $ do
            chunkStrict' 4 []       `shouldBe` [[]]
            chunkStrict' 4 [1]      `shouldBe` [[1]]
            chunkStrict' 4 [1..2]   `shouldBe` [[1..2]]
            chunkStrict' 4 [1..3]   `shouldBe` [[1..3]]
            chunkStrict' 4 [1..4]   `shouldBe` [[1..4]]
            chunkStrict' 4 [1..5]   `shouldBe` [[1..4], [5]]
            chunkStrict' 4 [1..8]   `shouldBe` [[1..4], [5..8]]
            chunkStrict' 5 [1..20]  `shouldBe` [[1..5], [6..10], [11..15], [16..20]]
            chunkStrict' 5 [1..21]  `shouldBe` [[1..5], [6..10], [11..15], [16..20], [21]]
            chunkStrict' 5 [1..24]  `shouldBe` [[1..5], [6..10], [11..15], [16..20], [21..24]]
            chunkStrict' 5 [1..25]  `shouldBe` [[1..5], [6..10], [11..15], [16..20], [21..25]]
            chunkStrict' 16 [1..99] `shouldBe` [[1..16], [17..32], [33..48], [49..64], [65..80], [81..96], [97..99]]

        it "encodes" $ do
            prng 0    100 `shouldSatisfy` testEncode
            prng 1    101 `shouldSatisfy` testEncode
            prng 15   102 `shouldSatisfy` testEncode
            prng 16   103 `shouldSatisfy` testEncode
            prng 100  104 `shouldSatisfy` testEncode
            prng 1000 105 `shouldSatisfy` testEncode

        it "decodes" $ do
            prng 0    100 `shouldSatisfy` testDecode
            prng 1    101 `shouldSatisfy` testDecode
            prng 15   102 `shouldSatisfy` testDecode
            prng 16   103 `shouldSatisfy` testDecode
            prng 100  104 `shouldSatisfy` testDecode
            prng 1000 105 `shouldSatisfy` testDecode

    describe "Archives and unarchives" $ do
        it "empty file" $ do
            arch []                            `shouldReturn` True

        it "high entropy files" $ do
            arch (prng 1000 200)               `shouldReturn` True
            arch (prng 2000 201)               `shouldReturn` True
            arch (prng 3000 202)               `shouldReturn` True
            arch (prng 5000 203)               `shouldReturn` True
            arch (prng 1000000 204)            `shouldReturn` True
            arch (prng 5000000 205)            `shouldReturn` True

        it "low entropy files" $ do
            arch (take 10000 (cycle [1..10]))  `shouldReturn` True
            arch (take 20000 (cycle [65..90])) `shouldReturn` True

-----------------------------------------------------------------------------

chunkLazy' :: Int -> [Word8] -> [[Word8]]
chunkLazy' size bs = map B.unpack (chunkLazy size (L.pack bs))

chunkStrict' :: Int -> [Word8] -> [[Word8]]
chunkStrict' size bs = map B.unpack (chunkStrict size (B.pack bs))

testEncode :: [Word8] -> Bool
testEncode bs = aes256encode password salt (L.pack bs) == refAES password salt (B.pack bs)
    where
        password = B.pack [1..32]
        salt = B.pack [223..255]

testDecode :: [Word8] -> Bool
testDecode bs = L.toStrict (aes256decode password salt (B.pack bs)) == refAES password salt (B.pack bs)
    where
        password = B.pack [1..32]
        salt = B.pack [223..255]

refAES :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
refAES password salt plaintext = ctrCombine ctx nullIV plaintext
    where
        ctx :: AES256
        ctx = case cipherInit key of
            CryptoPassed a -> a
            CryptoFailed e -> error (show e)

        key :: B.ByteString
        key = generate Parameters {n = 16384, r = 8, p = 1, outputLength = 32} password salt

arch :: [Word8] -> IO Bool
arch content = do
    let password = B.pack [1..32]
    let content1 = B.pack content
    B.writeFile "tmp.dat" content1
    arch_result <- archive "tmp.dat" password
    r <- case arch_result of
            Left _     -> return False
            Right arch -> do
--                putStrLn ("data = " ++ show (length content) ++ ", archive = " ++ show (B.length arch))
                unarch_result <- unarchive arch "tmp.dat" password
                case unarch_result of
                    Left _  -> return False
                    Right _ -> do
                        content2 <- B.readFile "tmp.dat"
                        return $ content1 == content2
    removeFile "tmp.dat"
    return r

prng :: Int -> Int -> [Word8]
prng n seed = take n $ suite seed
    where
        suite :: Int -> [Word8]
        suite seed = let next = (seed * 1664525 + 1013904223) .&. 0xFFFFFFFF
                     in fromIntegral (next `shiftR` 15) : suite next

-----------------------------------------------------------------------------
