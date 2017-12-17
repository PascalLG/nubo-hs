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

module MsgPackSpec (
    spec
) where

import Test.Hspec
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Word (Word8)
import Control.Monad
import System.Random
import MsgPack
import Error

-----------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "encoding/decoding" $ do
        it "null" $ do
            (MsgNull, [ 0xC0 ]) `shouldSatisfy` encdec
        it "bool" $ do
            (MsgBool False, [ 0xC2 ]) `shouldSatisfy` encdec
            (MsgBool True,  [ 0xC3 ]) `shouldSatisfy` encdec
        it "integer" $ do
            (MsgInteger 1, [ 0x01 ]) `shouldSatisfy` encdec
            (MsgInteger 42, [ 0x2A ]) `shouldSatisfy` encdec
            (MsgInteger 255, [ 0xCC, 0xFF ]) `shouldSatisfy` encdec
            (MsgInteger 65535, [ 0xCD, 0xFF, 0xFF ]) `shouldSatisfy` encdec
            (MsgInteger 4294967295, [ 0xCE, 0xFF, 0xFF, 0xFF, 0xFF ]) `shouldSatisfy` encdec
            (MsgInteger 9223372036854775807, [ 0xCF, 0x7F, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF ]) `shouldSatisfy` encdec
            (MsgInteger (-1), [ 0xFF ]) `shouldSatisfy` encdec
            (MsgInteger (-32), [ 0xE0 ]) `shouldSatisfy` encdec
            (MsgInteger (-127), [ 0xD0, 0x81 ]) `shouldSatisfy` encdec
            (MsgInteger (-32767), [ 0xD1, 0x80, 0x01 ]) `shouldSatisfy` encdec
            (MsgInteger (-65536), [ 0xD2, 0xFF, 0xFF, 0x00, 0x00 ]) `shouldSatisfy` encdec
            (MsgInteger (-4294967296), [ 0xD3, 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00 ]) `shouldSatisfy` encdec
            replicateM_ 100000 $ do
                v <- MsgInteger <$> getStdRandom (randomR (-2147483649, 4294967297))
                (decodeMsgPack . encodeMsgPack) v `shouldBe` Right v
        it "float" $ do
            (MsgFloat 3.14, [ 0xCB, 0x40, 0x09, 0x1E, 0xB8, 0x51, 0xEB, 0x85, 0x1F ]) `shouldSatisfy` encdec
            replicateM_ 100 $ do
                v <- MsgFloat <$> getStdRandom random
                (decodeMsgPack . encodeMsgPack) v `shouldBe` Right v
        it "string" $ do
            (MsgString (T.empty), [0xA0]) `shouldSatisfy` encdec
            (MsgString (T.singleton 'A'), [0xA1, 0x41]) `shouldSatisfy` encdec
            (MsgString (T.pack "Hello, world!"), [0xAD, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x2C, 0x20, 0x77, 0x6F, 0x72, 0x6C, 0x64, 0x21]) `shouldSatisfy` encdec
            (MsgString (T.replicate 32 (T.singleton 'B')), [0xD9, 0x20] ++ replicate 32 0x42) `shouldSatisfy` encdec
            (MsgString (T.replicate 256 (T.singleton 'C')), [0xDA, 0x01, 0x00] ++ replicate 256 0x43) `shouldSatisfy` encdec
            (MsgString (T.replicate 65537 (T.singleton 'D')), [0xDB, 0x00, 0x01, 0x00, 0x01] ++ replicate 65537 0x44) `shouldSatisfy` encdec
        it "invalid streams" $ do
            [ 0xC2, 0x10 ] `shouldSatisfy` cantdec
            [ 0xC2, 0x10, 0x11 ] `shouldSatisfy` cantdec
            [ 0xCB ] `shouldSatisfy` cantdec
            [ 0xCB, 0x40, 0x09 ] `shouldSatisfy` cantdec
            [ 0xCB, 0x40, 0x09, 0x1E, 0xB8, 0x51, 0xEB, 0x85 ] `shouldSatisfy` cantdec

encdec :: (MsgValue, [Word8]) -> Bool
encdec (value, list) = encodeMsgPack value == bs && decodeMsgPack bs == Right value
    where bs = L.pack list

cantdec :: [Word8] -> Bool
cantdec list = case decodeMsgPack (L.pack list) of
    Left (ErrMsgPackDecoding _) -> True
    _                           -> False

-----------------------------------------------------------------------------
