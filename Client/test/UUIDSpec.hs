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

module UUIDSpec (
    spec
) where

import Test.Hspec
import Data.Binary (encode, decode)
import UUID.Internal

-----------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "toString" $ do
        it "prints a UUID" $ do
            toString (UUID 0x00000000 0x00000000 0x00000000 0x00000000) `shouldBe` "00000000-0000-0000-0000-000000000000"
            toString (UUID 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF 0xFFFFFFFF) `shouldBe` "ffffffff-ffff-ffff-ffff-ffffffffffff"
            toString (UUID 0x00010203 0x04050607 0x08090A0B 0x0C0D0E0F) `shouldBe` "00010203-0405-0607-0809-0a0b0c0d0e0f"
            toString (UUID 0xF0E0D0C0 0xB0A09080 0x70605040 0x30201000) `shouldBe` "f0e0d0c0-b0a0-9080-7060-504030201000"

    describe "uuidFromBytes" $ do
        it "builds a UUID" $ do
            uuidFromBytes 1 (replicate 16 0)    `shouldBe` UUID 0x00000000 0x00001000 0x80000000 0x00000000
            uuidFromBytes 2 (replicate 16 0)    `shouldBe` UUID 0x00000000 0x00002000 0x80000000 0x00000000
            uuidFromBytes 3 (replicate 16 0)    `shouldBe` UUID 0x00000000 0x00003000 0x80000000 0x00000000
            uuidFromBytes 4 (replicate 16 0)    `shouldBe` UUID 0x00000000 0x00004000 0x80000000 0x00000000
            uuidFromBytes 1 (replicate 16 255)  `shouldBe` UUID 0xFFFFFFFF 0xFFFF1FFF 0xBFFFFFFF 0xFFFFFFFF
            uuidFromBytes 4 (replicate 16 255)  `shouldBe` UUID 0xFFFFFFFF 0xFFFF4FFF 0xBFFFFFFF 0xFFFFFFFF

    describe "put/get" $ do
        it "serializes a UUID" $ do
            let t1 = UUID 0x01234567 0x89ABCDEF 0xFEDCBA98 0x76543210
            let t2 = UUID 0x00010203 0x04050607 0x08090A0B 0x0C0D0E0F
            (decode . encode) t1 `shouldBe` t1
            (decode . encode) t2 `shouldBe` t2

-----------------------------------------------------------------------------
