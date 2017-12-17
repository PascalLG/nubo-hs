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

module UUID.Internal (
    UUID(..)
  , toString
  , uuidFromBytes
) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.Binary (Binary(..))
import Data.Word (Word8, Word32)
import Data.Char (intToDigit)
import Control.Monad (liftM4)

-----------------------------------------------------------------------------

-- | Our UUID data type.
--
data UUID = UUID !Word32 !Word32 !Word32 !Word32
    deriving (Eq, Ord)

-- | Make our UUID type member of Show.
--
instance Show UUID where
    show = toString

-- | Make our UUID type member of Binary.
--
instance Binary UUID where
    put (UUID w0 w1 w2 w3) = mapM_ put [w0, w1, w2, w3]
    get = liftM4 UUID get get get get

-- | Print a UUID to a String according to the
-- RFC 4122 format.
--
toString :: UUID -> String
toString (UUID w0 w1 w2 w3) = (hexw w0) ++ (hexw' w1) ++ (hexw' w2) ++ (hexw w3)
    where hexw :: Word32 -> String
          hexw w = [hexn w x | x <- [28, 24, 20, 16, 12, 8, 4, 0]]

          hexw' :: Word32 -> String
          hexw' w = [hexn w x | x <- [-1, 28, 24, 20, 16, -1, 12, 8, 4, 0]]

          hexn :: Word32 -> Int -> Char
          hexn w r 
              | r >= 0    = intToDigit $ fromIntegral $ (w `shiftR` r) .&. 0x0F
              | otherwise = '-'

-- | Build a UUID from a version number and a list of 16 bytes according
-- to RFC 4122. The version number should lie between 1 and 5.
--
uuidFromBytes :: Int -> [Word8] -> UUID
uuidFromBytes v [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf] =
    UUID (word b0 b1 b2 b3) (word b4 b5 b6' b7) (word b8' b9 ba bb) (word bc bd be bf)
    where b6' = b6 .&. 0x0F .|. (fromIntegral v `shiftL` 4)
          b8' = b8 .&. 0x3F .|. 0x80
          word a b c d = (fromIntegral a `shiftL` 24) .|. (fromIntegral b `shiftL` 16) .|. (fromIntegral c `shiftL`  8) .|. (fromIntegral d)
uuidFromBytes _ _ = error "UUID: wrong number of components"

-----------------------------------------------------------------------------
