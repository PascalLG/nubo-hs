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

module MsgPack (
      MsgValue(..)
    , (!?)
    , msgString
    , encodeMsgPack
    , decodeMsgPack
) where

import Prelude hiding (lookup)
import Control.Monad (replicateM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString.Builder
import Data.Binary.Get
import Data.Monoid ((<>))
import Data.Bits ((.|.), (.&.))
import Data.Int (Int8)
import Error

-----------------------------------------------------------------------------
-- Type to represent a MessagePack tree.

data MsgValue = MsgString T.Text
              | MsgBinary B.ByteString
              | MsgInteger Int
              | MsgFloat Double
              | MsgBool Bool
              | MsgNull
              | MsgObject [(String, MsgValue)]
              | MsgArray [MsgValue]
                deriving(Eq, Ord, Show)

-----------------------------------------------------------------------------
-- Handling MessagePack data.

-- | Lookup a value from its key. Return Nothing if the key is not
-- found or if the passed value is not an object.
--
lookup :: String -> MsgValue -> Maybe MsgValue
lookup name (MsgObject list) = find list
    where
        find :: [(String, MsgValue)] -> Maybe MsgValue
        find [] = Nothing
        find ((k, v):xs)
             | k == name = Just v
             | otherwise = find xs
lookup _ _ = Nothing

-- | Operator version of lookup.
--
infixl 9 !?
(!?) :: MsgValue -> String -> Maybe MsgValue
(!?) m k = lookup k m

-- | Helper function to build a MsgString value from a String.
--
msgString :: String -> MsgValue
msgString = MsgString . T.pack

-----------------------------------------------------------------------------
-- Encoding MessagePack data.

-- | Encode a MessagePack structure to a lazy ByteString.
--
encodeMsgPack :: MsgValue -> L.ByteString
encodeMsgPack = toLazyByteString . encodeMsgValue

-- | Encode a single MessagePack value.
--
encodeMsgValue :: MsgValue -> Builder

encodeMsgValue MsgNull = word8 0xC0
encodeMsgValue (MsgBool False) = word8 0xC2
encodeMsgValue (MsgBool True) = word8 0xC3
encodeMsgValue (MsgFloat n) = word8 0xCB <> doubleBE n

encodeMsgValue (MsgString s)
    | len < 32      = word8 (0xA0 .|. fromIntegral len) <> content
    | len < 256     = word8 0xD9 <> word8 (fromIntegral len) <> content
    | len < 65536   = word8 0xDA <> word16BE (fromIntegral len) <> content
    | otherwise     = word8 0xDB <> word32BE (fromIntegral len) <> content
    where
        utf = encodeUtf8 s
        content = byteString utf
        len = B.length utf

encodeMsgValue (MsgBinary b)
    | len < 256     = word8 0xC4 <> word8 (fromIntegral len) <> content
    | len < 65536   = word8 0xC5 <> word16BE (fromIntegral len) <> content
    | otherwise     = word8 0xC6 <> word32BE (fromIntegral len) <> content
    where
        len = B.length b
        content = byteString b

encodeMsgValue (MsgInteger n)
    | n < -2147483648   = word8 0xD3 <> int64BE (fromIntegral n)
    | n < -32768        = word8 0xD2 <> int32BE (fromIntegral n)
    | n < -128          = word8 0xD1 <> int16BE (fromIntegral n)
    | n < -32           = word8 0xD0 <> int8 (fromIntegral n)
    | n < 128           = int8 (fromIntegral n)
    | n < 256           = word8 0xCC <> word8 (fromIntegral n)
    | n < 65536         = word8 0xCD <> word16BE (fromIntegral n)
    | n < 4294967296    = word8 0xCE <> word32BE (fromIntegral n)
    | otherwise         = word8 0xCF <> word64BE (fromIntegral n)

encodeMsgValue (MsgArray a)
    | len < 16      = word8 (0x90 .|. fromIntegral len) <> objects
    | len < 65536   = word8 0xDC <> word16BE (fromIntegral len) <> objects
    | otherwise     = word8 0xDD <> word32BE (fromIntegral len) <> objects
    where
        len = length a
        objects = mconcat (map encodeMsgValue a)

encodeMsgValue (MsgObject o)
    | len < 16      = word8 (0x80 .|. fromIntegral len) <> pairs
    | len < 65536   = word8 0xDE <> word16BE (fromIntegral len) <> pairs
    | otherwise     = word8 0xDF <> word32BE (fromIntegral len) <> pairs
    where
        len = length o
        pair (k, v) = encodeMsgValue (MsgString (T.pack k)) <> encodeMsgValue v
        pairs = mconcat (map pair o)

-----------------------------------------------------------------------------
-- Decoding MessagePack data.

-- | Decode a MessagePack structure from a lazy bytestring. Return either an
-- error message or the decoded data.
--
decodeMsgPack :: L.ByteString -> Either Error MsgValue
decodeMsgPack msg = case runGetOrFail decodeMsgValue msg of
    Left (_, _, e)  -> Left $ ErrMsgPackDecoding e
    Right (r, _, v) -> if L.null r then Right v
                                   else Left $ ErrMsgPackDecoding "extra input"

-- | Decode a single MessagePack value.
--
decodeMsgValue :: Get MsgValue
decodeMsgValue = do
    code <- getWord8
    if      code <= 0x7F then return $ MsgInteger (fromIntegral code)
    else if code <= 0x8F then decodeObject (fromIntegral (code .&. 0x0F))
    else if code <= 0x9F then decodeArray (fromIntegral (code .&. 0x0F))
    else if code <= 0xBF then decodeString (fromIntegral (code .&. 0x1F))
    else if code == 0xC0 then return MsgNull
    else if code == 0xC2 then return $ MsgBool False
    else if code == 0xC3 then return $ MsgBool True
    else if code == 0xC4 then getWord8 >>= \len -> decodeBinary (fromIntegral len)
    else if code == 0xC5 then getWord16be >>= \len -> decodeBinary (fromIntegral len)
    else if code == 0xC6 then getWord32be >>= \len -> decodeBinary (fromIntegral len)
    else if code == 0xCB then MsgFloat <$> getDoublebe
    else if code == 0xCC then MsgInteger . fromIntegral <$> getWord8
    else if code == 0xCD then MsgInteger . fromIntegral <$> getWord16be
    else if code == 0xCE then MsgInteger . fromIntegral <$> getWord32be
    else if code == 0xCF then MsgInteger . fromIntegral <$> getWord64be
    else if code == 0xD0 then MsgInteger . fromIntegral <$> getInt8
    else if code == 0xD1 then MsgInteger . fromIntegral <$> getInt16be
    else if code == 0xD2 then MsgInteger . fromIntegral <$> getInt32be
    else if code == 0xD3 then MsgInteger . fromIntegral <$> getInt64be
    else if code == 0xD9 then getWord8 >>= \len -> decodeString (fromIntegral len)
    else if code == 0xDA then getWord16be >>= \len -> decodeString (fromIntegral len)
    else if code == 0xDB then getWord32be >>= \len -> decodeString (fromIntegral len)
    else if code == 0xDC then getWord16be >>= \len -> decodeArray (fromIntegral len)
    else if code == 0xDD then getWord32be >>= \len -> decodeArray (fromIntegral len)
    else if code == 0xDE then getWord16be >>= \len -> decodeObject (fromIntegral len)
    else if code == 0xDF then getWord32be >>= \len -> decodeObject (fromIntegral len)
    else if code >= 0xE0 then return $ MsgInteger (fromIntegral (fromIntegral code :: Int8))
    else                      fail "invalid encoding"

    where
        decodeString len = MsgString . decodeUtf8 <$> getByteString len
        decodeBinary len = MsgBinary <$> getByteString len
        decodeArray len = MsgArray <$> replicateM len decodeMsgValue
        decodeObject len = MsgObject <$> replicateM len decodePair
        decodePair = do
            k <- decodeMsgValue
            v <- decodeMsgValue
            case k of
                MsgString s -> return (T.unpack s, v)
                _           -> fail "object key should be a string"

-----------------------------------------------------------------------------
