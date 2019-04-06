module VectorUtils where

import qualified Data.ByteString             as B
import qualified Data.Vector.Unboxed         as VU
-- import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Storable as VS

import Data.Word

byteStringToVector :: B.ByteString -> VU.Vector Word8
byteStringToVector bs = VU.generate (B.length bs) $ B.index bs

vectorV4ToByteString :: VS.Vector Word8 -> B.ByteString
vectorV4ToByteString xs = B.pack $ VS.toList xs

