{-# LANGUAGE FlexibleContexts #-}
module Utilities.Vector
  ( byteStringToVector
  , vectorToByteString
  ) where

import qualified Data.ByteString.Internal      as B
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Storable          as VS

import           Data.Word

{-# INLINE byteStringToVector #-}
byteStringToVector :: VG.Vector v Word8 => B.ByteString -> v Word8
byteStringToVector = VS.convert . vector

{-# INLINE vectorToByteString #-}
vectorToByteString :: VG.Vector v Word8 => v Word8 -> B.ByteString
vectorToByteString = byteString . VS.convert

{-# INLINE vector #-}
vector :: B.ByteString -> VS.Vector Word8
vector (B.PS ptr off l) = VS.unsafeFromForeignPtr ptr off l

{-# INLINE byteString #-}
byteString :: VS.Vector Word8 -> B.ByteString
byteString xs = B.PS ptr off l where (ptr, off, l) = VS.unsafeToForeignPtr xs

