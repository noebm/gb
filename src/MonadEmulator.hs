module MonadEmulator where

import Control.Lens

import Data.Bits.Lens
import Data.Bits
import Data.Word
import Data.Int

data Reg8 = A | B | C | D | E | F | H | L
  deriving (Eq, Show)

data Reg16 = AF | BC | DE | HL | PC | SP
  deriving (Eq, Show)

data LoadStore8 = Register8 Reg8 | Addr8 Word16
  deriving (Eq, Show)

data LoadStore16 = Register16 Reg16 | Addr16 Word16
  deriving (Eq, Show)

class Monad m => MonadEmulator m where
  -- stores
  store8 :: LoadStore8 -> Word8 -> m ()
  store16 :: LoadStore16 -> Word16 -> m ()

  -- loads
  load8 :: LoadStore8 -> m Word8
  load16 :: LoadStore16 -> m Word16

  -- timing
  advCycles :: Word -> m ()
  getCycles :: m Word

word16 :: Iso' (Word8, Word8) Word16
word16 = iso
  (\(h,l) -> shift (fromIntegral h) 8 .|. fromIntegral l)
  (\w -> (fromIntegral $ shift (w .&. 0xff00) (negate 8), fromIntegral $ w .&. 0x00ff))

flagZ, flagN, flagH, flagC :: Lens' Word8 Bool
flagZ = bitAt 7
flagN = bitAt 6
flagH = bitAt 5
flagC = bitAt 4

immediate8 :: MonadEmulator m => m Word8
immediate8 = do
  let regPC = Register16 PC
  pc <- load16 regPC
  store16 regPC (pc + 1)
  load8 (Addr8 pc)

byte :: MonadEmulator m => m Word8
byte = immediate8

ushort :: MonadEmulator m => m Word16
ushort = do
  l <- byte
  h <- byte
  return $ (h, l) ^. word16

int8 :: MonadEmulator m => m Int8
int8 = fromIntegral <$> immediate8

jump :: MonadEmulator m => Word16 -> m ()
jump = store16 (Register16 PC)

jumpRelative :: MonadEmulator m => Int8 -> m ()
jumpRelative addrdiff = do
  let aux :: Int8 -> Word16 -> Word16
      aux x addr = fromIntegral $ (fromIntegral addr :: Int) + fromIntegral x
  pc <- load16 (Register16 PC)
  store16 (Register16 PC) $ aux addrdiff pc

push :: MonadEmulator m => Word16 -> m ()
push w = do
  sp <- load16 (Register16 SP)
  store16 (Addr16 (sp - 2)) w
  store16 (Register16 SP) (sp - 2)

pop :: MonadEmulator m => m Word16
pop = do
  sp <- load16 (Register16 SP)
  store16 (Register16 SP) (sp + 2)
  load16 (Addr16 sp)

call :: MonadEmulator m => Word16 -> m ()
call addr = do
  push =<< load16 (Register16 PC)
  jump addr

ret :: MonadEmulator m => m ()
ret = jump =<< pop
