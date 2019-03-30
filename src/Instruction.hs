module Instruction
  (
    instruction
  )
where

import Prelude hiding (compare)
import Numeric

import Control.Lens

import Data.Word
import Data.Bits
import Data.Bits.Lens

import Memory.Accessible

-- import MonadEmulator hiding (load8,load16,store8,store16)
-- import qualified MonadEmulator as X

-- | decompose byte to xxyyyzzz
byteCodeDecompose :: Word8 -> (Word8, Word8, Word8)
byteCodeDecompose b =
  ((b `shiftR` 6) .&. 0x3, (b `shiftR` 3) .&. 0x7, b .&. 0x7)
{-# INLINE byteCodeDecompose #-}

data Source8 = Source8 Reg8 | PointerHL
  deriving (Show, Eq)

newtype Source16 = Source16 Reg16
  deriving (Show, Eq)

data SourcePtr = PtrBC | PtrDE | PtrHLi | PtrHLd
  deriving (Show, Eq)

hexbyte :: Word8 -> String
hexbyte w = ("0x" ++) . showHex w $ ""

hexushort :: Word16 -> String
hexushort w = ("0x" ++) . showHex w $ ""

selectSource16 :: Word8 -> Source16
selectSource16 b
  | b .&. 0x30 == 0x00 = Source16 BC
  | b .&. 0x30 == 0x10 = Source16 DE
  | b .&. 0x30 == 0x20 = Source16 HL
  | b .&. 0x30 == 0x30 = Source16 SP
  | otherwise = error "selectSource16: no known register"

timingSource8 :: Source8 -> Word
timingSource8 (Source8 _) = 4
timingSource8 PointerHL   = 8

getSource8 :: MonadEmulator m => Source8 -> m Word8
getSource8 (Source8 r) = load8 (Register8 r)
getSource8 PointerHL = load8 . Addr8 =<< load16 (Register16 HL)

writeSource8 :: MonadEmulator m => Source8 -> Word8 -> m ()
writeSource8 (Source8 r) w = store8 (Register8 r) w
writeSource8 PointerHL w = do
  addr <- load16 (Register16 HL)
  store8 (Addr8 addr) w

modifySource8 :: MonadEmulator m => Source8 -> (Word8 -> Word8) -> m ()
modifySource8 s f = writeSource8 s . f =<< getSource8 s

getSource16 :: MonadEmulator m => Source16 -> m Word16
getSource16 (Source16 r) = load16 (Register16 r)

writeSource16 :: MonadEmulator m => Source16 -> Word16 -> m ()
writeSource16 (Source16 r) dw = store16 (Register16 r) dw

modifySource16 :: MonadEmulator m => Source16 -> (Word16 -> Word16) -> m ()
modifySource16 s f = writeSource16 s . f =<< getSource16 s

modifyFlags :: MonadEmulator m => (Word8 -> Word8) -> m ()
modifyFlags g = do
  let rF = Register8 F
  flags <- load8 rF
  store8 rF $ g flags

logicOp :: (MonadEmulator m)
        => Source8
        -> (Word8 -> Word8 -> Word8)
        -> ((Word8, Word8) -> Word8 -> (Word8 -> Word8)) -- ^ (old , new) delta 
        -> m Word
logicOp reg op flagChange = do
  value <- getSource8 reg
  a <- load8 (Register8 A)
  let a' = a `op` value
  store8 (Register8 A) a'
  modifyFlags $ flagChange (a , a') value
  return $ timingSource8 reg

selectStack16 :: Word8 -> Reg16
selectStack16 b
  | b .&. 0x30 == 0x00 = BC
  | b .&. 0x30 == 0x10 = DE
  | b .&. 0x30 == 0x20 = HL
  | b .&. 0x30 == 0x30 = AF
  | otherwise = error "selectSource16: no known register"

-- reg16Stack :: (HasRegisters c) => Reg16 -> Lens' c Word16
-- reg16Stack BC = regBC
-- reg16Stack DE = regDE
-- reg16Stack HL = regHL
-- -- FIXME correct reg16 naming...
-- reg16Stack SP = regAF

{- SourcePtr -}

selectSourcePtr :: Word8 -> SourcePtr
selectSourcePtr b
  | b .&. 0x30 == 0x00 = PtrBC
  | b .&. 0x30 == 0x10 = PtrDE
  | b .&. 0x30 == 0x20 = PtrHLi
  | b .&. 0x30 == 0x30 = PtrHLd
  | otherwise = error $ "selectSourcePtr: " ++ hexbyte b

getSourcePtr :: MonadEmulator m => SourcePtr -> m Word16
getSourcePtr PtrBC  = load16 (Register16 BC)
getSourcePtr PtrDE  = load16 (Register16 DE)
getSourcePtr PtrHLi = do
  hl <- load16 (Register16 HL)
  store16 (Register16 HL) (hl + 1)
  return hl
getSourcePtr PtrHLd = do
  hl <- load16 (Register16 HL)
  store16 (Register16 HL) (hl - 1)
  return hl

{- Instructions -}

-- bitInstruction :: Int -> Source8 -> m Timing
bitInstruction :: MonadEmulator m => Int -> Source8 -> m Word
bitInstruction b s8 = do
  v <- getSource8 s8
  f <- load8 (Register8 F)
  store8 (Register8 F) $ f
    & flagZ .~ not (v `testBit` b)
    & flagN .~ False
    & flagH .~ True
  return $ timingSource8 s8 * 2

resetInstruction :: MonadEmulator m => Int -> Source8 -> m Word
resetInstruction b s8 = do
  modifySource8 s8 (bitAt b .~ False)
  return $ timingSource8 s8 * 2

setInstruction :: MonadEmulator m => Int -> Source8 -> m Word
setInstruction b s8 = do
  modifySource8 s8 (bitAt b .~ True)
  return $ timingSource8 s8 * 2

-- rotation through carry
rotateLeft  x c = rotateLeft'  x & _1 . bitAt 0 .~ c
rotateRight x c = rotateRight' x & _1 . bitAt 7 .~ c

-- rotate then copy to carry
rotateLeft'  x = (x `rotateL` 1, x `testBit` 7)

rotateRight' x = (x `rotateR` 1, x `testBit` 0)

rotateFlags x' c' = do
  f <- load8 (Register8 F)
  store8 (Register8 F) $ f
    & flagZ .~ c' -- not (v `testBit` b)
    & flagN .~ False
    & flagH .~ False
    & flagZ .~ (x' == 0)

rrc, rlc, rr,rl :: MonadEmulator m => Word8 -> m Word8
rrc x = do
  let (x' , c') = rotateRight' x
  rotateFlags x' c'
  return x'

rlc x = do
  let (x' , c') = rotateLeft' x
  rotateFlags x' c'
  return x'

rr x = do
  f <- load8 (Register8 F)
  let c = f ^. flagC
  let (x' , c') = rotateRight x c
  rotateFlags x' c'
  return x'

rl x = do
  f <- load8 (Register8 F)
  let c = f ^. flagC
  let (x' , c') = rotateLeft x c
  rotateFlags x' c'
  return x'

extendedInstruction :: MonadEmulator m => Word8 -> m Word
extendedInstruction b = case x of
  0 -> case y of
    0 -> withSource rlc
    1 -> withSource rrc
    2 -> withSource rl
    3 -> withSource rr
    _ -> error "extended instruction not implemented"
  1 -> bitInstruction bit' reg
  2 -> resetInstruction bit' reg
  3 -> setInstruction bit' reg
  _ -> error "impossible"
  where
  (x,y,z) = byteCodeDecompose b
  bit' = fromIntegral y
  withSource f = do
    getSource8 reg >>= f >>= writeSource8 reg
    return instrTime
  reg = case z of
    0 -> Source8 B
    1 -> Source8 C
    2 -> Source8 D
    3 -> Source8 E
    4 -> Source8 H
    5 -> Source8 L
    6 -> PointerHL
    7 -> Source8 A
    _ -> error "impossible"
  instrTime
    | z == 6    = 16
    | otherwise = 8

jumpRelByFlag :: MonadEmulator m => (Word8 -> Bool) -> m Word
jumpRelByFlag g = do
  relAddr <- int8
  f <- load8 (Register8 F)
  if g f
    then 12 <$ jumpRelative relAddr
    else return 8

incFlags value = do
  f <- load8 (Register8 F)
  store8 (Register8 F) $ f
    & flagZ .~ (value == 0)
    & flagN .~ False
    -- has a carry when the result has all zeros for bits <= 3
    -- & flagH .~ not (any (value `testBit`) [0..3])

decFlags value = do
  f <- load8 (Register8 F)
  store8 (Register8 F) $ f
    & flagZ .~ (value == 0)
    & flagN .~ False
    -- has a carry when the result has all zeros for bits <= 3
    -- & flagH .~ all (value `testBit`) [0..3]

compare n = do
  a <- load8 (Register8 A)
  modifyFlags $ \f -> f
    & flagZ .~ (a == n)
    & flagN .~ True
    & flagH .~ (a .&. 0x0F < n .&. 0x0F)
    & flagC .~ (a < n)

add value = do
  a <- load8 (Register8 A)
  let a' = a + value
  store8 (Register8 A) a'

  let carry7 = (a .&. value) `testBit` 7
  let carry3 = (a .&. value) `testBit` 3
  -- let zero = (carry7 && a `xor` value == 0xff) || a .|. value == 0x00
  f <- load8 (Register8 F)
  store8 (Register8 F) $ f
    & flagZ .~ (a' == 0)
    & flagN .~ False
      -- XXX is this correct?
    & flagH .~ carry3
    & flagC .~ carry7

sub value = do
  compare value
  a <- load8 (Register8 A)
  let a' = a - value
  store8 (Register8 A) a'

-- instruction :: (MonadState s m, HasRegisters s, HasRom s) => Word8 -> m Timing
instruction :: MonadEmulator m => Word8 -> m Word
instruction b = case x of
    -- 0x00 nop
  0 | b == 0x00 -> return 4
    -- 0x01 stop
    | b == 0x01 -> error "stop"
    -- 0x02 jr nz
    | b == 0x20 -> jumpRelByFlag (views flagZ not)
    -- 0x03 jr nc
    | b == 0x30 -> jumpRelByFlag (views flagC not)

    -- [ 0x01 - 0x31 ] ld ?? , d16
    | b .&. 0x0F == 0x01 && b .&. 0xF0 <= 0x30 -> do
        d16 <- ushort
        let s = selectSource16 b
        writeSource16 s d16
        return 12

    -- [ 0x02 - 0x32 ] ld (??), A
    | b .&. 0x0F == 0x02 && b .&. 0xF0 <= 0x30 -> do
        addr <- getSourcePtr (selectSourcePtr b)
        store8 (Addr8 addr) =<< load8 (Register8 A)
        return 8

    -- [ 0x03 - 0x33 ] inc ??
    | b .&. 0x0F == 0x03 && b .&. 0xF0 <= 0x30 -> do
        modifySource16 (selectSource16 b) (+1)
        return 8

    -- inc
    | b .&. 0x0F == 0x04 && b .&. 0xF0 <= 0x30 -> do
        let toReg   = reg y
        r <- getSource8 toReg
        let n = r + 1
        writeSource8 toReg n
        incFlags n
        return $ if toReg == PointerHL
          then 12
          else 4

    -- dec
    | b .&. 0x0F == 0x05 && b .&. 0xF0 <= 0x30 -> do
        let toReg   = reg y
        r <- getSource8 toReg
        let n = r - 1
        writeSource8 toReg n
        decFlags n
        return $ if toReg == PointerHL
          then 12
          else 4

    -- ld
    | b .&. 0x0F == 0x06 && b .&. 0xF0 <= 0x30 -> do
        writeSource8 (reg y) =<< byte
        return $ 4 + regtime y

    -- rlca
    | b == 0x07 -> do
        let r = Register8 A
        store8 r =<< rlc =<< load8 r
        return 4
    -- rla
    | b == 0x17 -> do
        let r = Register8 A
        store8 r =<< rl =<< load8 r
        return 4
    | b == 0x27 -> error "daa"
    | b == 0x37 -> error "scf"

    | b == 0x08 -> error "ld a16"

    -- jr
    | b == 0x18 -> do
        jumpRelative =<< int8
        return 12
    | b == 0x28 -> jumpRelByFlag (view flagZ)
    | b == 0x38 -> jumpRelByFlag (view flagC)

    | b .&. 0x0F == 0x09 && b .&. 0xF0 <= 0x30 -> do
        error "add hl"

    -- [ 0x0a - 0x3a ] ld a , (??)
    | b .&. 0x0F == 0x0A && b .&. 0xF0 <= 0x30 -> do
        let toReg = selectSourcePtr b
        store8 (Register8 A) =<< load8 . Addr8 =<< getSourcePtr toReg
        return 8

    | b .&. 0x0F == 0x0B && b .&. 0xF0 <= 0x30 -> do
        error "dec ??"

    -- [ 0x0e - 0x3e ] inc ?
    | b .&. 0x0F == 0x0C && b .&. 0xF0 <= 0x30 -> do
        let toReg   = reg y
        n <- (+1) <$> getSource8 toReg
        writeSource8 toReg n
        f <- load8 (Register8 F)
        store8 (Register8 F) $ f
          & flagZ .~ (n == 0)
          & flagN .~ False
          -- has a carry when the result has all zeros for bits <= 3
          & flagH .~ not (any (n `testBit`) [0..3])
        return 4

    -- [ 0x0e - 0x3e ] dec ?
    | b .&. 0x0F == 0x0D && b .&. 0xF0 <= 0x30 -> do
        let toReg   = reg y
        n <- (\k -> k - 1) <$> getSource8 toReg
        writeSource8 toReg n
        f <- load8 (Register8 F)
        store8 (Register8 F) $ f
          & flagZ .~ (n == 0)
          & flagN .~ True
          -- has a carry when the result has all zeros for bits <= 3
          & flagH .~ all (n `testBit`) [0..3]

        return 4

    -- [ 0x0e - 0x3e ] ld ? , d8
    | b .&. 0x0F == 0x0E && b .&. 0xF0 <= 0x30 -> do
      writeSource8 (reg y) =<< byte
      return $ 4 + regtime y

    | b == 0x0F -> do
        let r = Register8 A
        store8 r =<< rrc =<< load8 r
        return 4
      -- error "rrca"
    | b == 0x1F -> do
        let r = Register8 A
        store8 r =<< rr =<< load8 r
        return 4
    | b == 0x2F -> error "cpl"
    | b == 0x3F -> error "ccf"

  -- [ 0x40 - 0x7F ] ld ?, ?
  1 -> if b == 0x76 then error "halt"
      else do
        writeSource8 (reg y) =<< getSource8 (reg z)
        return $ max (regtime y) (regtime z)

    -- add
  2 -> case y of
    0 -> arith add
    -- adc XXX might overflow
    1 -> arithCarry add
    -- sub
    2 -> arith sub
    -- sbc XXX might overflow
    3 -> arithCarry sub

    -- and
    4 -> logicOp (reg z) (.&.) $ \(_a , a') _value x -> x
          & flagZ .~ (a' == 0)
          & flagN .~ False
          & flagH .~ True
          & flagC .~ False

    -- xor
    5 -> logicOp (reg z) xor $ \(_a , a') _value x -> x
          & flagZ .~ (a' == 0)
          & flagN .~ False
          & flagH .~ False
          & flagC .~ False

    -- [ 0xb0 - 0xb7 ] or
    6 -> logicOp (reg z) (.|.) $ \(_a , a') _value x -> x
          & flagZ .~ (a' == 0)
          & flagN .~ False
          & flagH .~ False
          & flagC .~ False

    -- [ 0xb8 - 0xbf ] cp
    7 -> arith compare
    _ -> error "impossible"

  3 | b == 0xC0 -> error "ret nz"
    | b == 0xD0 -> error "ret nc"

    | b == 0xE0 -> do
        n <- byte
        let addr = (0xFF , n) ^. word16
        store8 (Addr8 addr) =<< load8 (Register8 A)
        return 12

    | b == 0xF0 -> do
        n <- byte
        let addr = (0xFF , n) ^. word16
        store8 (Register8 A) =<< load8 (Addr8 addr)
        return 12

    | b .&. 0x0F == 0x01 && b .&. 0xF0 >= 0xC0 -> do
        store16 (Register16 $ selectStack16 b) =<< pop
        return 12

    | b == 0xe2 -> do
        a <- load8 (Register8 A)
        c <- load8 (Register8 C)
        let addr = (0xFF , c) ^. word16
        store8 (Addr8 addr) a
        return 8

    | b == 0xf2 -> do
        c <- load8 (Register8 C)
        let addr = (0xFF , c) ^. word16
        store8 (Register8 A) =<< load8 (Addr8 addr)
        return 8

    | b .&. 0x0F == 0x05 && b .&. 0xF0 >= 0xc0 -> do
        push =<< load16 (Register16 $ selectStack16 b)
        return 16

    -- ret
    | b == 0xc9 -> do
        ret
        return 16

    | b == 0xea -> do
        addr <- ushort
        store8 (Addr8 addr) =<< load8 (Register8 A)
        return 16

    | b == 0xfa -> do
        addr <- ushort
        store8 (Register8 A) =<< load8 (Addr8 addr)
        return 16

    -- 0xcb instruction
    | b == 0xcb -> extendedInstruction =<< byte
    -- call
    | b == 0xcd -> do
        call =<< ushort
        return 24


    | b == 0xFE -> do
        n <- byte
        compare n
        return 8

    | b == 0xC3 -> do
        jump =<< ushort
        return 16

    -- disable interrupts
    | b == 0xF3 -> do
        store8 (Addr8 0xFFFF) 0x00
        return 4

  _ -> do
        pc <- load16 (Register16 PC)
        error $ "instruction " ++ hexbyte b ++ " not implemented at " ++ hexushort (pc - 1)
  where
  (x,y,z) = byteCodeDecompose b
  reg w = case w of
    0 -> Source8 B
    1 -> Source8 C
    2 -> Source8 D
    3 -> Source8 E
    4 -> Source8 H
    5 -> Source8 L
    6 -> PointerHL
    7 -> Source8 A
    _ -> error "impossible"

  regtime w = if w == 6 then 8 else 4

  arith f = do
    f =<< getSource8 (reg z)
    return $ regtime z

  arithCarry f = do
    value <- getSource8 (reg z)
    c <- view flagC <$> load8 (Register8 F)
    let value' = value + if c then 1 else 0
    f value'
    return $ regtime z
