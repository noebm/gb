module Instruction
--   (
--     instruction
--   )
where

import Prelude hiding (compare)
import Numeric
import Text.Printf

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
hexbyte w = printf "0x%02x" w
  -- ("0x" ++) . showHex w $ ""

hexushort :: Word16 -> String
hexushort w = printf "0x%04x" w
  -- ("0x" ++) . showHex w $ ""

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
        -- => Source8
        => (Word8 -> Word8 -> Word8)
        -> ((Word8, Word8) -> Word8 -> (Word8 -> Word8)) -- ^ (old , new) delta 
        -> Word8
        -> m () -- Word
logicOp op flagChange value = do
  -- value <- getSource8 reg
  a <- load8 (Register8 A)
  let a' = a `op` value
  store8 (Register8 A) a'
  modifyFlags $ flagChange (a , a') value
  -- return $ timingSource8 reg

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
rotateLeft, rotateRight :: Word8 -> Bool -> (Word8, Bool)
rotateLeft  x c = rotateLeftCircular  x & _1 . bitAt 0 .~ c
rotateRight x c = rotateRightCircular x & _1 . bitAt 7 .~ c

-- rotate then copy to carry
rotateLeftCircular, rotateRightCircular :: Word8 -> (Word8, Bool)
rotateLeftCircular  x = (x `rotateL` 1, x `testBit` 7)
rotateRightCircular x = (x `rotateR` 1, x `testBit` 0)

rotateFlags x' c' = do
  f <- load8 (Register8 F)
  store8 (Register8 F) $ f
    & flagZ .~ c' -- not (v `testBit` b)
    & flagN .~ False
    & flagH .~ False
    & flagZ .~ (x' == 0)

rrc, rlc, rr,rl :: MonadEmulator m => Word8 -> m Word8
rrc x = do
  let (x' , c') = rotateRightCircular x
  rotateFlags x' c'
  return x'

rlc x = do
  let (x' , c') = rotateLeftCircular x
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

sla, sra, srl :: Monad m => Word8 -> m Word8
sla = return . (`shiftL` 1)
sra = return . (`shiftR` 1)
srl x = do
  v <- sra x
  return $ v `clearBit` 7

extendedInstruction :: MonadEmulator m => Word8 -> m Word
extendedInstruction b = case x of
  0 -> case y of
    0 -> withSource rlc
    1 -> withSource rrc
    2 -> withSource rl
    3 -> withSource rr
    4 -> withSource sla
    5 -> withSource sra
    6 -> withSource (\x -> return $ ((x `shiftL` 4) .&. 0xF0) .|. ((x `shiftR` 4) .&. 0x0F))
    7 -> withSource srl
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
  0 -> case z of
    0 -> case y of
        0 -> return 4
        1 -> do
          addr <- load16 . Addr16 =<< ushort
          store16 (Addr16 addr) =<< load16 (Register16 SP)
          return 20
        2 -> setStop >> return 0
        3 -> do
          jumpRelative =<< int8
          return 12
        _ -> jumpRelByFlag (ctrlFlags y)

    1 -> do
        let s = selectSource16 b
        if y `testBit` 0
          then do
          hl <- load16 (Register16 HL)
          r <- getSource16 s
          store16 (Register16 HL) (hl + r)
          -- XXX flags
          return 8

          -- error $ "add hl," ++ show s
          -- [ 0x01 - 0x31 ] ld ?? , d16
          else do
          writeSource16 s =<< ushort
          return 12

    -- [ 0x02 - 0x32 ] ld (??), A
    -- [ 0x0a - 0x3a ] ld a , (??)
    2 -> do
        addr <- getSourcePtr (selectSourcePtr b)
        let (s, t) = if y `testBit` 0 then (Addr8 addr, Register8 A) else (Register8 A, Addr8 addr)
        store8 t =<< load8 s
        return 8

    -- [ 0x03 - 0x33 ] inc ??
    -- [ 0x0B - 0x3B ] dec ??
    3 -> do
        if y `testBit` 0
          then modifySource16 (selectSource16 b) (\s -> s - 1)
          else modifySource16 (selectSource16 b) (+1)
        return 8

    -- inc
    4 -> do
        let r = reg y
        n <- (+1) <$>getSource8 r
        writeSource8 r n
        modifyFlags $ \f -> f
          & flagZ .~ (n == 0)
          & flagN .~ False
          -- has a carry when the result has all zeros for bits <= 3
          & flagH .~ not (any (n `testBit`) [0..3])
        return $ max (regtime y + 4) 4

    -- dec
    5 -> do
        let r = reg y
        n <- (\s -> s - 1) <$> getSource8 r
        writeSource8 r n
        modifyFlags $ \f -> f
          & flagZ .~ (n == 0)
          & flagN .~ True
          -- has a carry when the result has all zeros for bits <= 3
          & flagH .~ all (n `testBit`) [0..3]
        return $ if r == PointerHL then 12 else 4

    -- ld ? , d8
    6 -> do
        writeSource8 (reg y) =<< byte
        return $ 4 + regtime y

    7 -> case y of
        0 -> do
          let r = Register8 A
          store8 r =<< rlc =<< load8 r
          modifyFlags (flagZ .~ False)
          return 4
        1 -> do
          let r = Register8 A
          store8 r =<< rrc =<< load8 r
          modifyFlags (flagZ .~ False)
          return 4
        2 -> do
          let r = Register8 A
          store8 r =<< rl =<< load8 r
          modifyFlags (flagZ .~ False)
          return 4
        3 -> do
          let r = Register8 A
          store8 r =<< rr =<< load8 r
          modifyFlags (flagZ .~ False)
          return 4
        4 -> do
          -- XXX daa
          f <- load8 (Register8 F)
          v <- load8 (Register8 A)
          let vcorr = (if (f ^. flagH || (not (f ^. flagN) && (v .&. 0xF) > 9)) then 0x06 else 0x00)
                    + (if (f ^. flagC || (not (f ^. flagN) && v > 0x99))        then 0x60 else 0x00)
          let vcorr' = if f ^. flagN then -vcorr else vcorr
          let v' = v + vcorr'
          store8 (Register8 A) v'
          modifyFlags $ \k -> k
            & flagH .~ False
            & flagC .~ (f ^. flagC || (not (f ^. flagN) && v > 0x99))
            & flagZ .~ (v' == 0)
          return 4
        5 -> do
          store8 (Register8 A) . complement =<< load8 (Register8 A)
          modifyFlags $ (flagN .~ True) . (flagH .~ True)
          return 4
        6 -> do
          modifyFlags ((flagN .~ False) . (flagH .~ False) . (flagC .~ True))
          return 4
        7 -> do
          modifyFlags ((flagN .~ False) . (flagH .~ False) . (flagC %~ not))
          return 4
        _ -> error "impossible"
    _ -> error "impossible"

  -- [ 0x40 - 0x7F ] ld ?, ?
  1 -> if b == 0x76 then error "halt"
      else do
        writeSource8 (reg y) =<< getSource8 (reg z)
        return $ max (regtime y) (regtime z)

    -- add
  2 -> do
    alu y =<< getSource8 (reg z)
    return $ regtime z

  3 -> case z of
    0 -> if y `testBit` 2
      then if y `testBit` 0
      then error "0xE8 / 0xF8"
      else do
        n <- byte
        let addr = (0xFF , n) ^. word16
        let (t , s) = if y `testBit` 1 then (Register8 A, Addr8 addr) else (Addr8 addr, Register8 A)
        store8 t =<< load8 s
        return 12
        -- error "ldh"
      else do
        f <- ctrlFlags y <$> load8 (Register8 F)
        if f
          then ret >> return 20
          else return 4

    1 -> if y `testBit` 0
      then case y `shiftR` 1 of
        0 -> ret >> return 16
        1 -> error "reti"
        2 -> do
          jump =<< load16 (Register16 HL)
          return 4
        3 -> do
          store16 (Register16 SP) =<< load16 (Register16 HL)
          return 8
        _ -> error "impossible"

      else do
        store16 (Register16 $ selectStack16 b) =<< pop
        return 12
    2 -> if y `testBit` 2
      then do
        addr <- if y `testBit` 0
          then ushort
          else view word16 . (,) 0xFF <$> load8 (Register8 C)
        let (t , s) = if y `testBit` 1 then (Register8 A, Addr8 addr) else (Addr8 addr, Register8 A)
        store8 t =<< load8 s
        return $ if y `testBit` 0 then 16 else 8
      else do
        f <- ctrlFlags y <$> load8 (Register8 F)
        addr <- ushort
        if f
          then jump addr >> return 16
          else return 12
    3 -> case y of
        0 -> do
          jump =<< ushort
          return 16
        1 -> extendedInstruction =<< byte
        6 -> do
          store8 (Register8 F) 0x00
          return 4
          -- error "di"
        7 -> do
          store8 (Register8 F) 0x3F
          return 4
        _ -> error $ printf "invalid opcode 0x%02x" b
    4 ->
      if y `testBit` 2
      then error $ printf "invalid opcode 0x%02x" b
      else do
        f <- ctrlFlags y <$> load8 (Register8 F)
        addr <- ushort
        if f
          then call addr >> return 24
          else return 12

    5 -> if y `testBit` 0
      then if b == 0xCD then ushort >>= call >> return 24 else error $ printf "invalid opcode 0x%02x" b
      else do
        push =<< load16 (Register16 $ selectStack16 b)
        return 16
    6 -> byte >>= alu y >> return 8
    7 -> restart (y * 8) >> return 16

  _ -> do
        pc <- load16 (Register16 PC)
        error $ "instruction " ++ hexbyte b ++ " not implemented at " ++ hexushort (pc - 1)
  where
  (x,y,z) = byteCodeDecompose b
  {-# INLINE reg #-}
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

  {-# INLINE regtime #-}
  regtime w = if w == 6 then 8 else 4

  {-# INLINE alu #-}
  alu w = case w of
    0 -> add
    1 -> fcarry add
    2 -> sub
    3 -> fcarry sub
    4 -> logicOp (.&.) (\(_a , a') _value x -> x
          & flagZ .~ (a' == 0)
          & flagN .~ False
          & flagH .~ True
          & flagC .~ False)

    -- xor
    5 -> logicOp xor (\(_a , a') _value x -> x
          & flagZ .~ (a' == 0)
          & flagN .~ False
          & flagH .~ False
          & flagC .~ False)

    -- [ 0xb0 - 0xb7 ] or
    6 -> logicOp (.|.) (\(_a , a') _value s -> s
          & flagZ .~ (a' == 0)
          & flagN .~ False
          & flagH .~ False
          & flagC .~ False)
    7 -> compare

  {-# INLINE arith #-}
  arith f = do
    f =<< getSource8 (reg z)
    return $ regtime z

  {-# INLINE fcarry #-}
  fcarry f value = do
    c <- views flagC (fromIntegral . fromEnum) <$> load8 (Register8 F)
    f (value + c)

  {-# INLINE ctrlFlags #-}
  ctrlFlags :: Word8 -> Word8 -> Bool
  ctrlFlags w = views (if w `testBit` 1 then flagC else flagZ) (if w `testBit` 0 then id else not)
