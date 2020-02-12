module Instruction.Interpret where

import Data.Word
import Data.Bits
import Data.Bits.Lens (bitAt)

import Control.Lens hiding (op, to, from)
import Control.Monad
import GB

import Text.Printf

import Instruction.Instruction
import Instruction.Time
import MonadEmulator

{-# INLINE modifyFlags #-}
modifyFlags :: MonadEmulator m => (Word8 -> Word8) -> m ()
modifyFlags g = do
  flags <- loadReg F
  storeReg F $ g flags

{-# INLINE getFlag #-}
getFlag :: MonadEmulator m => Maybe Flag -> m Bool
getFlag Nothing = return True
getFlag (Just FlagC) = view flagC <$> loadReg F
getFlag (Just FlagZ) = view flagZ <$> loadReg F
getFlag (Just FlagNC) = views flagC not <$> loadReg F
getFlag (Just FlagNZ) = views flagZ not <$> loadReg F

{-# INLINE addrFF #-}
addrFF :: Word8 -> Word16
addrFF k = (0xFF , k) ^. word16

{-# INLINE getIn16 #-}
getIn16 :: MonadEmulator m => In16 -> m Word16
getIn16 arg = case arg of
  InSP        -> loadSP
  InReg16 r   -> loadReg16 r
  InImm16     -> word
  InImmAddr16 -> loadAddr16 =<< word

{-# INLINE setOut16 #-}
setOut16 :: MonadEmulator m => Out16 -> Word16 -> m ()
setOut16 arg = case arg of
  OutReg16 r   -> storeReg16 r
  OutSP        -> storeSP
  OutImmAddr16 -> \w -> (`storeAddr16` w) =<< word

getAddress :: MonadEmulator m => Addr -> m Word16
getAddress AddrBC = loadReg16 BC
getAddress AddrDE = loadReg16 DE
getAddress AddrHL = loadReg16 HL
getAddress AddrHLi = do
  hl <- loadReg16 HL
  storeReg16 HL (hl + 1)
  return hl
getAddress AddrHLd = do
  hl <- loadReg16 HL
  storeReg16 HL (hl - 1)
  return hl
getAddress AddrDirect = word
getAddress ZeroPage   = addrFF <$> byte
getAddress ZeroPageC  = addrFF <$> loadReg C

getIn8 :: MonadEmulator m => In8 -> m Word8
getIn8 (InReg8 r)     = loadReg r
getIn8 (InAddr8 addr) = loadAddr =<< getAddress addr
getIn8 InImm8         = byte

setOut8 :: MonadEmulator m => Out8 -> Word8 -> m ()
setOut8 (OutReg8 r)     = storeReg r
setOut8 (OutAddr8 addr) = \b -> (`storeAddr` b) =<< getAddress addr

daa :: MonadEmulator m => m ()
daa = do
  f <- loadReg F
  v <- loadReg A
  let vcorr'
        | f ^. flagN
        = (if f ^. flagC then 0x60 else 0x00)
        + (if f ^. flagH then 0x06 else 0x00)
        | otherwise
        = (if f ^. flagC || v > 0x99              then 0x60 else 0x00)
        + (if (f ^. flagH) || (v .&. 0x0f) > 0x09 then 0x06 else 0x00)
  let v' = if f ^. flagN then v - vcorr' else v + vcorr'
  storeReg A v'
  storeReg F $ f
    & flagH .~ False
    & flagC .~ (f ^. flagC || (not (f ^. flagN) && v > 0x99))
    & flagZ .~ (v' == 0)

{-# INLINE add #-}
add :: Word8 -> Word8 -> Bool -> (Word8 , Word8)
add a v c =
  (s , 0x00 & flagZ .~ (s == 0) & flagH .~ (carry_info `testBit` 4) & flagC .~ (carry_info `testBit` 8))
  where
    v' = fromIntegral v
    a' = fromIntegral a
    s' = a' + v' :: Int
    s'' = s' + fromEnum c

    carry_info = let f = xor (a' `xor` v') in (f s' .|. f s'')
    s = fromIntegral s''

{-# INLINE sub #-}
sub :: Word8 -> Word8 -> Bool -> (Word8 , Word8)
sub a v c =
  (s , 0x40 & flagZ .~ (s == 0) & flagH .~ (carry_info `testBit` 4) & flagC .~ (carry_info `testBit` 8))
  where
    v' = fromIntegral v
    a' = fromIntegral a
    s' = a' - v' :: Int
    s'' = s' - fromEnum c

    carry_info = let f = xor (a' `xor` v') in (f s' .|. f s'')
    s = fromIntegral s''

{-# INLINE arith #-}
arith :: MonadEmulator m
      => (Word8 -> Word8 -> Bool -> (Word8 , Word8))
      -> m Word8
      -> Bool
      -> m ()
arith fun g useCarry = do
  k <- g
  a <- loadReg A
  cf <- if useCarry then view flagC <$> loadReg F else return False
  let (a' , f) = fun a k cf
  storeReg A a'
  storeReg F f

{-# INLINE rotateLeft #-}
rotateLeft :: Word8 -> Bool -> (Word8, Bool)
rotateLeft v c =
  let v' = v `rotateL` 1
      c' = v' `testBit` 0
  in (v' & bitAt 0 .~ c, c')

{-# INLINE rotateRight #-}
rotateRight :: Word8 -> Bool -> (Word8, Bool)
rotateRight v c =
  let v' = v `rotateR` 1
      c' = v' `testBit` 7
  in (v' & bitAt 7 .~ c, c')

{-# INLINE rotateLeftCarry #-}
rotateLeftCarry :: Word8 -> (Word8, Bool)
rotateLeftCarry v = rotateLeft v (v `testBit` 7)

{-# INLINE rotateRightCarry #-}
rotateRightCarry :: Word8 -> (Word8, Bool)
rotateRightCarry v = rotateRight v (v `testBit` 0)

{-# INLINE shiftLeftArithmetic #-}
shiftLeftArithmetic :: Word8 -> (Word8, Bool)
shiftLeftArithmetic v =
  let v' = v `shiftL` 1
      c' = v `testBit` 7
  in (v', c')

{-# INLINE shiftRightArithmetic #-}
shiftRightArithmetic :: Word8 -> (Word8, Bool)
shiftRightArithmetic v =
  let v' = v `shiftR` 1
      c' = v `testBit` 0
  in (v' & bitAt 7 .~ (v `testBit` 7), c')

{-# SPECIALISE interpretM :: Instruction -> GB IO (Word , StepInfo) #-}
interpretM :: (HardwareMonad m, MonadEmulator m) => Instruction -> m (Word , StepInfo)
interpretM instr@(Instruction _ t op) = case op of
  NOP -> (,) (getTime True t) <$> prefetch

  LD from to -> do
    setOut8 to =<< getIn8 from
    (,) (getTime True t) <$> prefetch

  LD16 from to -> do
    setOut16 to =<< getIn16 from
    (,) (getTime True t) <$> prefetch

  LD16_SP_HL -> do
    sp <- loadSP
    r <- sbyte
    let v = addRelative sp r
    storeReg16 HL v
    storeReg F $ 0x00
      & flagC .~ ((v .&. 0xFF) < (sp .&. 0xFF))
      & flagH .~ ((v .&. 0x0F) < (sp .&. 0x0F))
    (,) (getTime True t) <$> prefetch

  AND arg -> do
    v <- getIn8 arg
    a <- loadReg A
    let a' = a .&. v
    storeReg A a'
    modifyFlags $ \_ -> 0x20
      & flagZ .~ (a' == 0)
    (,) (getTime True t) <$> prefetch

  OR arg -> do
    v <- getIn8 arg
    a <- loadReg A
    let a' = a .|. v
    storeReg A a'
    modifyFlags $ \_ -> 0x00
      & flagZ .~ (a' == 0)
    (,) (getTime True t) <$> prefetch

  XOR arg -> do
    v <- getIn8 arg
    a <- loadReg A
    let a' = a `xor` v
    storeReg A (a `xor` v)
    modifyFlags $ \_ -> 0x00
      & flagZ .~ (a' == 0)
    (,) (getTime True t) <$> prefetch

  {- 0xCB instructions and specialization for A -}
  BIT y arg -> do
    v <- getIn8 arg
    modifyFlags $ \f -> f
      & flagZ .~ not (v `testBit` fromIntegral y)
      & flagN .~ False
      & flagH .~ True
    (,) (getTime True t) <$> prefetch

  SWAP arg -> do
    x <- getIn8 (outToIn arg)
    let x' = ((x `shiftL` 4) .&. 0xF0) .|. ((x `shiftR` 4) .&. 0x0F)
    setOut8 arg x'
    modifyFlags $ \_ -> 0x00 & flagZ .~ (x' == 0)
    (,) (getTime True t) <$> prefetch

  RES bidx arg -> do
    setOut8 arg . (`clearBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    (,) (getTime True t) <$> prefetch

  SET bidx arg -> do
    setOut8 arg . (`setBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    (,) (getTime True t) <$> prefetch

  RL arg -> do
    v <- getIn8 (outToIn arg)
    c <- view flagC <$> loadReg F
    let (v' , c') = rotateLeft v c
    setOut8 arg v'
    storeReg F (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    (,) (getTime True t) <$> prefetch

  RLA -> do
    v <- loadReg A
    c <- view flagC <$> loadReg F
    let (v' , c') = rotateLeft v c
    storeReg A v'
    storeReg F (0x00 & flagC .~ c')
    (,) (getTime True t) <$> prefetch

  RR arg -> do
    v <- getIn8 (outToIn arg)
    c <- view flagC <$> loadReg F
    let (v' , c') = rotateRight v c
    setOut8 arg v'
    storeReg F (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    (,) (getTime True t) <$> prefetch

  RRA -> do
    v <- loadReg A
    c <- view flagC <$> loadReg F
    let (v' , c') = rotateRight v c
    storeReg A v'
    storeReg F (0x00 & flagC .~ c')
    (,) (getTime True t) <$> prefetch

  RLCA -> do
    v <- loadReg A
    let (v' , c') = rotateLeftCarry v
    storeReg A v'
    storeReg F (0x00 & flagC .~ c')
    (,) (getTime True t) <$> prefetch

  RRCA -> do
    v <- loadReg A
    let (v' , c') = rotateRightCarry v
    storeReg A v'
    storeReg F (0x00 & flagC .~ c')
    (,) (getTime True t) <$> prefetch

  RLC arg -> do
    v <- getIn8 (outToIn arg)
    let (v' , c') = rotateLeftCarry v
    setOut8 arg v'
    storeReg F (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    (,) (getTime True t) <$> prefetch

  RRC arg -> do
    v <- getIn8 (outToIn arg)
    let (v' , c') = rotateRightCarry v
    setOut8 arg v'
    storeReg F (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    (,) (getTime True t) <$> prefetch

  SRL arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v `shiftR` 1
    setOut8 arg v'
    modifyFlags $ \_ -> 0x00
      & flagC .~ (v `testBit` 0)
      & flagZ .~ (v' == 0)
    (,) (getTime True t) <$> prefetch

  SLA arg -> do
    v <- getIn8 (outToIn arg)
    let (v' , c') = shiftLeftArithmetic v
    setOut8 arg v'
    storeReg F (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    (,) (getTime True t) <$> prefetch

  SRA arg -> do
    v <- getIn8 (outToIn arg)
    let (v' , c') = shiftRightArithmetic v
    setOut8 arg v'
    storeReg F (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    (,) (getTime True t) <$> prefetch

  JR f -> do
    t' <- getFlag f
    r <- sbyte
    when t' $ jumpRelative r
    (,) (getTime t' t) <$> prefetch

  JP f addr -> do
    t' <- getFlag f
    addr <- getAddress addr
    when t' $ storePC addr
    (,) (getTime t' t) <$> prefetch

  CALL f -> do
    t' <- getFlag f
    when t' . call =<< word
    (,) (getTime t' t) <$> prefetch

  RET f -> do
    t' <- getFlag f
    when t' ret
    (,) (getTime t' t) <$> prefetch

  RETI -> do
    setIME True
    ret
    (,) (getTime True t) <$> prefetch

  RST g -> do
    restart $ (* 8) g
    (,) (getTime True t) <$> prefetch

  PUSH reg -> do
    push =<< loadReg16 reg
    (,) (getTime True t) <$> prefetch
  POP reg -> do
    pop >>= storeReg16 reg
    when (reg == AF) (modifyFlags (.&. 0xF0))
    (,) (getTime True t) <$> prefetch

  ADD arg -> do
    arith add (getIn8 arg) False
    (,) (getTime True t) <$> prefetch

  ADD16_HL from -> do
    v <- loadReg16 HL
    dv <- getIn16 from
    let v' = v + dv
    storeReg16 HL v'
    modifyFlags $ \f -> f
      & flagN .~ False
      & flagC .~ (v' < v)
      & flagH .~ ((v' .&. 0x0FFF) < (v .&. 0x0FFF))
    (,) (getTime True t) <$> prefetch

  ADD16_SP -> do
    v <- loadSP
    dv <- sbyte
    let v' = addRelative v dv
    storeSP v'
    modifyFlags $ \f -> 0x00
      & flagC .~ ((v' .&. 0xFF) < (v .&. 0xFF))
      & flagH .~ ((v' .&. 0x0F) < (v .&. 0x0F))
    (,) (getTime True t) <$> prefetch

  SUB arg -> do
    arith sub (getIn8 arg) False
    (,) (getTime True t) <$> prefetch

  ADC arg -> do
    arith add (getIn8 arg) True
    (,) (getTime True t) <$> prefetch

  SBC arg -> do
    arith sub (getIn8 arg) True
    (,) (getTime True t) <$> prefetch

  CP arg -> do
    k <- getIn8 arg
    a <- loadReg A
    let (_, f) = sub a k False
    storeReg F f
    (,) (getTime True t) <$> prefetch

  INC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v + 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0xFF)
      & flagN .~ False
      & flagH .~ (v .&. 0x0F == 0x0F)
    (,) (getTime True t) <$> prefetch

  INC16 arg -> do
    setOut16 arg . (+1) =<< getIn16 (out16ToIn16 arg)
    (,) (getTime True t) <$> prefetch

  DEC16 arg -> do
    setOut16 arg . subtract 1 =<< getIn16 (out16ToIn16 arg)
    (,) (getTime True t) <$> prefetch

  DEC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v - 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0x01)
      & flagN .~ True
      & flagH .~ (v .&. 0x0F == 0x00)
    (,) (getTime True t) <$> prefetch

  DI -> do
    setIME False
    (,) (getTime True t) <$> prefetch

  EI -> do
    setIME True
    (,) (getTime True t) <$> prefetch

  DAA -> do
    daa
    (,) (getTime True t) <$> prefetch

  CPL -> do
    storeReg A . complement =<< loadReg A
    modifyFlags $ \f -> f
      & flagH .~ True
      & flagN .~ True
    (,) (getTime True t) <$> prefetch

  CCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC %~ not
    (,) (getTime True t) <$> prefetch

  SCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC .~ True
    (,) (getTime True t) <$> prefetch

  HALT -> do
    i <- anyInterrupts
    ime <- getIME
    out <- if not ime && has _Just i then loadPC >>= fmap Running . loadAddr else return Halt
    return (getTime True t, out)

  _ -> error $ "failed at " ++ show instr
