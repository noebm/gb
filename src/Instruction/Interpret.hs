module Instruction.Interpret where

import Data.Word
import Data.Int
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
  let rF = Register8 F
  flags <- load8 rF
  store8 rF $ g flags

{-# INLINE getFlag #-}
getFlag :: MonadEmulator m => Maybe Flag -> m Bool
getFlag Nothing = return True
getFlag (Just FlagC) = view flagC <$> load8 (Register8 F)
getFlag (Just FlagZ) = view flagZ <$> load8 (Register8 F)
getFlag (Just FlagNC) = views flagC not <$> load8 (Register8 F)
getFlag (Just FlagNZ) = views flagZ not <$> load8 (Register8 F)

{-# INLINE addrFF #-}
addrFF :: Word8 -> Word16
addrFF k = (0xFF , k) ^. word16

{-# INLINE getIn16 #-}
getIn16 :: MonadEmulator m => In16 -> m Word16
getIn16 arg = case arg of
  InSP        -> loadSP
  InReg16 r   -> load16 $ Register16 r
  InImm16     -> word
  InImmAddr16 -> load16 . Addr16 =<< word

{-# INLINE setOut16 #-}
setOut16 :: MonadEmulator m => Out16 -> Word16 -> m ()
setOut16 arg = case arg of
  OutReg16 r   -> store16 (Register16 r)
  OutSP        -> storeSP
  OutImmAddr16 -> \w -> (`store16` w) . Addr16 =<< word

getAddress :: MonadEmulator m => Addr -> m Word16
getAddress AddrBC = load16 (Register16 BC)
getAddress AddrDE = load16 (Register16 DE)
getAddress AddrHL = load16 (Register16 HL)
getAddress AddrHLi = do
  hl <- load16 (Register16 HL)
  store16 (Register16 HL) (hl + 1)
  return hl
getAddress AddrHLd = do
  hl <- load16 (Register16 HL)
  store16 (Register16 HL) (hl - 1)
  return hl
getAddress AddrDirect = word
getAddress ZeroPage   = addrFF <$> byte
getAddress ZeroPageC  = addrFF <$> load8 (Register8 C)

getIn8 :: MonadEmulator m => In8 -> m Word8
getIn8 (InReg8 r)     = load8 (Register8 r)
getIn8 (InAddr8 addr) = load8 . Addr8 =<< getAddress addr
getIn8 InImm8         = byte

setOut8 :: MonadEmulator m => Out8 -> Word8 -> m ()
setOut8 (OutReg8 r)     = store8 (Register8 r)
setOut8 (OutAddr8 addr) = \b -> (`store8` b) . Addr8 =<< getAddress addr

daa :: MonadEmulator m => m ()
daa = do
  f <- load8 (Register8 F)
  v <- load8 (Register8 A)
  let vcorr'
        | f ^. flagN
        = (if f ^. flagC then 0x60 else 0x00)
        + (if f ^. flagH then 0x06 else 0x00)
        | otherwise
        = (if f ^. flagC || v > 0x99              then 0x60 else 0x00)
        + (if (f ^. flagH) || (v .&. 0x0f) > 0x09 then 0x06 else 0x00)
  let v' = if f ^. flagN then v - vcorr' else v + vcorr'
  store8 (Register8 A) v'
  store8 (Register8 F) $ f
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
  a <- load8 (Register8 A)
  cf <- if useCarry then view flagC <$> load8 (Register8 F) else return False
  let (a' , f) = fun a k cf
  store8 (Register8 A) a'
  store8 (Register8 F) f

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

{-# SPECIALISE interpretM :: Instruction -> GB IO Word #-}
interpretM :: MonadEmulator m => Instruction -> m Word
interpretM instr@(Instruction _ t op) = case op of
  NOP -> return $ getTime True t

  LD from to -> do
    setOut8 to =<< getIn8 from
    return $ getTime True t

  LD16 from to -> do
    setOut16 to =<< getIn16 from
    return $ getTime True t

  LD16_SP_HL -> do
    sp <- loadSP
    r <- sbyte
    let v = addRelative sp r
    store16 (Register16 HL) v
    store8 (Register8 F) $ 0x00
      & flagC .~ ((v .&. 0xFF) < (sp .&. 0xFF))
      & flagH .~ ((v .&. 0x0F) < (sp .&. 0x0F))
    return $ getTime True t

  AND arg -> do
    v <- getIn8 arg
    a <- load8 (Register8 A)
    let a' = a .&. v
    store8 (Register8 A) a'
    modifyFlags $ \_ -> 0x20
      & flagZ .~ (a' == 0)
    return $ getTime True t

  OR arg -> do
    v <- getIn8 arg
    a <- load8 (Register8 A)
    let a' = a .|. v
    store8 (Register8 A) a'
    modifyFlags $ \_ -> 0x00
      & flagZ .~ (a' == 0)
    return $ getTime True t

  XOR arg -> do
    v <- getIn8 arg
    a <- load8 (Register8 A)
    let a' = a `xor` v
    store8 (Register8 A) (a `xor` v)
    modifyFlags $ \_ -> 0x00
      & flagZ .~ (a' == 0)
    return $ getTime True t

  {- 0xCB instructions and specialization for A -}
  BIT y arg -> do
    v <- getIn8 arg
    modifyFlags $ \f -> f
      & flagZ .~ not (v `testBit` fromIntegral y)
      & flagN .~ False
      & flagH .~ True
    return $ getTime True t

  SWAP arg -> do
    x <- getIn8 (outToIn arg)
    let x' = ((x `shiftL` 4) .&. 0xF0) .|. ((x `shiftR` 4) .&. 0x0F)
    setOut8 arg x'
    modifyFlags $ \_ -> 0x00 & flagZ .~ (x' == 0)
    return $ getTime True t

  RES bidx arg -> do
    setOut8 arg . (`clearBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    return $ getTime True t

  SET bidx arg -> do
    setOut8 arg . (`setBit` fromIntegral bidx) =<< getIn8 (outToIn arg)
    return $ getTime True t

  RL arg -> do
    v <- getIn8 (outToIn arg)
    c <- view flagC <$> load8 (Register8 F)
    let (v' , c') = rotateLeft v c
    setOut8 arg v'
    store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    return $ getTime True t

  RLA -> do
    v <- load8 (Register8 A)
    c <- view flagC <$> load8 (Register8 F)
    let (v' , c') = rotateLeft v c
    store8 (Register8 A) v'
    store8 (Register8 F) (0x00 & flagC .~ c')
    return $ getTime True t

  RR arg -> do
    v <- getIn8 (outToIn arg)
    c <- view flagC <$> load8 (Register8 F)
    let (v' , c') = rotateRight v c
    setOut8 arg v'
    store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    return $ getTime True t

  RRA -> do
    v <- load8 (Register8 A)
    c <- view flagC <$> load8 (Register8 F)
    let (v' , c') = rotateRight v c
    store8 (Register8 A) v'
    store8 (Register8 F) (0x00 & flagC .~ c')
    return $ getTime True t

  RLCA -> do
    v <- load8 (Register8 A)
    let (v' , c') = rotateLeftCarry v
    store8 (Register8 A) v'
    store8 (Register8 F) (0x00 & flagC .~ c')
    return $ getTime True t

  RRCA -> do
    v <- load8 (Register8 A)
    let (v' , c') = rotateRightCarry v
    store8 (Register8 A) v'
    store8 (Register8 F) (0x00 & flagC .~ c')
    return $ getTime True t

  RLC arg -> do
    v <- getIn8 (outToIn arg)
    let (v' , c') = rotateLeftCarry v
    setOut8 arg v'
    store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    return $ getTime True t

  RRC arg -> do
    v <- getIn8 (outToIn arg)
    let (v' , c') = rotateRightCarry v
    setOut8 arg v'
    store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    return $ getTime True t

  SRL arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v `shiftR` 1
    setOut8 arg v'
    modifyFlags $ \_ -> 0x00
      & flagC .~ (v `testBit` 0)
      & flagZ .~ (v' == 0)
    return $ getTime True t

  SLA arg -> do
    v <- getIn8 (outToIn arg)
    let (v' , c') = shiftLeftArithmetic v
    setOut8 arg v'
    store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    return $ getTime True t

  SRA arg -> do
    v <- getIn8 (outToIn arg)
    let (v' , c') = shiftRightArithmetic v
    setOut8 arg v'
    store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
    return $ getTime True t

  JR f -> do
    t' <- getFlag f
    r <- sbyte
    when t' $ jumpRelative r
    return $ getTime t' t

  JP f addr -> do
    t' <- getFlag f
    addr <- getAddress addr
    when t' $ storePC addr
    return $ getTime t' t

  CALL f -> do
    t' <- getFlag f
    when t' . call =<< word
    return $ getTime t' t

  RET f -> do
    t' <- getFlag f
    when t' ret
    return $ getTime t'  t

  RETI -> do
    setIEM True
    ret
    return $ getTime True t

  RST g -> do
    restart $ (* 8) g
    return $ getTime True t

  PUSH reg -> do
    push =<< load16 (Register16 reg)
    return $ getTime True t
  POP reg -> do
    pop >>= store16 (Register16 reg)
    when (reg == AF) (modifyFlags (.&. 0xF0))
    return $ getTime True t

  ADD arg -> do
    arith add (getIn8 arg) False
    return (getTime True t)

  ADD16_HL from -> do
    v <- load16 (Register16 HL)
    dv <- getIn16 from
    let v' = v + dv
    store16 (Register16 HL) v'
    modifyFlags $ \f -> f
      & flagN .~ False
      & flagC .~ (v' < v)
      & flagH .~ ((v' .&. 0x0FFF) < (v .&. 0x0FFF))
    return $ getTime True t

  ADD16_SP -> do
    v <- loadSP
    dv <- sbyte
    let v' = addRelative v dv
    storeSP v'
    modifyFlags $ \f -> 0x00
      & flagC .~ ((v' .&. 0xFF) < (v .&. 0xFF))
      & flagH .~ ((v' .&. 0x0F) < (v .&. 0x0F))
    return $ getTime True t

  SUB arg -> do
    arith sub (getIn8 arg) False
    return (getTime True t)

  ADC arg -> do
    arith add (getIn8 arg) True
    return (getTime True t)

  SBC arg -> do
    arith sub (getIn8 arg) True
    return (getTime True t)

  CP arg -> do
    k <- getIn8 arg
    a <- load8 (Register8 A)
    let (_, f) = sub a k False
    store8 (Register8 F) f
    return $ getTime True t

  INC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v + 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0xFF)
      & flagN .~ False
      & flagH .~ (v .&. 0x0F == 0x0F)
    return $ getTime True t

  INC16 arg -> do
    setOut16 arg . (+1) =<< getIn16 (out16ToIn16 arg)
    return $ getTime True t

  DEC16 arg -> do
    setOut16 arg . subtract 1 =<< getIn16 (out16ToIn16 arg)
    return $ getTime True t

  DEC arg -> do
    v <- getIn8 (outToIn arg)
    let v' = v - 1
    setOut8 arg v'
    modifyFlags $ \f -> f
      & flagZ .~ (v == 0x01)
      & flagN .~ True
      & flagH .~ (v .&. 0x0F == 0x00)
    return $ getTime True t

  DI -> do
    setIEM False
    return (getTime True t)

  EI -> do
    setIEM True
    return (getTime True t)

  DAA -> do
    daa
    return (getTime True t)

  CPL -> do
    let r = Register8 A
    store8 r . complement =<< load8 r
    modifyFlags $ \f -> f
      & flagH .~ True
      & flagN .~ True
    return $ getTime True t

  CCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC %~ not
    return $ getTime True t

  SCF -> do
    modifyFlags $ \f -> f
      & flagH .~ False
      & flagN .~ False
      & flagC .~ True
    return $ getTime True t

  HALT -> do
    setHalt
    return $ getTime True t

  _ -> error $ "failed at " ++ show instr
