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

modifyFlags :: MonadEmulator m => (Word8 -> Word8) -> m ()
modifyFlags g = do
  let rF = Register8 F
  flags <- load8 rF
  store8 rF $ g flags

{-# INLINE getFlag #-}
getFlag :: Maybe Flag -> Word8 -> Bool
getFlag Nothing = \_ -> True
getFlag (Just FlagC) = view flagC
getFlag (Just FlagZ) = view flagZ
getFlag (Just FlagNC) = views flagC not
getFlag (Just FlagNZ) = views flagZ not

{-# INLINE addrFF #-}
addrFF :: Word8 -> Word16
addrFF k = (0xFF , k) ^. word16

{-# INLINE getArgM #-}
getArgM :: MonadEmulator m => Arg -> Either (m Word8) (m Word16)
getArgM arg = case arg of
  -- single byte data
  ArgDirect8 r -> Left (load8 (Register8 r))
  ArgSP        -> Right (loadSP)
  Immediate8   -> Left byte
  -- double byte data
  ArgDirect16 r -> Right (load16 $ Register16 r)
  Immediate16   -> Right word

  -- addresses
  Address    -> Right word
  -- AddressRel -> Right (addrRel =<< sbyte)
  AddressRel -> Left byte

  -- pointers
  ArgPointerImmFF   -> Left (load8 . Addr8 . addrFF =<< byte)
  ArgPointerImm8    -> Left (load8 . Addr8 =<< word)
  ArgPointerImm16   -> Right (load16 . Addr16 =<< word)
  ArgPointerRegFF r -> Left (load8 . Addr8 . addrFF =<< load8 (Register8 r))
  ArgPointerReg   r -> Left (load8 . Addr8 =<< load16 (Register16 r))
  ArgPointerHLi -> Left $ do
    hl <- load16 (Register16 HL)
    store16 (Register16 HL) (hl + 1)
    load8 (Addr8 hl)
  ArgPointerHLd -> Left $ do
    hl <- load16 (Register16 HL)
    store16 (Register16 HL) (hl - 1)
    load8 (Addr8 hl)

{-# INLINE setArgM #-}
setArgM :: MonadEmulator m => Arg -> Either (Word8 -> m ()) (Word16 -> m ())
setArgM arg = case arg of
  -- single byte data
  ArgDirect8 r -> Left (store8 (Register8 r))
  -- double byte data
  ArgDirect16 r -> Right (store16 $ Register16 r)
  ArgSP -> Right (storeSP)

  -- pointers
  ArgPointerRegFF r -> Left (\b -> (`store8` b) . Addr8 . addrFF =<< load8 (Register8 r))
  ArgPointerReg   r -> Left (\b -> (`store8` b) . Addr8 =<< load16 (Register16 r))
  ArgPointerHLi -> Left $ \b -> do
    hl <- load16 (Register16 HL)
    store16 (Register16 HL) (hl + 1)
    store8 (Addr8 hl) b
  ArgPointerHLd -> Left $ \b -> do
    hl <- load16 (Register16 HL)
    store16 (Register16 HL) (hl - 1)
    store8 (Addr8 hl) b

  ArgPointerImmFF -> Left (\b -> (`store8` b) . Addr8 . addrFF =<< byte)
  ArgPointerImm8  -> Left (\b -> (`store8` b) . Addr8 =<< word)
  ArgPointerImm16 -> Right (\w -> (`store16` w) . Addr16 =<< word)

  x -> error $ "setArgM: cannot write to " ++ show x
  -- ArgFlag f -> Left (load8 (Register8 F))
  -- Address    -> Right word
  -- AddressFF  -> Right (addrFF <$> byte)
  -- AddressRel -> Right (addrRel =<< sbyte)

class Argument a where
  getArgumentM :: MonadEmulator m => a -> Either (m Word8) (m Word16)
  setArgumentM :: MonadEmulator m => a -> Either (Word8 -> m ()) (Word16 -> m ())
  toArg :: a -> Arg

instance Argument Arg where
  setArgumentM = setArgM
  getArgumentM = getArgM
  toArg = id

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
  modifyFlags $ \k -> k
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
  LD from to
    | Left s <- setArgumentM to
    , Left g <- getArgumentM from
      -> do
        s =<< g
        return $ getTime True t
  LD16 from to
      | Right s <- setArgumentM to
      , Right g <- getArgumentM from
        -> do
      s =<< g
      return $ getTime True t
  LD16_SP_HL
    | Right sHL <- setArgumentM (ArgDirect16 HL)
    , Right gSP <- getArgumentM ArgSP
    , Left getRel <- getArgumentM Immediate8
        -> do
      sp <- gSP
      r <- fromIntegral <$> getRel
      let v = addRelative sp r
      sHL v
      store8 (Register8 F) $ 0x00
        & flagC .~ ((v .&. 0xFF) < (sp .&. 0xFF))
        & flagH .~ ((v .&. 0x0F) < (sp .&. 0x0F))
      return $ getTime True t
  AND arg
    | Left g <- getArgumentM arg -> do
      v <- g
      a <- load8 (Register8 A)
      let a' = a .&. v
      store8 (Register8 A) a'
      modifyFlags $ \_ -> 0x20
        & flagZ .~ (a' == 0)
      return $ getTime True t

  OR arg
    | Left g <- getArgumentM arg -> do
      v <- g
      a <- load8 (Register8 A)
      let a' = a .|. v
      store8 (Register8 A) a'
      modifyFlags $ \_ -> 0x00
        & flagZ .~ (a' == 0)
      return $ getTime True t

  XOR arg
    | Left g <- getArgumentM arg -> do
      v <- g
      a <- load8 (Register8 A)
      let a' = a `xor` v
      store8 (Register8 A) (a `xor` v)
      modifyFlags $ \_ -> 0x00
        & flagZ .~ (a' == 0)
      return $ getTime True t

  {- 0xCB instructions and specialization for A -}
  BIT y arg
    | Left g <- getArgumentM arg -> do
      v <- g
      modifyFlags $ \f -> f
        & flagZ .~ not (v `testBit` fromIntegral y)
        & flagN .~ False
        & flagH .~ True
      return $ getTime True t

  SWAP arg
      | Left g <- getArgumentM arg
      , Left s <- setArgumentM arg -> do
          x <- g
          let x' = ((x `shiftL` 4) .&. 0xF0) .|. ((x `shiftR` 4) .&. 0x0F)
          s x'
          modifyFlags $ \_ -> 0x00 & flagZ .~ (x' == 0)
          return $ getTime True t

  RES bidx arg
      | Left s <- setArgumentM arg
      , Left g <- getArgumentM arg -> do
          s . (`clearBit` fromIntegral bidx) =<< g
          return $ getTime True t
  SET bidx arg
      | Left s <- setArgumentM arg
      , Left g <- getArgumentM arg -> do
          s . (`setBit` fromIntegral bidx) =<< g
          return $ getTime True t

  RL arg
            | Left g <- getArgumentM arg
            , Left s <- setArgumentM arg -> do
                v <- g
                c <- view flagC <$> load8 (Register8 F)
                let (v' , c') = rotateLeft v c
                s v'
                store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
                return $ getTime True t
  RLA -> do
    v <- load8 (Register8 A)
    c <- view flagC <$> load8 (Register8 F)
    let (v' , c') = rotateLeft v c
    store8 (Register8 A) v'
    store8 (Register8 F) (0x00 & flagC .~ c')
    return $ getTime True t
  RR arg
            | Left g <- getArgumentM arg
            , Left s <- setArgumentM arg -> do
                v <- g
                c <- view flagC <$> load8 (Register8 F)
                let (v' , c') = rotateRight v c
                s v'
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
  RLC arg
      | Left g <- getArgumentM arg
      , Left s <- setArgumentM arg -> do
          v <- g
          let (v' , c') = rotateLeftCarry v
          s v'
          store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
          return $ getTime True t
  RRC arg
      | Left g <- getArgumentM arg
      , Left s <- setArgumentM arg -> do
          v <- g
          let (v' , c') = rotateRightCarry v
          s v'
          store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
          return $ getTime True t
  SRL arg
            | Left g <- getArgumentM arg
            , Left s <- setArgumentM arg -> do
                v <- g
                let v' = v `shiftR` 1
                s v'
                modifyFlags $ \_ -> 0x00
                  & flagC .~ (v `testBit` 0)
                  & flagZ .~ (v' == 0)
                return $ getTime True t
  SLA arg
      | Left g <- getArgumentM arg
      , Left s <- setArgumentM arg -> do
          v <- g
          let (v' , c') = shiftLeftArithmetic v
          s v'
          store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
          return $ getTime True t
  SRA arg
      | Left g <- getArgumentM arg
      , Left s <- setArgumentM arg -> do
          v <- g
          let (v' , c') = shiftRightArithmetic v
          s v'
          store8 (Register8 F) (0x00 & flagC .~ c' & flagZ .~ (v' == 0))
          return $ getTime True t
  JR f arg
    | Left g <- getArgumentM arg -> do
        t' <- getFlag f <$> load8 (Register8 F)
        r <- fromIntegral <$> g
        when t' $ jumpRelative r
        return $ getTime t' t
  JP f arg
    | Right g <- getArgumentM arg -> do
        t' <- getFlag f <$> load8 (Register8 F)
        addr <- g
        when t' $ storePC addr
        return $ getTime t' t
  CALL f arg
    | Right g <- getArgumentM arg -> do
        t' <- getFlag f <$> load8 (Register8 F)
        when t' . call =<< g
        return $ getTime t' t
  RET f -> do
    t' <- getFlag f <$> load8 (Register8 F)
    when t' ret
    return $ getTime t'  t
  RETI -> do
    setIEM True
    ret
    return $ getTime True t

  RST g -> do
      restart $ (* 8) g
      return $ getTime True t
  PUSH arg
    | Right g <- getArgumentM arg
     -> g >>= push >> return (getTime True t)
  POP arg
    | Right s <- setArgumentM arg -> do
      pop >>= s
      when (toArg arg == ArgDirect16 AF) (modifyFlags (.&. 0xF0))
      return $ getTime True t

  ADD arg
            | Left g <- getArgumentM arg -> arith add g False >> return (getTime True t)

  ADD16_HL from
      | Right s <- setArgumentM (ArgDirect16 HL)
      , Right gs <- getArgumentM (ArgDirect16 HL)
      , Right g  <- getArgumentM from -> do
          v <- gs
          dv <- g
          let v' = v + dv
          s v'
          modifyFlags $ \f -> f
            & flagN .~ False
            & flagC .~ (v' < v)
            & flagH .~ ((v' .&. 0x0FFF) < (v .&. 0x0FFF))
          return $ getTime True t
  ADD16_SP
      | Right s  <- setArgumentM ArgSP
      , Right gs <- getArgumentM ArgSP
      , Left getRel <- getArgumentM Immediate8 -> do
          v <- gs
          dv <- fromIntegral <$> getRel
          let v' = addRelative v dv
          s v'
          modifyFlags $ \f -> 0x00
            & flagC .~ ((v' .&. 0xFF) < (v .&. 0xFF))
            & flagH .~ ((v' .&. 0x0F) < (v .&. 0x0F))
          return $ getTime True t
  SUB arg
            | Left g <- getArgumentM arg -> arith sub g False >> return (getTime True t)
  ADC arg
            | Left g <- getArgumentM arg -> arith add g True >> return (getTime True t)
  SBC arg
            | Left g <- getArgumentM arg -> arith sub g True >> return (getTime True t)
  CP arg
            | Left g <- getArgumentM arg -> do
                k <- g
                a <- load8 (Register8 A)
                let (_, f) = sub a k False
                store8 (Register8 F) f
                return $ getTime True t
  INC arg
            | Left g <- getArgumentM arg
            , Left s <- setArgumentM arg -> do
                v <- g
                let v' = v + 1
                s v'
                modifyFlags $ \f -> f
                  & flagZ .~ (v == 0xFF)
                  & flagN .~ False
                  & flagH .~ (v .&. 0x0F == 0x0F)
                return $ getTime True t
  INC16 arg
            | Right g <- getArgumentM arg
            , Right s <- setArgumentM arg -> do
                s . (+1) =<< g
                return $ getTime True t

  DEC16 arg
            | Right g <- getArgumentM arg
            , Right s <- setArgumentM arg -> do
                s . subtract 1 =<< g
                return $ getTime True t
  DEC arg
            | Left g <- getArgumentM arg
            , Left s <- setArgumentM arg -> do
                v <- g
                let v' = v - 1
                s v'
                modifyFlags $ \f -> f
                  & flagZ .~ (v == 0x01)
                  & flagN .~ True
                  & flagH .~ (v .&. 0x0F == 0x00)
                return $ getTime True t


  DI -> setIEM False >> return (getTime True t)
  EI -> setIEM True >> return (getTime True t)
  DAA -> daa >> return (getTime True t)
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
