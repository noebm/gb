module Instruction.Interpret where

import Data.Word
import Data.Int
import Data.Bits
import Data.Bits.Lens (bitAt)

import Control.Lens hiding (op, to, from)
import Control.Monad

import Text.Printf

import Instruction.Instruction
import Memory.Accessible

import Instruction (byteCodeDecompose, modifyFlags)

{-# INLINE isControlStatement #-}
isControlStatement :: Instruction a -> Bool
isControlStatement (Instruction _ op _) = op `elem`
  [ JP, JR, CALL, RET, RETI, RST, STOP, HALT ]

{-# INLINE getFlag #-}
getFlag :: Flag -> Word8 -> Bool
getFlag FlagC = view flagC
getFlag FlagZ = view flagZ
getFlag FlagNC = views flagC not
getFlag FlagNZ = views flagZ not


{-# INLINE addrFF #-}
addrFF :: Word8 -> Word16
addrFF k = (0xFF , k) ^. word16

-- {-# INLINE addrRel #-}
-- addrRel :: MonadEmulator m => Int8 -> m Word16
-- addrRel k = do
--   pc <- load16 (Register16 PC)
--   return $ addRelative pc k

{-# INLINE getArgM #-}
getArgM :: MonadEmulator m => Arg -> Either (m Word8) (m Word16)
getArgM arg = case arg of
  -- single byte data
  ArgDirect8 r -> Left (load8 (Register8 r))
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
  ArgFlag _ -> Left (load8 (Register8 F))

  ArgByteCode b -> Left (return b)

{-# INLINE setArgM #-}
setArgM :: MonadEmulator m => Arg -> Either (Word8 -> m ()) (Word16 -> m ())
setArgM arg = case arg of
  -- single byte data
  ArgDirect8 r -> Left (store8 (Register8 r))
  -- double byte data
  ArgDirect16 r -> Right (store16 $ Register16 r)

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

addFlags :: MonadEmulator m => Word8 -> Word8 -> m ()
addFlags vold vnew = modifyFlags $ \f -> f
  & flagC .~ (vnew < vold)                 -- register overflows
  & flagZ .~ (vnew == 0)                   -- new value is zero
  & flagN .~ False                         -- not a subtraction
  & flagH .~ (vnew .&. 0xF < vold .&. 0xF) -- lower half of register overflows

subFlags :: MonadEmulator m => Word8 -> Word8 -> m ()
subFlags vold vnew = modifyFlags $ \f -> f
  & flagC .~ (vnew > vold)                 -- register underflows
  & flagZ .~ (vnew == 0)                   -- new value is zero
  & flagN .~ True                          -- is a subtraction
  & flagH .~ (vnew .&. 0xF > vold .&. 0xF) -- lower half of register underflows

daa :: MonadEmulator m => m ()
daa = do
  f <- load8 (Register8 F)
  v <- load8 (Register8 A)
  let vcorr'
        | f ^. flagN
        = if f ^. flagC then 0x60 else 0x00
        + if f ^. flagH then 0x06 else 0x00
        | otherwise
        = if f ^. flagC || v > 0x99 then 0x60 else 0x00
        + if f ^. flagH || v > 0x09 then 0x06 else 0x00
  let v' = if f ^. flagN then v - vcorr' else v + vcorr'
  store8 (Register8 A) v'
  modifyFlags $ \k -> k
    & flagH .~ False
    & flagC .~ (f ^. flagC || (not (f ^. flagN) && v > 0x99))
    & flagZ .~ (v' == 0)

interpretM :: (MonadEmulator m, Argument a, Show a) => Instruction a -> m ()
interpretM instr@(Instruction b op args) = case op of
  NOP -> return ()
  LD -> case args of
    [to , from] -> case (setArgumentM to, getArgumentM from) of
      (Left s , Left g) -> s =<< g
      (Right s , Right g) -> s =<< g
      _ -> error $ printf "interpretM: %s - cannot match type" (show instr)
    l@[ rHL, rSP, ptrRel ]
      | [ ArgDirect16 HL, ArgDirect16 SP, Immediate8 ] <- toArg <$> l
      , Right sHL <- setArgumentM rHL
      , Right gSP <- getArgumentM rSP
      , Left  getRel <- getArgumentM ptrRel
        -> do
      sp <- gSP
      r <- fromIntegral <$> getRel
      let v = addRelative sp r
      sHL v
      modifyFlags $ \_ -> if r >= 0
        then 0x00
        & flagC .~ ((v .&. 0xFF) < (sp .&. 0xFF))
        & flagH .~ ((v .&. 0x0F) < (sp .&. 0x0F))
        else 0x00
        & flagC .~ ((v .&. 0xFF) >= (sp .&. 0xFF))
        & flagH .~ ((v .&. 0x0F) >= (sp .&. 0x0F))
    _ -> msg

  AND -> case getArgumentM <$> args of
    [ Left g ] -> do
      v <- g
      a <- load8 (Register8 A)
      let a' = a .&. v
      store8 (Register8 A) a'
      modifyFlags $ \_ -> 0x20
        & flagZ .~ (a' == 0)
    _ -> msg

  OR -> case getArgumentM <$> args of
    [ Left g ] -> do
      v <- g
      a <- load8 (Register8 A)
      let a' = a .|. v
      store8 (Register8 A) a'
      modifyFlags $ \_ -> 0x00
        & flagZ .~ (a' == 0)
    _ -> msg

  XOR -> case getArgumentM <$> args of
    [ Left g ] -> do
      v <- g
      a <- load8 (Register8 A)
      let a' = a `xor` v
      store8 (Register8 A) (a `xor` v)
      modifyFlags $ \_ -> 0x00
        & flagZ .~ (a' == 0)
    _ -> msg

  BIT -> case getArgumentM <$> args of
    [ Left gb, Left g ] -> do
      y <- gb
      v <- g
      modifyFlags $ \f -> f
        & flagZ .~ not (v `testBit` fromIntegral y)
        & flagN .~ False
        & flagH .~ True
    _ -> msg
  JR -> case args of
    [ arg ]
      | Left g <- getArgumentM arg -> do
          r <- fromIntegral <$> g
          jumpRelative r
    [ argf , arg ]
      | Left g <- getArgumentM arg
      , ArgFlag f <- toArg argf -> do
          t <- getFlag f <$> load8 (Register8 F)
          r <- fromIntegral <$> g
          when t $ jumpRelative r
    _ -> msg
  JP -> case args of
    [ arg ]
      | Right g <- getArgumentM arg ->
          g >>= store16 (Register16 PC)
    [ argf , arg ]
      | Right g <- getArgumentM arg
      , ArgFlag f <- toArg argf -> do
          t <- getFlag f <$> load8 (Register8 F)
          addr <- g
          when t $ store16 (Register16 PC) addr
    _ -> msg
  CALL -> case args of
    [ arg ]
      | Right g <- getArgumentM arg -> do
          push =<< load16 (Register16 PC)
          g >>= store16 (Register16 PC)
    [ argf , arg ]
      | Right g <- getArgumentM arg
      , ArgFlag f <- toArg argf -> do
          t <- getFlag f <$> load8 (Register8 F)
          addr <- g
          when t $ do
            push =<< load16 (Register16 PC)
            store16 (Register16 PC) addr
    _ -> msg
  RET -> case toArg <$> args of
    [ ArgFlag f ] -> do
          t <- getFlag f <$> load8 (Register8 F)
          when t $ pop >>= store16 (Register16 PC)
    [] -> pop >>= store16 (Register16 PC)
    _ -> msg
  RST -> case getArgumentM <$> args of
    [ Left g ] -> do
      restart . (* 8) =<< g
    _ -> msg
  PUSH -> case getArgumentM <$> args of
    [ Right g ] -> g >>= push
    _ -> msg
  POP -> case setArgumentM <$> args of
    [ Right s ] -> pop >>= s
    _ -> msg

  INC -> case args of
    [ arg ] | Right g <- getArgumentM arg
            , Right s <- setArgumentM arg -> s . (+1) =<< g
            | Left g <- getArgumentM arg
            , Left s <- setArgumentM arg -> do
                v <- g
                let v' = v + 1
                s v'
                addFlags v v'
    _ -> msg
  ADD -> case args of
    [ arg ] | Left g <- getArgumentM arg -> do
                k <- g
                v <- load8 (Register8 A)
                let v' = v + k
                store8 (Register8 A) v'
                addFlags v v'
    [ to , from ]
      | ArgDirect16 HL <- toArg to
      , Right s  <- setArgumentM to
      , Right gs <- getArgumentM to
      , Right g  <- getArgumentM from -> do
          v <- gs
          dv <- g
          let v' = v + dv
          s v'
          modifyFlags $ \f -> f
            & flagN .~ False
            & flagC .~ ((v' .&. 0xFF) < (v .&. 0xFF))
            & flagH .~ ((v' .&. 0x0F) < (v .&. 0x0F))
      | ArgDirect16 SP <- toArg to
      , Right s <- setArgumentM to
      , Right gs <- getArgumentM to
      , Left getRel <- getArgumentM from -> do
          v <- gs
          dv <- fromIntegral <$> getRel
          let v' = addRelative v dv
          s v'
          modifyFlags $ \f -> f
            & flagZ .~ False
            & flagN .~ False
            & flagC .~ ((v' .&. 0xFF) < (v .&. 0xFF))
            & flagH .~ ((v' .&. 0x0F) < (v .&. 0x0F))
    _ -> msg
  SUB -> case args of
    [ arg ] | Left g <- getArgumentM arg -> do
                k <- g
                v <- load8 (Register8 A)
                let v' = v - k
                store8 (Register8 A) v'
                subFlags v v'
    _ -> msg
  ADC -> case args of
    [ arg ] | Left g <- getArgumentM arg -> do
                k <- g
                v <- load8 (Register8 A)
                c <- fromIntegral . fromEnum . view flagC <$> load8 (Register8 F)
                let v' = v + k + c
                store8 (Register8 A) v'
                addFlags v v'
    _ -> msg
  SBC -> case args of
    [ arg ] | Left g <- getArgumentM arg -> do
                k <- g
                v <- load8 (Register8 A)
                c <- fromIntegral . fromEnum . view flagC <$> load8 (Register8 F)
                let v' = v - k - c
                store8 (Register8 A) v'
                subFlags v v'
    _ -> msg
  CP -> case args of
    [ arg ] | Left g <- getArgumentM arg -> do
                k <- g
                v <- load8 (Register8 A)
                let v' = v - k
                subFlags v v'
    _ -> msg

  DEC -> case args of
    [ arg ] | Right g <- getArgumentM arg
            , Right s <- setArgumentM arg -> s . subtract 1 =<< g
            | Left g <- getArgumentM arg
            , Left s <- setArgumentM arg -> do
                v <- g
                let v' = v - 1
                s v'
                subFlags v v'
    _ -> msg


  RL -> case args of
    [ arg ] | Left g <- getArgumentM arg
            , Left s <- setArgumentM arg -> do
                v <- g
                let v' = v `rotateL` 1
                let c' = v' `testBit` 0
                f <- load8 (Register8 F)
                let c = f ^. flagC
                s (v' & bitAt 0 .~ c)
                store8 (Register8 F) (f & flagC .~ c')
    _ -> msg
  RLA -> do
    v <- load8 (Register8 A)
    let v' = v `rotateL` 1
    let c' = v' `testBit` 0
    f <- load8 (Register8 F)
    let c = f ^. flagC
    store8 (Register8 A) (v' & bitAt 0 .~ c)
    store8 (Register8 F) (f & flagC .~ c')

  RR -> case args of
    [ arg ] | Left g <- getArgumentM arg
            , Left s <- setArgumentM arg -> do
                v <- g
                let v' = v `rotateR` 1
                let c' = v' `testBit` 7
                f <- load8 (Register8 F)
                let c = f ^. flagC
                s (v' & bitAt 7 .~ c)
                store8 (Register8 F) (f & flagC .~ c')
    _ -> msg
  RRA -> do
    v <- load8 (Register8 A)
    let v' = v `rotateR` 1
    let c' = v' `testBit` 7
    f <- load8 (Register8 F)
    let c = f ^. flagC
    store8 (Register8 A) (v' & bitAt 7 .~ c)
    store8 (Register8 F) (f & flagC .~ c')

  DI -> setIEM False
  EI -> setIEM True
  DAA -> daa
  CPL -> do
    let r = Register8 A
    store8 r . complement =<< load8 r
    modifyFlags $ \f -> f
      & flagH .~ True
      & flagN .~ True
  CCF -> modifyFlags $ \f -> f
    & flagH .~ False
    & flagN .~ False
    & flagC %~ not

  SCF -> modifyFlags $ \f -> f
    & flagH .~ False
    & flagN .~ False
    & flagC .~ True

  _ -> error $ "failed at " ++ show instr

  where msg = error $ printf "interpretM: %s - invalid arguments %s" (show op) (show args)
