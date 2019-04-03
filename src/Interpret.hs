module Interpret where

import Data.Word
import Data.Int
import Data.Bits
import Data.Bits.Lens (bitAt)

import Control.Lens hiding (op, to, from)
import Control.Monad

import Text.Printf

import OpCode
import Memory.Accessible

import Instruction (byteCodeDecompose, modifyFlags)

{-# INLINE isControlStatement #-}
isControlStatement :: Instruction -> Bool
isControlStatement (Instruction _ op _) = op `elem`
  [ JP, JR, CALL, RET, RETI, RST, STOP, HALT ]

{-# INLINE getFlag #-}
getFlag :: Flag -> Word8 -> Bool
getFlag FlagC = view flagC
getFlag FlagZ = view flagZ
getFlag FlagNC = views flagC not
getFlag FlagNZ = views flagZ not

{-# INLINE addRelative #-}
addRelative :: Word16 -> Int8 -> Word16
addRelative addr x = fromIntegral $ (fromIntegral addr :: Int) + fromIntegral x

{-# INLINE addrFF #-}
addrFF :: Word8 -> Word16
addrFF k = (0xFF , k) ^. word16
{-# INLINE addrRel #-}
addrRel :: MonadEmulator m => Int8 -> m Word16
addrRel k = do
  pc <- load16 (Register16 PC)
  return $ addRelative pc k

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
  AddressRel -> Right (addrRel =<< sbyte)

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

interpretM :: MonadEmulator m => Instruction -> m ()
interpretM instr@(Instruction b op args) = case op of
  NOP -> return ()
  LD -> case args of
    [to , from] -> case (setArgM to, getArgM from) of
      (Left s , Left g) -> s =<< g
      (Right s , Right g) -> s =<< g
      _ -> error $ printf "interpretM: %s - cannot match type" (show instr)
    [ArgDirect16 HL, ArgPointerReg SP, AddressRel] -> do
      sp <- load16 (Register16 SP)
      r <- sbyte
      let v = addRelative sp r
      store16 (Register16 HL) v
      modifyFlags $ \_ -> if r >= 0
        then 0x00
        & flagC .~ ((v .&. 0xFF) < (sp .&. 0xFF))
        & flagH .~ ((v .&. 0x0F) < (sp .&. 0x0F))
        else 0x00
        & flagC .~ ((v .&. 0xFF) >= (sp .&. 0xFF))
        & flagH .~ ((v .&. 0x0F) >= (sp .&. 0x0F))
    _ -> msg

  AND -> case getArgM <$> args of
    [ Left g ] -> do
      v <- g
      a <- load8 (Register8 A)
      let a' = a .&. v
      store8 (Register8 A) a'
      modifyFlags $ \_ -> 0x20
        & flagZ .~ (a' == 0)
    _ -> msg

  OR -> case getArgM <$> args of
    [ Left g ] -> do
      v <- g
      a <- load8 (Register8 A)
      let a' = a .|. v
      store8 (Register8 A) a'
      modifyFlags $ \_ -> 0x00
        & flagZ .~ (a' == 0)
    _ -> msg

  XOR -> case getArgM <$> args of
    [ Left g ] -> do
      v <- g
      a <- load8 (Register8 A)
      let a' = a `xor` v
      store8 (Register8 A) (a `xor` v)
      modifyFlags $ \_ -> 0x00
        & flagZ .~ (a' == 0)
    _ -> msg

  BIT -> case getArgM <$> args of
    [ Left g ] -> do
      let (_, y, _) = byteCodeDecompose b
      v <- g
      modifyFlags $ \f -> f
        & flagZ .~ not (v `testBit` fromIntegral y)
        & flagN .~ False
        & flagH .~ True
    _ -> msg
  JR -> case args of
    [ arg ]
      | Right g <- getArgM arg ->
          g >>= store16 (Register16 PC)
    [ ArgFlag f , arg ]
      | Right g <- getArgM arg -> do
          t <- getFlag f <$> load8 (Register8 F)
          addr <- g
          when t $ store16 (Register16 PC) addr
    _ -> msg
  JP -> case args of
    [ arg ]
      | Right g <- getArgM arg ->
          g >>= store16 (Register16 PC)
    [ ArgFlag f , arg ]
      | Right g <- getArgM arg -> do
          t <- getFlag f <$> load8 (Register8 F)
          addr <- g
          when t $ store16 (Register16 PC) addr
    _ -> msg
  CALL -> case args of
    [ arg ]
      | Right g <- getArgM arg -> do
          push =<< load16 (Register16 PC)
          g >>= store16 (Register16 PC)
    [ ArgFlag f , arg ]
      | Right g <- getArgM arg -> do
          t <- getFlag f <$> load8 (Register8 F)
          addr <- g
          when t $ do
            push =<< load16 (Register16 PC)
            store16 (Register16 PC) addr
    _ -> msg
  RET -> case args of
    [ ArgFlag f ] -> do
          t <- getFlag f <$> load8 (Register8 F)
          when t $ pop >>= store16 (Register16 PC)
    [] -> pop >>= store16 (Register16 PC)
    _ -> msg
  RST -> do
    load16 (Register16 PC) >>= push
    store16 (Register16 PC) $ fromIntegral $ (b .&. 7) * 8
  PUSH -> case getArgM <$> args of
    [ Right g ] -> g >>= push
    _ -> msg
  POP -> case setArgM <$> args of
    [ Right s ] -> pop >>= s
    _ -> msg

  INC -> case args of
    [ arg ] | Right g <- getArgM arg
            , Right s <- setArgM arg -> s . (+1) =<< g
            | Left g <- getArgM arg
            , Left s <- setArgM arg -> do
                v <- g
                let v' = v + 1
                s v'
                addFlags v v'
    _ -> msg
  ADD -> case args of
    [ arg ] | Left g <- getArgM arg -> do
                k <- g
                v <- load8 (Register8 A)
                let v' = v + k
                store8 (Register8 A) v'
                addFlags v v'
    [ to@(ArgDirect16 HL) , from ]
      | Right s  <- setArgM to
      , Right gs <- getArgM to
      , Right g  <- getArgM from -> do
          v <- gs
          dv <- g
          let v' = v + dv
          s v'
          modifyFlags $ \f -> f
            & flagN .~ False
            & flagC .~ ((v' .&. 0xFF) < (v .&. 0xFF))
            & flagH .~ ((v' .&. 0x0F) < (v .&. 0x0F))
    _ -> msg
  SUB -> case args of
    [ arg ] | Left g <- getArgM arg -> do
                k <- g
                v <- load8 (Register8 A)
                let v' = v - k
                store8 (Register8 A) v'
                subFlags v v'
    _ -> msg
  ADC -> case args of
    [ arg ] | Left g <- getArgM arg -> do
                k <- g
                v <- load8 (Register8 A)
                c <- fromIntegral . fromEnum . view flagC <$> load8 (Register8 F)
                let v' = v + k + c
                store8 (Register8 A) v'
                addFlags v v'
    _ -> msg
  SBC -> case args of
    [ arg ] | Left g <- getArgM arg -> do
                k <- g
                v <- load8 (Register8 A)
                c <- fromIntegral . fromEnum . view flagC <$> load8 (Register8 F)
                let v' = v - k - c
                store8 (Register8 A) v'
                subFlags v v'
    _ -> msg
  CP -> case args of
    [ arg ] | Left g <- getArgM arg -> do
                k <- g
                v <- load8 (Register8 A)
                let v' = v - k
                subFlags v v'
    _ -> msg

  DEC -> case args of
    [ arg ] | Right g <- getArgM arg
            , Right s <- setArgM arg -> s . subtract 1 =<< g
            | Left g <- getArgM arg
            , Left s <- setArgM arg -> do
                v <- g
                let v' = v - 1
                s v'
                subFlags v v'
    _ -> msg


  RL -> case args of
    [ arg ] | Left g <- getArgM arg
            , Left s <- setArgM arg -> do
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
    [ arg ] | Left g <- getArgM arg
            , Left s <- setArgM arg -> do
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

  _ -> error $ "failed at " ++ show instr

  where msg = error $ printf "interpretM: %s - invalid arguments %s" (show op) (show args)
