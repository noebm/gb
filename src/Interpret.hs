module Interpret where

import Data.Word
import Data.Int
import Data.Bits

import Control.Lens hiding (op, to, from)
import Control.Monad

import Text.Printf

import OpCode
import Memory.Accessible

import Instruction (byteCodeDecompose, modifyFlags)

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
  AddressFF  -> Right (addrFF <$> byte)
  AddressRel -> Right (addrRel =<< sbyte)

  -- pointers
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

  AddressFF -> Left (\b -> (`store8` b) . Addr8 . addrFF =<< byte)
  Address   -> Left (\b -> (`store8` b) . Addr8 =<< word)

  x -> error $ "setArgM: cannot write to " ++ show x
  -- ArgFlag f -> Left (load8 (Register8 F))
  -- Address    -> Right word
  -- AddressFF  -> Right (addrFF <$> byte)
  -- AddressRel -> Right (addrRel =<< sbyte)

interpretM :: MonadEmulator m => Instruction -> m ()
interpretM instr@(Instruction b op args) = case op of
  LD -> case args of
    [to , from] -> case (setArgM to, getArgM from) of
      (Left s , Left g) -> s =<< g
      (Right s , Right g) -> s =<< g
      _ -> error "interpretM: LD - cannot match type"
    [ArgPointerReg HL, ArgPointerReg SP, AddressRel] -> do
      sp <- load16 (Register16 SP)
      r <- sbyte
      store16 (Register16 HL) (addRelative sp r)
    _ -> msg
  XOR -> case getArgM <$> args of
    [ Left g ] -> do
      v <- g
      a <- load8 (Register8 A)
      store8 (Register8 A) (a `xor` v)
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
    [] -> pop >>= store16 (Register16 PC)
    _ -> msg
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
                modifyFlags $ \f -> f
                  & flagN .~ False
                  & flagH .~ (v' `xor` v) `testBit` 3
                  & flagC .~ (v' `xor` v) `testBit` 7
    _ -> msg

  _ -> error $ "failed at " ++ show instr

  where msg = error $ printf "interpretM: %s - invalid arguments" (show op)

