module Interpret where

import Data.Word
import Data.Int
import Data.Bits
import Control.Lens

import OpCode
import Memory.Accessible

getFlag :: Flag -> Word8 -> Bool
getFlag FlagC = view flagC
getFlag FlagZ = view flagZ
getFlag FlagNC = views flagC not
getFlag FlagNZ = views flagZ not

addRelative :: Word16 -> Int8 -> Word16
addRelative addr x = fromIntegral $ (fromIntegral addr :: Int) + fromIntegral x

addrFF k = (0xFF , k) ^. word16
addrRel k = do
  pc <- load16 (Register16 PC)
  return $ addRelative pc k

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

  x -> error $ "cannot write to " ++ show x
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
    _ -> error "interpretM: LD - invalid arguments"
  XOR -> case getArgM <$> args of
    [ Left g ] -> do
      v <- g
      a <- load8 (Register8 A)
      store8 (Register8 A) (a `xor` v)
    _ -> error "interpretM: XOR - invalid arguments"
  -- BIT -> case getArgM <$> args of
  --   [ Left g ] -> do
  --     let (_, y, _) = byteCodeDecompose b
  _ -> error $ "failed at " ++ show instr

