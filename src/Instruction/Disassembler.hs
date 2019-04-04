module Instruction.Disassembler where

import Control.Monad.State

import MonadEmulator
import Instruction.Interpret
import Instruction.Instruction
import Text.Printf

import Data.Foldable
import Data.Traversable

import Data.List
import Data.Maybe
import Data.Word
import Data.Int

data ArgData = ArgByte Word8 | ArgWord Word16

argData :: MonadEmulator m => Arg -> m (Maybe ArgData)
argData arg = case argSize arg of
  0 -> return Nothing
  1 -> Just . ArgByte <$> byte
  2 -> Just . ArgWord <$> word
  _ -> error "impossible argsize"

data ArgWithData = ArgWithData
  { removeArgData :: Arg
  , argumentData :: Maybe ArgData
  }

instance Argument ArgWithData where
  getArgumentM (ArgWithData t d)
    | Just v <- d = case v of
        ArgByte b -> case t of
          Immediate8      -> Left (return b)
          ArgPointerImmFF -> Left (load8 . Addr8 $ addrFF b)
          AddressRel      -> Left (return b)
          _ -> error $ printf "ArgWithData.getArgumentM: %s has data when it shouldn't" (show t)
        ArgWord w -> case t of
          Immediate16     -> Right (return w)
          Address         -> Right (return w)
          ArgPointerImm8  -> Left  (load8 (Addr8 w))
          ArgPointerImm16 -> Right (load16 (Addr16 w))
          _ -> error $ printf "ArgWithData.getArgumentM: %s has data when it shouldn't" (show t)
    | otherwise = getArgumentM t
  setArgumentM (ArgWithData t d)
    | Just (ArgWord w) <- d , ArgPointerImm8  <- t = Left  (\b -> (`store8` b) $ Addr8 w)
    | Just (ArgByte b) <- d , ArgPointerImmFF <- t = Left (\b' -> (`store8` b') $ Addr8 $ addrFF b)
    | Just (ArgWord w) <- d , ArgPointerImm16 <- t = Right (\b -> (`store16` b) $ Addr16 w)
    | otherwise  = setArgumentM t
  toArg = removeArgData

data DisassembledInstruction = DisassembledInstruction
  { address :: Word16
  , instruction :: Instruction ArgWithData
  }

-- whatHappened :: MonadEmulator m => DisassembledInstruction -> m String
-- whatHappened (DisassembledInstruction addr instr) = 

disassemble :: MonadEmulator m => m (Instruction ArgWithData)
disassemble = do
  i <- parseInstructionM byte
  mapM (\arg -> ArgWithData arg <$> argData arg) i

hasTargetAddress :: [ ArgWithData ] -> Word16 -> Maybe Word16
hasTargetAddress args addr = case find (isAddress . removeArgData) args of
  Just (ArgWithData Address    (Just (ArgWord addr'))) -> Just addr'
  Just (ArgWithData AddressRel (Just (ArgByte rel  ))) -> Just $ addRelative addr (fromIntegral rel)
  _ -> Nothing
  where
    isAddress Address = True
    isAddress AddressRel = True
    isAddress _ = False

isConditional :: Arg -> Bool
isConditional (ArgFlag _) = True
isConditional _ = False

isCall :: Instruction a -> Bool
isCall (Instruction _ CALL _) = True
isCall _ = False

runDisassembler :: MonadEmulator m => (Word16 -> Bool) -> m [ DisassembledInstruction ]
runDisassembler stopPlease
  = fmap (sortBy (\x y -> compare (address x) (address y)))
  . (`execStateT` []) $ do
  let parse = do
        addr <- load16 (Register16 PC)
        x <- gets (find ((== addr) . address))
        unless (isJust x || stopPlease addr) $ do
          instr <- disassemble
          modify' (DisassembledInstruction addr instr:)
          if isControlStatement instr
            then do
            -- a conditional control statement guards additional code
            addr' <- load16 (Register16 PC)
            when (any (isConditional . removeArgData) instr || isCall instr) parse
            -- after finding the remaining code jump to next address
            mapM_ (const parse <=< store16 (Register16 PC)) (hasTargetAddress (toList instr) addr')
            else
            parse

  let aux x = store16 (Register16 PC) x >> parse
  -- begin at usual program entry point
  aux 0x100
  -- walk through all interrupt addresses
  for [0..12] $ \addr -> aux (addr * 8)

instance Show DisassembledInstruction where
  show (DisassembledInstruction addr instr) = printf "0x%04x: %s" addr (show instr)

instance Show ArgWithData where
  show (ArgWithData t Nothing) = show t
  show (ArgWithData t (Just d))
    | ArgByte b <- d = case t of
        Immediate8 -> printf "0x%02x" b
        AddressRel -> printf "%d" (fromIntegral b :: Int8)
        ArgPointerImmFF -> printf "(0xFF%02x)" b
        _ -> error $ printf "%s has byte data" (show t)

    | ArgWord w <- d = case t of
        Immediate16 -> printf "0x%04x" w
        Address     -> printf "0x%04x" w
        ArgPointerImm8  -> printf "(0x%04x)" w
        ArgPointerImm16 -> printf "(0x%04x)" w
        _ -> error $ printf "%s has word data" (show t)

-- instance Show DisassembledInstruction where
--   show (DisassembledInstruction addr mne args)
--     = printf "0x%04x: %s %s" addr (show mne) (showArgStructure $ disassembleArg <$> args)
--     where

