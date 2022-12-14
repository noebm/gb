module RomTester where

import           Hardware.HardwareMonad
import           Instruction.Instruction
import           Instruction.Interpreter
import           Instruction.Types.Address
import           Instruction.Types.Readable
import           Instruction.Types.Writable
import           MonadEmulator
import           Utilities.Cofree

import           Control.Lens            hiding ( (:<) )
import           Control.Monad
import           Control.Monad.Trans

import           System.IO.Unsafe

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as B
import           Data.Functor
import           Data.IORef
import           Data.Word

-- detect instruction loops
isLooping :: Word16 -> Instruction' -> Bool
isLooping addr instr = hasn't flag instr && instr ^. expr . to isLoopOp where
  isLoopOp (JR offset             ) = offset == -2
  isLoopOp (JP (AddrDirect target)) = target == addr
  isLoopOp _                        = False

isByteSequenceAt :: MonadEmulator m => [Word8] -> Word16 -> m Bool
isByteSequenceAt (s0 : seq) addr = do
  b <- loadAddr addr
  if b == s0 then isByteSequenceAt seq (addr + 1) else return False
isByteSequenceAt [] addr = return True

-- unsafePerformIO used since specialize for interpreter are only defined for IO
-- and runtime performance depends on these costly specialize pragmas
-- in the future one could add variants for ST
testWithSerial :: Rom -> B.ByteString
testWithSerial rom = toLazyByteString $ unsafePerformIO $ do
  let conf = EmulatorConfig Nothing rom
  output <- newIORef mempty
  let conn = Just $ \b -> lift (modifyIORef' output (<> word8 b)) $> 0
  let go (instr :< s) = unless (uncurry isLooping instr) $ go =<< s
  runEmulatorT conf $ storePC 0x100 >> newInstr conn >>= go
  readIORef output

-- text output is appended to a zero terminated string at 0xa004
getMemoryString :: MonadEmulator m => m B.ByteString
getMemoryString = B.pack <$> go 0xa004 where
  go addr = do
    b  <- loadAddr addr
    xs <- if b /= 0 then go (addr + 1) else return []
    return $! b : xs

getMemoryTestValue :: MonadEmulator m => m (Maybe Word8)
getMemoryTestValue = do
  valid <- [0xde, 0xb0, 0x61] `isByteSequenceAt` 0xa001
  sequenceA $ guard valid $> loadAddr 0xa000

-- only output instructions .. continue on halt, etc.
newInstr
  :: (MonadEmulator m, HardwareMonad m)
  => Maybe (Word8 -> m Word8)
  -> m (Cofree m (Word16, Instruction'))
newInstr conn =
  instructionsTrace
    >>= traverseCofree (\(dt, tr) -> tickHardware conn dt $> tr)
    >>= catMaybes

waitFor :: Monad m => (a -> Bool) -> Cofree m a -> m (Cofree m a)
waitFor f (x :< xs) = if f x then return (x :< xs) else waitFor f =<< xs

testWithMemory :: Rom -> Word8
testWithMemory rom = unsafePerformIO $ runEmulatorT conf $ do
  storePC 0x100
  newInstr Nothing
    >>= traverseCofree (const getMemoryTestValue)
    >>= findStart
    >>= catMaybes
    >>= findEnd
    >>= return
    .   view _extract
 where
  conf      = EmulatorConfig Nothing rom

  findStart = waitFor (== Just 0x80)
  findEnd   = waitFor (/= 0x80)

data MooneyeErrorCode
  -- Magic in registers except A signals success / failure
  = MooneyeMagicError Word8
  -- D value signals success / failure
  | MooneyeError42
  deriving (Eq, Show)

-- check for magic value
-- if it fails check for value 0x42 / 0x00 in D register
getMooneyeRet :: MonadEmulator m => m (Maybe MooneyeErrorCode)
getMooneyeRet = do
  hasMagic <- go regValues
  d        <- loadReg D
  a        <- loadReg A
  return $ if hasMagic
    then Nothing
    else do
      case d of
        0x42 -> Just MooneyeError42
        0x00 -> Nothing
        _    -> Just $ MooneyeMagicError a
 where
  regValues = [(B, 3), (C, 5), (D, 8), (E, 13), (H, 21), (L, 34)]
  go        = fmap and . mapM (\(r, v) -> (== v) <$> loadReg r)

testWithMooneye :: Maybe BootRom -> Rom -> Maybe MooneyeErrorCode
testWithMooneye brom rom = unsafePerformIO $ runEmulatorT conf $ do
  maybe (storePC 0x100) (const (return ())) brom
  go =<< newInstr Nothing
  getMooneyeRet
 where
  conf    = EmulatorConfig brom rom

  magicOp = LD (ReadReg8 B) (WriteReg8 B)
  go      = waitFor $ has (_2 . expr . only magicOp)
