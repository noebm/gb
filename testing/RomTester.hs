module RomTester where

import MonadEmulator
import Hardware.HardwareMonad
import Instruction.Instruction
import Instruction.Interpreter
import Instruction.Types.Address

import Control.Lens hiding ((:<))
import Control.Monad
import Control.Monad.Trans

import System.IO.Unsafe

import Data.Functor
import Data.Word
import Data.IORef
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as B

-- detect instruction loops
isLooping :: Word16 -> Instruction' -> Bool
isLooping addr instr = hasn't flag instr && instr ^. expr . to isLoopOp where
  isLoopOp (JR offset) = offset == -2
  isLoopOp (JP (AddrDirect target)) = target == addr
  isLoopOp _ = False

isByteSequenceAt :: MonadEmulator m => [ Word8 ] -> Word16 -> m Bool
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
    b <- loadAddr addr
    xs <- if b /= 0 then go (addr + 1) else return []
    return $! b : xs

getMemoryTestValue :: MonadEmulator m => m (Maybe Word8)
getMemoryTestValue = do
  valid <- [ 0xde, 0xb0, 0x61 ] `isByteSequenceAt` 0xa001
  sequenceA $ guard valid $> loadAddr 0xa000

coExtend :: Monad m => (a -> m b) -> Cofree m a -> m (Cofree m b)
coExtend f (x :< s) = (:< (coExtend f =<< s)) <$> f x

coFilter :: Monad m => Cofree m (Maybe a) -> m (Cofree m a)
coFilter (x :< s) = maybe id (\x -> return . (x :<)) x $ coFilter =<< s

-- only output instructions .. continue on halt, etc.
newInstr :: (MonadEmulator m, HardwareMonad m)
         => Maybe (Word8 -> m Word8)
         -> m (Cofree m (Word16 , Instruction'))
newInstr conn = instructionsTrace
  >>= coExtend (\(dt, tr) -> tickHardware conn dt $> tr)
  >>= coFilter

testWithMemory :: Rom -> Word8
testWithMemory rom = unsafePerformIO $ runEmulatorT conf $ do
  storePC 0x100
  newInstr Nothing
    >>= coExtend (const getMemoryTestValue)
    >>= findStart >>= coFilter >>= findEnd
  where
    conf = EmulatorConfig Nothing rom

    findStart (Just 0x80 :< x) = x
    findStart (_ :< x) = findStart =<< x

    findEnd (0x80 :< xs) = findEnd =<< xs
    findEnd (x :< xs) = return x

getMooneyeRet :: MonadEmulator m => m Bool
getMooneyeRet = go regValues where
  regValues = [(B, 3), (C, 5), (D, 8), (E, 13), (H, 21), (L, 34)]
  go = fmap and . mapM (\(r, v) -> (== v) <$> loadReg r)

testWithMooneye :: Maybe BootRom -> Rom -> Bool
testWithMooneye brom rom = unsafePerformIO $ runEmulatorT conf $ do
  maybe (storePC 0x100) (const (return ())) brom
  go =<< newInstr Nothing
  getMooneyeRet

  where
    conf = EmulatorConfig brom rom

    go ((addr, _) :< s) = do
      f <- [ 0x00, 0x18, 0xfd ] `isByteSequenceAt` addr
      unless f $ go =<< s
