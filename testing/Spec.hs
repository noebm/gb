{-# LANGUAGE OverloadedStrings #-}

import RomTester

import Hardware.BootRom
import Hardware.Cartridge.Rom
import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString as B
import Data.List

import Control.Lens

import Test.Hspec

import System.Directory
import System.FilePath
import System.Exit

blargg :: Spec
blargg = do
  let blarggBase = "verify/gb-test-roms"

  describe "instructions" $ do
    let d = blarggBase </> "cpu_instrs/individual"
    romfps <- runIO $ filter isRomFile <$> getDirectoryContents d
    forM_ romfps $ \romfp -> do
      rom <- fmap (either error id) $ runIO $ runExceptT $ readRom $ (d </>) romfp
      it romfp $ testWithSerial rom `shouldSatisfy` (B.isInfixOf "Passed" . view strict)

  id $ do
    let fp = "instr_timing/instr_timing.gb"
    rom <- fmap (either error id) $ runIO $ runExceptT (readRom $ blarggBase </> fp)
    it (takeFileName fp) $ testWithSerial rom `shouldSatisfy` (B.isInfixOf "Passed" . view strict)

  -- let memoryTestable = [ "oam_bug/oam_bug.gb", "dmg_sound/dmg_sound.gb" ]
  -- describe "memory tests" $ forM_ memoryTestable $ \fp -> do
  --   rom <- fmap (either error id) $ runIO $ runExceptT (readRom $ blarggBase </> fp)
  --   it fp $ testWithMemory rom `shouldSatisfy` (B.isInfixOf "Passed" . view strict)
  let memoryTestable = [ "oam_bug", "dmg_sound" ]
  forM_ memoryTestable $ \testGroup -> describe testGroup $ do
    let d = blarggBase </> testGroup </> "rom_singles"
    romfps <- runIO $ filter isRomFile <$> getDirectoryContents d
    forM_ romfps $ \romfp -> do
      rom <- fmap (either error id) $ runIO $ runExceptT $ readRom $ (d </>) romfp
      it romfp $ testWithMemory rom `shouldBe` 0x00

mooneyeRequiresBoot :: FilePath -> Bool
mooneyeRequiresBoot fp = isPrefixOf "boot" $ takeBaseName fp

mooneyeDMG :: FilePath -> Bool
mooneyeDMG fp =  any ($ base) dmg || not (any ($ base) nonDmg)
  where base = takeBaseName fp
        dmg =    [ isSuffixOf ('-' : s) | s <- [ "GS", "dmgABCmgb", "dmgABC" ] ]
        nonDmg = [ isSuffixOf ('-' : s) | s <- [ "S", "mgb", "A", "C", "dmg0", "sgb", "sgb2" ] ]

isRomFile :: FilePath -> Bool
isRomFile = isExtensionOf "gb"

mooneyeTestGroup :: Maybe BootRom -> FilePath -> FilePath -> Spec
mooneyeTestGroup brom base subdir = describe subdir $ do
  let d = base </> subdir
  let f = maybe not (const id) brom
  romfps <- runIO $ filter (\r -> isRomFile r && mooneyeDMG r && f (mooneyeRequiresBoot r))
        <$> getDirectoryContents d
  forM_ romfps $ \romfp -> do
    rom <- fmap (either error id) $ runIO $ runExceptT $ readRom $ (d </>) romfp
    it romfp $ testWithMooneye brom rom `shouldBe` True

mooneye :: Spec
mooneye = do
  let mooneyeBase = "verify/mooneye"
  let nonBootGroups = mooneyeTestGroup Nothing mooneyeBase
  mapM_ nonBootGroups
    [ "emulator-only/mbc1"
    , "acceptance/timer"
    , "acceptance/bits"
    , "acceptance/instr"
    , "acceptance/interrupts"
    , "acceptance/serial"
    , "acceptance/oam_dma"
    ]
  -- use nonstandard return codes for non 'boot_' roms
  -- mooneyeTestGroup mooneyeBase "acceptance"

  brom <- runIO $ fmap (either error id) $ runExceptT $ readBootRom "DMG_ROM.bin"
  mapM_ (mooneyeTestGroup (Just brom) mooneyeBase) [ "acceptance" ]

main :: IO ()
main = hspec $ do
  describe "blargg roms" blargg
  describe "mooneye roms" mooneye

