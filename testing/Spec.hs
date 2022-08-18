{-# LANGUAGE OverloadedStrings #-}

import           RomTester

import           Control.Monad.Except
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.List
import           Hardware.BootRom
import           Hardware.Cartridge.Rom

import           Control.Lens

import           Test.Hspec

import           System.Directory
import           System.Exit
import           System.FilePath

blargg :: Spec
blargg = do
  let blarggBase = "verify/gb-test-roms"

  describe "instructions" $ do
    let d = blarggBase </> "cpu_instrs/individual"
    romfps <- runIO $ getRomsInDir d
    forM_ romfps $ \romfp -> do
      rom <- runIO $ getRomFile False $ d </> romfp
      it romfp
        $               testWithSerial rom
        `shouldSatisfy` (B.isInfixOf "Passed" . view strict)

  do
    let fp = "instr_timing/instr_timing.gb"
    rom <- runIO $ getRomFile False $ blarggBase </> fp
    it (takeFileName fp)
      $               testWithSerial rom
      `shouldSatisfy` (B.isInfixOf "Passed" . view strict)

  -- let memoryTestable = [ "oam_bug/oam_bug.gb", "dmg_sound/dmg_sound.gb" ]
  -- describe "memory tests" $ forM_ memoryTestable $ \fp -> do
  --   rom <- fmap (either error id) $ runIO $ runExceptT (readRom False $ blarggBase </> fp)
  --   it fp $ testWithMemory rom `shouldSatisfy` (B.isInfixOf "Passed" . view strict)
  let memoryTestable = ["oam_bug", "dmg_sound"]
  forM_ memoryTestable $ \testGroup -> describe testGroup $ do
    let d = blarggBase </> testGroup </> "rom_singles"
    romfps <- runIO $ getRomsInDir d
    forM_ romfps $ \romfp -> do
      rom <- runIO $ getRomFile False $ d </> romfp
      it romfp $ testWithMemory rom `shouldBe` 0x00

mooneyeRequiresBoot :: FilePath -> Bool
mooneyeRequiresBoot fp = isPrefixOf "boot" $ takeBaseName fp

mooneyeDMG :: FilePath -> Bool
mooneyeDMG fp = any ($ base) dmg || not (any ($ base) nonDmg)
 where
  base = takeBaseName fp
  dmg  = [ isSuffixOf ('-' : s) | s <- ["GS", "dmgABCmgb", "dmgABC"] ]
  nonDmg =
    [ isSuffixOf ('-' : s)
    | s <- ["S", "mgb", "A", "C", "dmg0", "sgb", "sgb2"]
    ]
        -- temporarily exclude because it fails to load
    ++ [("oam_dma_start" ==) . takeBaseName]

getRomsInDir :: FilePath -> IO [FilePath]
getRomsInDir = fmap (filter (isExtensionOf "gb")) . getDirectoryContents

getRomFile :: Bool -> FilePath -> IO Rom
getRomFile f = fmap (either error id) . runExceptT . readRom f

mooneye :: Spec
mooneye = do
  let mooneyeBase = "verify/mooneye"

  brom <- runIO $ fmap (either error id) $ runExceptT $ readBootRom
    "DMG_ROM.bin"
  romGroups <-
    runIO
    $ mapM
        (\(f, subdir) -> do
          let d = mooneyeBase </> subdir
          romfps <- filter f <$> getRomsInDir d
          roms   <- mapM (\fp -> fmap ((,) fp) $ getRomFile False $ d </> fp)
                         romfps
          return $ (subdir, roms)
        )
    $ [ (mooneyeDMG, x)
      | x <-
        [ "emulator-only/mbc1"
        , "acceptance/timer"
        , "acceptance/bits"
        , "acceptance/instr"
        , "acceptance/interrupts"
        , "acceptance/serial"
        , "acceptance/oam_dma"
        , "acceptance/ppu"
-- loops on oam_dma_start
        , "acceptance"
        ]
      ]

  forM_ romGroups $ \(group, roms) -> describe group $ mapM_
    (\(test, rom) ->
      it test $ testWithMooneye (Just brom) rom `shouldBe` Nothing
    )
    roms

main :: IO ()
main = hspec $ parallel $ do
  describe "blargg roms"  blargg
  describe "mooneye roms" mooneye

