{-# LANGUAGE TemplateHaskell #-}
module Hardware.Cartridge.Bank.Bank
  ( Bank, Banks
  , BankState
  , isBankIndex

  , activeBank

  , makeBanks
  , swapBank
  )
where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Data.Word

import Control.Lens

type Banks = V.Vector Bank

type Bank = VU.Vector Word8

data BankState = BankState
  { _banks :: Banks
  , _activeBankIndex :: Int
  }

makeLenses ''BankState

activeBank :: Traversal' BankState Bank
activeBank f s = (banks . ix (s ^. activeBankIndex)) f s

isBankIndex :: Int -> V.Vector Bank -> Bool
isBankIndex i xs = 0 <= i && i < V.length xs

makeBanks :: Int -> V.Vector Bank -> BankState
makeBanks i xs
  | isBankIndex i xs = BankState { _banks = xs , _activeBankIndex = i }
  | otherwise = error "makeBanks: active index out of range"

swapBank :: Int -> BankState -> BankState
swapBank i s = s & activeBankIndex .~ (i `mod` V.length (s ^. banks))
