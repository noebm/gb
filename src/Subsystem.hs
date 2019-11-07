{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Subsystem where

import Data.Word
import Data.Proxy
import Data.Monoid
import Data.Maybe

import Control.Monad
import Control.Applicative

-- | Memory mapped subsystems
class IsSubsystem a where

  inRange :: Proxy a -> Word16 -> Bool

  loadSubsystem :: a -> Word16 -> Word8
  storeSubsystem :: Word16 -> Word8 -> a -> a

maybeLoadSubsystem :: forall s . IsSubsystem s => s -> Word16 -> Maybe Word8
maybeLoadSubsystem s addr = do
  guard (inRange (Proxy :: Proxy s) addr)
  return $ loadSubsystem s addr

data Subsystem = forall a . IsSubsystem a => Subsystem a

load :: [ Subsystem ] -> Word16 -> Word8
load s addr = fromMaybe (error "load: not in range") . getFirst
  $ foldMap (\(Subsystem x) -> First $ maybeLoadSubsystem x addr) s

maybeStore :: forall s . IsSubsystem s => Word16 -> Word8 -> s -> Maybe s
maybeStore addr b s = do
  guard (inRange (Proxy :: Proxy s) addr)
  return $ storeSubsystem addr b s

maybeStoreSubsystem :: Word16 -> Word8 -> Subsystem -> Maybe Subsystem
maybeStoreSubsystem addr b (Subsystem x) = Subsystem <$> maybeStore addr b x

aux :: (a -> Maybe a) -> [ a ] -> [ a ]
aux f (x : xs) = maybe (x : aux f xs) (: xs) $ f x
aux _ [] = []

aux' :: (a -> Maybe a) -> [ a ] -> [ a ]
aux' f = fmap (\x -> fromMaybe x $ f x)

first :: (a -> Maybe a) -> [ a ] -> Maybe [ a ]
first f (x : xs) = ((: xs) <$> f x) <|> ((x :) <$> first f xs)
first _ [] = Nothing

store :: Word16 -> Word8 -> [ Subsystem ] -> [ Subsystem ]
store addr b = aux (maybeStoreSubsystem addr b)
