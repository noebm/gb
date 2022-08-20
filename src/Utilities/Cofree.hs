module Utilities.Cofree where

import           Control.Comonad.Cofree

{-# INLINE coExtend #-}
coExtend :: Applicative f => (a -> f b) -> Cofree f a -> f (Cofree f b)
coExtend f = go where go (x :< s) = (:<) <$> f x <*> (go <$> s)

{-# INLINE coFilter #-}
coFilter :: Monad m => Cofree m (Maybe a) -> m (Cofree m a)
coFilter (x :< s) = maybe id (\x -> return . (x :<)) x $ coFilter =<< s
