module Utilities.Cofree where

import           Control.Comonad.Cofree

{-# INLINE traverseCofree #-}
traverseCofree :: Applicative f => (a -> f b) -> Cofree f a -> f (Cofree f b)
traverseCofree f = go where go (x :< s) = (:<) <$> f x <*> (go <$> s)

{-# INLINE catMaybes #-}
catMaybes :: Monad m => Cofree m (Maybe a) -> m (Cofree m a)
catMaybes (x :< s) = maybe id (\x -> return . (x :<)) x $ catMaybes =<< s
