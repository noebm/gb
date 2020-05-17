module Utilities.Cofree where

import Control.Comonad.Cofree

{-# INLINE coExtend #-}
coExtend :: Monad m => (a -> m b) -> Cofree m a -> m (Cofree m b)
coExtend f (x :< s) = (:< (coExtend f =<< s)) <$> f x

{-# INLINE coFilter #-}
coFilter :: Monad m => Cofree m (Maybe a) -> m (Cofree m a)
coFilter (x :< s) = maybe id (\x -> return . (x :<)) x $ coFilter =<< s
