module Utilities.Step where

data Step m a = Step { runStep :: m (a , Step m a) }

instance Functor f => Functor (Step f) where
  fmap f = Step . fmap (\(x , s') -> (f x , fmap f s')) . runStep

instance Applicative f => Applicative (Step f) where
  pure x = Step $ pure (x , pure x)
  Step fs <*> Step xs = Step $ (\(f , fs) (x , xs) -> (f x , fs <*> xs)) <$> fs <*> xs

{-# INLINE delay #-}
delay = Step

stepsFromLoop :: Monad m => (a -> m (b , a)) -> m a -> Step m b
stepsFromLoop f x = Step $ aux =<< x
  where
    aux x = do
      (y , x') <- f x
      return (y , Step $ aux x')
