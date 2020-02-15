module Utilities.Step where

import Data.Word

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

{-# INLINE extendStep #-}
extendStep :: Monad m => (a -> m b) -> Step m a -> Step m b
extendStep f s = Step $ do
  (x , s) <- runStep s
  y <- f x
  return (y , extendStep f s)

{-# INLINE (>~) #-}
(>~) :: Monad m => Step m a -> (a -> m b) -> Step m b
(>~) = flip extendStep

-- | Gather effects of 'n' iterations
{-# INLINE runSteps_ #-}
runSteps_ :: Monad m => Word -> Step m a -> m (Step m a)
runSteps_ 0 s = return s
runSteps_ n s = do
  (_ , s) <- runStep s
  runSteps_ (n - 1) s

{-# INLINE runStepsWith #-}
runStepsWith :: Monad m => Word -> Step m a -> (a -> m ()) -> m (Step m a)
runStepsWith 0 s _ = return s
runStepsWith n s f = do
  (dt, s') <- runStep s
  f dt
  runStepsWith (n - 1) s' f
