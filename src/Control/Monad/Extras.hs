module Control.Monad.Extras (seqM) where

seqM :: Monad m => m a -> m a
seqM m = do
  a <- m
  return $! a
