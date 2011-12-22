
module Control.MonadUtil where

import Control.Monad

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust ma act = maybe (return ()) act ma

mtry :: (MonadPlus m) => a -> m a -> m a
-- ^ Run action, on failure return a
mtry a act = act `mplus` return a

miterate :: (MonadPlus m) => (a -> m a) -> a -> m a
-- ^ Loop until failure then return last result
miterate act a = mtry a (act a >>= miterate act)

mfold :: (MonadPlus m) => (s -> a -> m s) -> s -> [a] -> m s
-- ^ Loop over elements until failure or list exhaustion then return last result
mfold act s [] = return s
mfold act s (a : as) = mtry s (act s a >>= \s -> mfold act s as)
