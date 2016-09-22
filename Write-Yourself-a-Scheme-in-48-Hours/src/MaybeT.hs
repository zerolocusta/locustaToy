module MaybeT where

newtype MaybeT m a = MaybeT { runMaybeT :: (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just

  (>>=) ::  MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
