{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Constrained.IO
  (MonadIO(..))
  where

import           Control.Monad.Constrained
import           Control.Monad.Constrained.Trans

import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy as Lazy

import           GHC.Exts

class Monad m =>
      MonadIO m  where
    type SuitableIO m a :: Constraint
    liftIO :: SuitableIO m a => IO a -> m a
    default liftIO :: (MonadTrans n, MonadIO t, m ~ n t, SuitableLift n t a, SuitableIO t a) => IO a -> m a
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}


instance MonadIO IO where
    type SuitableIO IO a = ()
    liftIO = id

instance MonadIO m =>
         MonadIO (IdentityT m) where
    type SuitableIO (IdentityT m) a = SuitableIO m a

instance MonadIO m =>
         MonadIO (MaybeT m) where
    type SuitableIO (MaybeT m) a = (Suitable m (Maybe a), SuitableIO m a)

instance MonadIO m =>
         MonadIO (ContT r m) where
    type SuitableIO (ContT r m) a = (Suitable m r, SuitableIO m a)

instance MonadIO m =>
         MonadIO (ReaderT r m) where
    type SuitableIO (ReaderT r m) a = SuitableIO m a

instance MonadIO m =>
         MonadIO (StateT s m) where
    type SuitableIO (StateT s m) a = (SuitableIO m a, Suitable m (a, s))

instance MonadIO m =>
         MonadIO (Lazy.StateT s m) where
    type SuitableIO (Lazy.StateT s m) a = (SuitableIO m a, Suitable m (a, s))
