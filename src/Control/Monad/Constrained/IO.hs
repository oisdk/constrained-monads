{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module is a duplication of the "Control.Monad.IO.Class" module, for
-- constrained monads.
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

-- | A class for monads which can have IO actions lifted into them.
class Monad m =>
      MonadIO m  where
    type SuitableIO m a :: Constraint
    liftIO :: SuitableIO m a => IO a -> m a

instance MonadIO IO where
    type SuitableIO IO a = ()
    liftIO = id

instance MonadIO m =>
         MonadIO (IdentityT m) where
    type SuitableIO (IdentityT m) a = SuitableIO m a
    liftIO = lift . liftIO

instance MonadIO m =>
         MonadIO (MaybeT m) where
    type SuitableIO (MaybeT m) a = (Suitable m (Maybe a), SuitableIO m a)
    liftIO = lift . liftIO

instance MonadIO m =>
         MonadIO (ContT r m) where
    type SuitableIO (ContT r m) a = (Suitable m r, SuitableIO m a)
    liftIO = lift . liftIO

instance MonadIO m =>
         MonadIO (ReaderT r m) where
    type SuitableIO (ReaderT r m) a = SuitableIO m a
    liftIO = lift . liftIO

instance MonadIO m =>
         MonadIO (StateT s m) where
    type SuitableIO (StateT s m) a = (SuitableIO m a, Suitable m (a, s))
    liftIO = lift . liftIO

instance MonadIO m =>
         MonadIO (Lazy.StateT s m) where
    type SuitableIO (Lazy.StateT s m) a = (SuitableIO m a, Suitable m (a, s))
    liftIO = lift . liftIO
