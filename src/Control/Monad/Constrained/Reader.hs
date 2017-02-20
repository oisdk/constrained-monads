{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | This module duplicates the Control.Monad.Reader module from the mtl, for
-- constrained monads.
module Control.Monad.Constrained.Reader
  (MonadReader(..)
  ,ReaderT(..)
  ,Reader
  )where

import           GHC.Exts

import           Control.Monad.Constrained
import           Control.Monad.Constrained.Trans

import           Control.Monad.Trans.Reader hiding (reader, ask, local)

import qualified Control.Monad.Trans.State.Lazy   as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Cont         as Cont
import qualified Control.Monad.Trans.Identity     as Identity
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.Except       as Except

-- | A class for reader monads.
class Monad m =>
      MonadReader r m  | m -> r where
    type ReaderSuitable m a :: Constraint
    {-# MINIMAL reader , local #-}
    -- | Retrieves the environment
    ask
        :: (ReaderSuitable m r)
        => m r
    ask = reader id
    -- | Executes a computation in a modified environment.
    local
        :: (ReaderSuitable m a, ReaderSuitable m r)
        => (r -> r) -- ^ The function to modify the environment.
        -> m a      -- ^ @Reader@ to run in the modified environment.
        -> m a

    -- | Retrieves a function of the current environment.
    reader
        :: (ReaderSuitable m r, ReaderSuitable m a)
        => (r -> a) -- ^ The selector function to apply to the environment.
        -> m a

instance MonadReader r ((->) r) where
    type ReaderSuitable ((->) r) a = ()
    ask = id
    local f m = m . f
    reader = id

instance Monad m => MonadReader r (ReaderT r m) where
    type ReaderSuitable (ReaderT r m) a = Suitable m a
    ask = ReaderT pure
    local = local
    reader f = ReaderT (pure . f)

instance MonadReader r' m =>
         MonadReader r' (Cont.ContT r m) where
    type ReaderSuitable (Cont.ContT r m) a
        = (ReaderSuitable m a, Suitable m r, ReaderSuitable m r)
    ask = lift ask
    local = liftLocal ask local
    reader = lift . reader

liftLocal
    :: (Monad m, Suitable m r)
    => m r'
    -> ((r' -> r') -> m r -> m r)
    -> (r' -> r')
    -> Cont.ContT r m a
    -> Cont.ContT r m a
liftLocal ask' local' f m =
    Cont.ContT $
    \c -> do
        r <- ask'
        local' f (Cont.runContT m (local' (const r) . c))

instance MonadReader r m => MonadReader r (Except.ExceptT e m) where
    type ReaderSuitable (Except.ExceptT e m) a
        = (ReaderSuitable m a
          ,Suitable m (Either e a)
          ,ReaderSuitable m (Either e a))
    ask = lift ask
    local = Except.mapExceptT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (Identity.IdentityT m) where
    type ReaderSuitable (Identity.IdentityT m) a = ReaderSuitable m a
    ask = lift ask
    local = Identity.mapIdentityT . local
    reader = lift . reader

instance MonadReader r m =>
         MonadReader r (Maybe.MaybeT m) where
    type ReaderSuitable (Maybe.MaybeT m) a
        = (ReaderSuitable m a
          ,ReaderSuitable m (Maybe a)
          ,Suitable m (Maybe a))
    ask = lift ask
    local = Maybe.mapMaybeT . local
    reader = lift . reader

instance MonadReader r m =>
         MonadReader r (State.Lazy.StateT s m) where
    type ReaderSuitable (State.Lazy.StateT s m) a
        = (ReaderSuitable m a
          ,ReaderSuitable m (a,s)
          ,Suitable m (a,s))
    ask = lift ask
    local = State.Lazy.mapStateT . local
    reader = lift . reader

instance MonadReader r m =>
         MonadReader r (State.Strict.StateT s m) where
    type ReaderSuitable (State.Strict.StateT s m) a
        = (ReaderSuitable m a
          ,ReaderSuitable m (a,s)
          ,Suitable m (a,s))
    ask = lift ask
    local = State.Strict.mapStateT . local
    reader = lift . reader
