{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeFamilies           #-}

module Control.Monad.Constrained.Reader where

import           GHC.Exts

import           Control.Monad.Constrained
import           Control.Monad.Constrained.Trans

import qualified Control.Monad.Trans.Reader       as Reader

import qualified Control.Monad.Trans.State.Lazy   as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Cont         as Cont
import qualified Control.Monad.Trans.Identity     as Identity
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.Except       as Except

class Monad m =>
      MonadReader r m  | m -> r where
    type ReaderSuitable m r a :: Constraint
    {-# MINIMAL reader , local #-}
    ask
        :: (ReaderSuitable m r r)
        => m r
    ask = reader id
    local
        :: (ReaderSuitable m r a, ReaderSuitable m r r)
        => (r -> r) -> m a -> m a
    reader
        :: (ReaderSuitable m r r, ReaderSuitable m r a)
        => (r -> a) -> m a

instance MonadReader r ((->) r) where
    type ReaderSuitable ((->) r) r a = ()
    ask = id
    local f m = m . f
    reader = id

instance Monad m => MonadReader r (Reader.ReaderT r m) where
    type ReaderSuitable (Reader.ReaderT r m) r a = (Suitable m a)
    ask = Reader.ReaderT pure
    local = Reader.local
    reader f = Reader.ReaderT (pure . f)

instance MonadReader r' m =>
         MonadReader r' (Cont.ContT r m) where
    type ReaderSuitable (Cont.ContT r m) r' a
        = (ReaderSuitable m r' a, Suitable m r, ReaderSuitable m r' r)
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
    type ReaderSuitable (Except.ExceptT e m) r a
        = (ReaderSuitable m r a
          ,Suitable m (Either e a)
          ,ReaderSuitable m r (Either e a))
    ask = lift ask
    local = Except.mapExceptT . local
    reader = lift . reader

instance MonadReader r m => MonadReader r (Identity.IdentityT m) where
    type ReaderSuitable (Identity.IdentityT m) r a = ReaderSuitable m r a
    ask = lift ask
    local = Identity.mapIdentityT . local
    reader = lift . reader

instance MonadReader r m =>
         MonadReader r (Maybe.MaybeT m) where
    type ReaderSuitable (Maybe.MaybeT m) r a
        = (ReaderSuitable m r a
          ,ReaderSuitable m r (Maybe a)
          ,Suitable m (Maybe a))
    ask = lift ask
    local = Maybe.mapMaybeT . local
    reader = lift . reader

instance MonadReader r m =>
         MonadReader r (State.Lazy.StateT s m) where
    type ReaderSuitable (State.Lazy.StateT s m) r a
        = (ReaderSuitable m r a
          ,ReaderSuitable m r (a,s)
          ,Suitable m (a,s))
    ask = lift ask
    local = State.Lazy.mapStateT . local
    reader = lift . reader

instance MonadReader r m =>
         MonadReader r (State.Strict.StateT s m) where
    type ReaderSuitable (State.Strict.StateT s m) r a
        = (ReaderSuitable m r a
          ,ReaderSuitable m r (a,s)
          ,Suitable m (a,s))
    ask = lift ask
    local = State.Strict.mapStateT . local
    reader = lift . reader
