{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Constrained.State
  (MonadState(..)
  ,StateT(..)
  ,gets
  ,modify
  ,modify'
  )where

import           GHC.Exts

import           Control.Monad.Constrained
import           Control.Monad.Constrained.Trans

import qualified Control.Monad.Trans.State.Lazy   as State.Lazy
import           Control.Monad.Trans.State.Strict hiding (state, get, modify, gets, modify')

import qualified Control.Monad.Trans.Cont         as Cont
import qualified Control.Monad.Trans.Identity     as Identity
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.Reader       as Reader
import qualified Control.Monad.Trans.Except       as Except


class Monad m =>
      MonadState s m  | m -> s where
    {-# MINIMAL state #-}
    type StateSuitable m a :: Constraint
    get
        :: (StateSuitable m s)
        => m s
    get =
        state
            (\s ->
                  (s, s))
    put
        :: (StateSuitable m (), StateSuitable m s)
        => s -> m ()
    put s = state (const ((), s))
    state
        :: (StateSuitable m a, StateSuitable m s)
        => (s -> (a, s)) -> m a

gets
    :: (StateSuitable m s, MonadState s m, Suitable m b)
    => (s -> b) -> m b
gets f = fmap f get

modify
    :: (StateSuitable m (), StateSuitable m s, MonadState s m)
    => (s -> s) -> m ()
modify f =
    state
        (\s ->
              ((), f s))

modify'
    :: (StateSuitable m (), StateSuitable m s, MonadState s m)
    => (s -> s) -> m ()
modify' f =
    state
        (\s ->
              let s' = f s
              in s' `seq` ((), s'))

instance Monad m => MonadState s (StateT s m) where
  type StateSuitable (StateT s m) a = Suitable m (a, s)
  state f = StateT (pure . f)

instance Monad m => MonadState s (State.Lazy.StateT s m) where
  type StateSuitable (State.Lazy.StateT s m) a = Suitable m (a, s)
  state f = State.Lazy.StateT (pure . f)

instance (MonadState s m, Suitable m r) => MonadState s (Cont.ContT r m) where
    type StateSuitable (Cont.ContT r m) a = StateSuitable m a
    state = lift . state

instance MonadState s m =>
         MonadState s (Maybe.MaybeT m) where
    type StateSuitable (Maybe.MaybeT m) a
        = (Suitable m (Maybe a), StateSuitable m a)
    state = lift . state

instance MonadState s m =>
         MonadState s (Identity.IdentityT m) where
    type StateSuitable (Identity.IdentityT m) a = StateSuitable m a
    state = lift . state

instance MonadState s m =>
         MonadState s (Reader.ReaderT r m) where
    type StateSuitable (Reader.ReaderT r m) a = StateSuitable m a
    state = lift . state

instance MonadState s m =>
         MonadState s (Except.ExceptT e m) where
    type StateSuitable (Except.ExceptT e m) a
        = (Suitable m (Either e a), StateSuitable m a)
    state = lift . state
