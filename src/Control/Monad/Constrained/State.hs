{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Constrained.State where

import           GHC.Exts

import           Control.Monad.Constrained

import qualified Control.Monad.Trans.State.Lazy   as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict

import qualified Control.Monad.Trans.Cont         as Cont
import qualified Control.Monad.Trans.Identity     as Identity
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.Reader       as Reader
import qualified Control.Monad.Trans.Except       as Except

import           Control.Monad.Constrained.Trans

class Monad m =>
      MonadState s m  | m -> s where
    {-# MINIMAL state #-}
    type StateSuitable m s a :: Constraint
    get
        :: (StateSuitable m s s)
        => m s
    get =
        state
            (\s ->
                  (s, s))
    put
        :: (StateSuitable m s (), StateSuitable m s s)
        => s -> m ()
    put s = state (const ((), s))
    state
        :: (StateSuitable m s a, StateSuitable m s s)
        => (s -> (a, s)) -> m a

gets :: (StateSuitable m s s, MonadState s m, Suitable m b) => (s -> b) -> m b
gets f = fmap f get

modify
    :: (StateSuitable m s (), StateSuitable m s s, MonadState s m)
    => (s -> s) -> m ()
modify f =
    state
        (\s ->
              ((), f s))

modify'
    :: (StateSuitable m s (), StateSuitable m s s, MonadState s m)
    => (s -> s) -> m ()
modify' f =
    state
        (\s ->
              let s' = f s
              in s' `seq` ((), s'))

instance Monad m => MonadState s (State.Strict.StateT s m) where
  type StateSuitable (State.Strict.StateT s m) s a = Suitable m (a, s)
  state f = State.Strict.StateT (pure . f)

instance Monad m => MonadState s (State.Lazy.StateT s m) where
  type StateSuitable (State.Lazy.StateT s m) s a = Suitable m (a, s)
  state f = State.Lazy.StateT (pure . f)

instance (MonadState s m, Suitable m r) => MonadState s (Cont.ContT r m) where
    type StateSuitable (Cont.ContT r m) s a = StateSuitable m s a
    state = lift . state

instance MonadState s m =>
         MonadState s (Maybe.MaybeT m) where
    type StateSuitable (Maybe.MaybeT m) s a = (Suitable m (Maybe a), StateSuitable m s a)
    state = lift . state

instance MonadState s m =>
         MonadState s (Identity.IdentityT m) where
    type StateSuitable (Identity.IdentityT m) s a = (StateSuitable m s a)
    state = lift . state

instance MonadState s m =>
         MonadState s (Reader.ReaderT r m) where
    type StateSuitable (Reader.ReaderT r m) s a = StateSuitable m s a
    state = lift . state

instance MonadState s m =>
         MonadState s (Except.ExceptT e m) where
    type StateSuitable (Except.ExceptT e m) s a = (Suitable m (Either e a), StateSuitable m s a)
    state = lift . state
