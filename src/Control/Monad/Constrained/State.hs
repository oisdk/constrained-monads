{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Constrained.State where

import Control.Monad.Constrained

import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Maybe as Maybe


class Monad m =>
      MonadState s m  | m -> s where
    {-# MINIMAL state #-}
    get
        :: Suitable m s
        => m s
    get =
        state
            (\s ->
                  (s, s))
    put
        :: Suitable m ()
        => s -> m ()
    put s = state (const ((), s))
    state
        :: Suitable m a
        => (s -> (a, s)) -> m a

instance Monad m => MonadState s (State.Strict.StateT s m) where
  state f = State.Strict.StateT (pure . f)

instance Monad m => MonadState s (State.Lazy.StateT s m) where
  state f = State.Lazy.StateT (pure . f)

instance MonadState s m => MonadState s (Maybe.MaybeT m) where
  
