{-# LANGUAGE RebindableSyntax #-}

module Control.Monad.Constrained.Cont
  (MonadCont(..)
  ,ContT(..)
  ,cont
  ,mapContT
  ,withContT
  ,runCont
  ,mapCont
  ,withCont)where

import Control.Monad.Constrained

import qualified Control.Monad.Trans.Cont as Cont
import           Control.Monad.Trans.Cont hiding (callCC)

import qualified Control.Monad.Trans.Reader       as Reader
import qualified Control.Monad.Trans.State.Lazy   as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Identity     as Identity
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.Except       as Except

class Monad m => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (ContT r m) where
  callCC = Cont.callCC

instance MonadCont m => MonadCont (Maybe.MaybeT m) where
  callCC = Maybe.liftCallCC callCC

instance MonadCont m => MonadCont (Reader.ReaderT r m) where
  callCC = Reader.liftCallCC callCC

instance MonadCont m => MonadCont (State.Lazy.StateT s m) where
  callCC = State.Lazy.liftCallCC callCC

instance MonadCont m => MonadCont (State.Strict.StateT s m) where
  callCC = State.Strict.liftCallCC callCC

instance MonadCont m => MonadCont (Identity.IdentityT m) where
  callCC = Identity.liftCallCC callCC

instance MonadCont m => MonadCont (Except.ExceptT e m) where
  callCC = Except.liftCallCC callCC
