{-# LANGUAGE RebindableSyntax #-}

-- | This module is a duplication of the Control.Monad.Cont module, from the\
-- mtl.
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

-- | A class for monads which can embed continuations.
class Monad m => MonadCont m where
    {- | @callCC@ (call-with-current-continuation)
    calls a function with the current continuation as its argument.
    Provides an escape continuation mechanism for use with Continuation monads.
    Escape continuations allow to abort the current computation and return
    a value immediately.
    They achieve a similar effect to 'Control.Monad.Error.throwError'
    and 'Control.Monad.Error.catchError'
    within an 'Control.Monad.Error.Error' monad.
    Advantage of this function over calling @return@ is that it makes
    the continuation explicit,
    allowing more flexibility and better control
    (see examples in "Control.Monad.Cont").

    The standard idiom used with @callCC@ is to provide a lambda-expression
    to name the continuation. Then calling the named continuation anywhere
    within its scope will escape from the computation,
    even if it is many layers deep within nested computations.
    -}
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
