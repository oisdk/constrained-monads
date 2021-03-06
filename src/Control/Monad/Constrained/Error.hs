{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | This module is a duplication of the Control.Monad.Error module from the
-- mtl, for constrained monads.
module Control.Monad.Constrained.Error
  (MonadError(..)
  ,ExceptT(..)
  ,Except)
  where

import           GHC.Exts

import           Control.Monad.Constrained
import           Control.Monad.Constrained.Trans

import           Control.Monad.Trans.Except hiding (catchE)

import qualified Control.Monad.Trans.Identity     as Identity
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.Reader       as Reader
import qualified Control.Monad.Trans.State.Lazy   as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict

import qualified Prelude

-- | A class for monads which can error out.
class Monad m =>
      MonadError e m  | m -> e where
    type SuitableError m a :: Constraint
    -- | Raise an error.
    throwError :: SuitableError m a => e -> m a
    {- |
    A handler function to handle previous errors and return to normal execution.
    A common idiom is:

    > do { action1; action2; action3 } `catchError` handler

    where the @action@ functions can call 'throwError'.
    Note that @handler@ and the do-block must have the same return type.
    -}
    catchError :: SuitableError m a => m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
    type SuitableError (Either e) a = ()
    throwError = Left
    catchError (Left x) f = f x
    catchError r _ = r

instance (Monad m, Prelude.Monad (Unconstrained m)) => MonadError e (ExceptT e m) where
    type SuitableError (ExceptT e m) a = Suitable m (Either e a)
    throwError = ExceptT . pure . Left
    catchError = catchE

catchE
    :: (Monad m, Suitable m (Either e' a))
    => ExceptT e m a
    -> (e -> ExceptT e' m a)
    -> ExceptT e' m a
catchE m h =
    ExceptT $
    do a <- runExceptT m
       case a of
           Left l -> runExceptT (h l)
           Right r -> return (Right r)
{-# INLINE catchE #-}

instance MonadError e m => MonadError e (Identity.IdentityT m) where
    type SuitableError (Identity.IdentityT m) a = SuitableError m a
    throwError = lift . throwError
    catchError = Identity.liftCatch catchError

instance (MonadError e m, Prelude.Monad (Unconstrained m)) =>
         MonadError e (Maybe.MaybeT m) where
    type SuitableError (Maybe.MaybeT m) a
        = (SuitableError m a
          ,SuitableError m (Maybe a)
          ,Suitable m (Maybe a))
    throwError = lift . throwError
    catchError = Maybe.liftCatch catchError

instance MonadError e m =>
         MonadError e (Reader.ReaderT r m) where
    type SuitableError (Reader.ReaderT r m) a = SuitableError m a
    throwError = lift . throwError
    catchError = Reader.liftCatch catchError

instance (MonadError e m, Prelude.Monad (Unconstrained m)) =>
         MonadError e (State.Lazy.StateT s m) where
    type SuitableError (State.Lazy.StateT s m) a
        = (Suitable m (a, s), SuitableError m (a, s), SuitableError m a)
    throwError = lift . throwError
    catchError = State.Lazy.liftCatch catchError

instance (MonadError e m, Prelude.Monad (Unconstrained m)) =>
         MonadError e (State.Strict.StateT s m) where
    type SuitableError (State.Strict.StateT s m) a
        = (Suitable m (a, s), SuitableError m (a, s), SuitableError m a)
    throwError = lift . throwError
    catchError = State.Strict.liftCatch catchError
