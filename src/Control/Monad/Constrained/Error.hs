{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Constrained.Error where

import           GHC.Exts

import           Control.Monad.Constrained
import           Control.Monad.Constrained.Trans

import qualified Control.Monad.Trans.Except       as Except

import qualified Control.Monad.Trans.Identity     as Identity
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.Reader       as Reader
import qualified Control.Monad.Trans.State.Lazy   as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict

class Monad m =>
      MonadError e m  | m -> e where
    type SuitableError m a :: Constraint
    throwError :: SuitableError m a => e -> m a
    catchError :: SuitableError m a => m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
    type SuitableError (Either e) a = ()
    throwError = Left
    catchError (Left x) f = f x
    catchError r _ = r

instance Monad m => MonadError e (Except.ExceptT e m) where
    type SuitableError (Except.ExceptT e m) a = Suitable m (Either e a)
    throwError = Except.ExceptT . pure . Left
    catchError = catchE

catchE
    :: (Monad m, Suitable m (Either e' a))
    => Except.ExceptT e m a
    -> (e -> Except.ExceptT e' m a)
    -> Except.ExceptT e' m a
catchE m h =
    Except.ExceptT $
    do a <- Except.runExceptT m
       case a of
           Left l -> Except.runExceptT (h l)
           Right r -> return (Right r)
{-# INLINE catchE #-}

instance MonadError e m => MonadError e (Identity.IdentityT m) where
    type SuitableError (Identity.IdentityT m) a = SuitableError m a
    throwError = lift . throwError
    catchError = Identity.liftCatch catchError

instance MonadError e m =>
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

instance MonadError e m => MonadError e (State.Lazy.StateT s m) where
    type SuitableError (State.Lazy.StateT s m) a
        = (Suitable m (a,s), SuitableError m (a,s), SuitableError m a)
    throwError = lift . throwError
    catchError = State.Lazy.liftCatch catchError

instance MonadError e m => MonadError e (State.Strict.StateT s m) where
    type SuitableError (State.Strict.StateT s m) a
        = (Suitable m (a,s), SuitableError m (a,s), SuitableError m a)
    throwError = lift . throwError
    catchError = State.Strict.liftCatch catchError
