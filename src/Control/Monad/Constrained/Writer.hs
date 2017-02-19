{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Constrained.Writer where

import           GHC.Exts

import           Control.Monad.Constrained
import           Control.Monad.Constrained.Trans

import qualified Control.Monad.Trans.State.Lazy   as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Identity     as Identity
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.Reader       as Reader
import qualified Control.Monad.Trans.Except       as Except

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    type WriterSuitable m a :: Constraint
    writer  :: WriterSuitable m a => (a,w) -> m a
    tell    :: WriterSuitable m () => w -> m ()
    listenC :: WriterSuitable m b => (a -> w -> b) -> m a -> m b
    passC   :: WriterSuitable m a => (a -> w -> w) -> m a -> m a

instance MonadWriter w m =>
         MonadWriter w (Except.ExceptT e m) where
    type WriterSuitable (Except.ExceptT e m) a
        = (WriterSuitable m a
          ,WriterSuitable m (Either e a)
          ,Suitable m (Either e a))
    writer = lift . writer
    tell = lift . tell
    listenC f = (Except.mapExceptT . listenC . flip) (fmap . flip f)
    passC = Except.mapExceptT . passC . either (const id)

listen
    :: (MonadWriter w m, WriterSuitable m (a, w))
    => m a -> m (a, w)
listen = listenC (,)

pass
    :: (MonadWriter w m, Suitable m a, WriterSuitable m (a, w -> w))
    => m (a, w -> w) -> m a
pass = fmap fst . passC snd

instance MonadWriter w m =>
         MonadWriter w (State.Lazy.StateT s m) where
    type WriterSuitable (State.Lazy.StateT s m) a
        = (WriterSuitable m a
          ,WriterSuitable m (a, s)
          ,Suitable m (a, s))
    writer = lift . writer
    tell = lift . tell
    listenC f m =
        State.Lazy.StateT
            (listenC
                 (\ ~(a,s') w ->
                       (f a w, s')) .
             State.Lazy.runStateT m)
    passC c m = State.Lazy.StateT (passC (c . fst) . State.Lazy.runStateT m)

instance MonadWriter w m =>
         MonadWriter w (State.Strict.StateT s m) where
    type WriterSuitable (State.Strict.StateT s m) a
        = (WriterSuitable m a
          ,WriterSuitable m (a, s)
          ,Suitable m (a, s))
    writer = lift . writer
    tell = lift . tell
    listenC f m =
        State.Strict.StateT
            (listenC
                 (\ (a,s') w ->
                       (f a w, s')) .
             State.Strict.runStateT m)
    passC c m = State.Strict.StateT (passC (c . fst) . State.Strict.runStateT m)

instance MonadWriter w m =>
         MonadWriter w (Identity.IdentityT m) where
    type WriterSuitable (Identity.IdentityT m) a = WriterSuitable m a
    writer = lift . writer
    tell = lift . tell
    listenC f = Identity.mapIdentityT (listenC f)
    passC f = Identity.mapIdentityT (passC f)

instance MonadWriter w m => MonadWriter w (Maybe.MaybeT m) where
    type WriterSuitable (Maybe.MaybeT m) a
        = (WriterSuitable m a
          ,WriterSuitable m (Maybe a)
          ,Suitable m (Maybe a))
    writer = lift . writer
    tell = lift . tell
    listenC f = (Maybe.mapMaybeT . listenC . flip) (fmap . flip f)
    passC = Maybe.mapMaybeT . passC . maybe id

instance MonadWriter w m => MonadWriter w (Reader.ReaderT r m) where
    type WriterSuitable (Reader.ReaderT r m) a = WriterSuitable m a
    writer = lift . writer
    tell = lift . tell
    listenC f = Reader.mapReaderT (listenC f)
    passC f = Reader.mapReaderT (passC f)
