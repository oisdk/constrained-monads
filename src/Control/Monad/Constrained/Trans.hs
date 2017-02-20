{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Constrained.Trans
  (MonadTrans(..))
  where

import           Control.Monad.Constrained

import           Control.Monad.Trans.Cont         (ContT (..))
import           Control.Monad.Trans.Except       (ExceptT (..))
import           Control.Monad.Trans.Identity     (IdentityT (..))
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State.Lazy   as Lazy (StateT (..))
import           Control.Monad.Trans.State.Strict as Strict (StateT (..))

import           GHC.Exts

class MonadTrans t  where
    type SuitableLift (t :: (* -> *) -> * -> *) (m :: * -> *) (a :: *) :: Constraint
    lift
        :: (Monad m, SuitableLift t m a)
        => m a -> t m a

instance MonadTrans (ContT r) where
    type SuitableLift (ContT r) m a = Suitable m r
    lift m = ContT (m >>=)
    {-# INLINE lift #-}

instance MonadTrans (ReaderT r) where
    type SuitableLift (ReaderT r) m a = ()
    lift m = ReaderT (const m)
    {-# INLINE lift #-}

instance MonadTrans (Strict.StateT r) where
    type SuitableLift (Strict.StateT r) m a = Suitable m (a,r)
    lift m = Strict.StateT (\s -> fmap (flip (,) s) m)
    {-# INLINE lift #-}

instance MonadTrans (Lazy.StateT r) where
    type SuitableLift (Lazy.StateT r) m a = Suitable m (a,r)
    lift m = Lazy.StateT (\s -> fmap (flip (,) s) m)
    {-# INLINE lift #-}

instance MonadTrans IdentityT where
    type SuitableLift IdentityT m a = ()
    lift = IdentityT
    {-# INLINE lift #-}

instance MonadTrans MaybeT where
    type SuitableLift MaybeT m a = Suitable m (Maybe a)
    lift = MaybeT . fmap Just
    {-# INLINE lift #-}

instance MonadTrans (ExceptT e) where
    type SuitableLift (ExceptT e) m a = Suitable m (Either e a)
    lift = ExceptT . fmap Right
    {-# INLINE lift #-}
