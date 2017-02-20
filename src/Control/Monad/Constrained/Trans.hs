{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module duplicates the "Control.Monad.Trans.Class" module for
-- constrained monads.
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

-- | A class for monad transformers with constraints. See
-- "Control.Monad.Trans.Class" for full documentation on the class without
-- constraints.
class MonadTrans t  where
    -- | A type for monads that are liftable into the outer monad. For instance,
    -- since 'StateT' is defined like so:
    --
    -- @newtype 'StateT' s m a = 'StateT' { 'runStateT' :: s -> m (a, s) }@
    --
    -- the underlying monad needs not to be able to hold @a@, but @(a, s)@.
    type SuitableLift (t :: (* -> *) -> * -> *) (m :: * -> *) (a :: *) :: Constraint
    -- | Lift a monad into an outer monad.
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
