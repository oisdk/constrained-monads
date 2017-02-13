{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Constrained.Trans where

import           Control.Monad.Constrained
import           Control.Monad.Cont        (ContT (..))
import           Control.Monad.Reader      (ReaderT (..))
import           Control.Monad.State       (StateT (..))
import           GHC.Exts

class MonadTrans t where
  type SuitableLift (t :: (* -> *) -> * -> *) (m :: * -> *) a :: Constraint
  lift :: (SuitableLift t m a, Monad m) => m a -> t m a

instance MonadTrans (ContT r) where
    type SuitableLift (ContT r) m a = Suitable m r
    lift m = ContT (m >>=)
    {-# INLINE lift #-}

instance MonadTrans (ReaderT r) where
    type SuitableLift (ReaderT r) m a = ()
    lift m = ReaderT (const m)
    {-# INLINE lift #-}

instance MonadTrans (StateT r) where
    type SuitableLift (StateT r) m a = Suitable m (a,r)
    lift m = StateT (\s -> fmap (flip (,) s) m)
    {-# INLINE lift #-}
