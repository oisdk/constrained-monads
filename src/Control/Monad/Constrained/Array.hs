{-# LANGUAGE GADTs              #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RebindableSyntax   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Control.Monad.Constrained.Array where

import Control.Monad.Constrained
import Data.Array.Unboxed hiding (Array)

data Array i a where
  Array :: IArray UArray a => UArray i a -> Array i a

deriving instance (Ix i, Eq a) => Eq (Array i a)
deriving instance (Ix i, Ord a) => Ord (Array i a)
deriving instance (Ix i, Show i, Show e) => Show (Array i e)

instance Ix i => Functor (Array i) where
  type Suitable (Array i) a = IArray UArray a
  fmap f (Array xs) = Array (amap f xs)

instance Ix i =>
         Foldable (Array i) where
    foldr f b (Array xs) = foldr f b (elems xs)
    {-# INLINE foldr #-}
    length (Array xs) = rangeSize (bounds xs)
    {-# INLINE length #-}
