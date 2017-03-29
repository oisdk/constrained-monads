{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}

module Control.Monad.Constrained.Internal.Unconstrained where

newtype ChurchSet a
  = ChurchSet (forall b. (b -> a -> b) -> b -> b)

instance Functor ChurchSet where
    fmap f (ChurchSet xs) = ChurchSet (\c -> xs (\ !a -> c a . f))
    {-# INLINE fmap #-}

instance Applicative ChurchSet where
    pure x =
        ChurchSet (\c b -> c b x)
    {-# INLINE pure #-}
    ChurchSet fs <*> ChurchSet xs =
      ChurchSet (\c -> fs (\ !fb f -> xs (\ !xb -> c xb . f) fb))
    {-# INLINE (<*>) #-}
