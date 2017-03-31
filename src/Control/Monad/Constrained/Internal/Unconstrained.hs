{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}

module Control.Monad.Constrained.Internal.Unconstrained where

newtype StrictLeftFold a
  = StrictLeftFold (forall b. (b -> a -> b) -> b -> b)

instance Functor StrictLeftFold where
    fmap f (StrictLeftFold xs) = StrictLeftFold (\c -> xs (\ !a -> c a . f))
    {-# INLINE fmap #-}

instance Applicative StrictLeftFold where
    pure x =
        StrictLeftFold (\c b -> c b x)
    {-# INLINE pure #-}
    StrictLeftFold fs <*> StrictLeftFold xs =
      StrictLeftFold (\c -> fs (\ !fb f -> xs (\ !xb -> c xb . f) fb))
    {-# INLINE (<*>) #-}
