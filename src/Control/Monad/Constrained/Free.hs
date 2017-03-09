{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

module Control.Monad.Constrained.Free where

import qualified Control.Monad.Constrained as Constrained
import Control.Monad.Constrained (AppVect(..), Suitable, liftA, FunType)

data Free f a where
  Pure :: (a -> b) -> a -> Free f b
  Ap :: Free f (a -> b) -> (c -> a) -> f c -> Free f b

instance Functor (Free f) where
  fmap f (Pure c x) = Pure (f . c) x
  fmap f (Ap tx c ay) = Ap ((f .) <$> tx) c ay

instance Applicative (Free f) where
  pure = Pure id
  Pure c f <*> tx = fmap (c f) tx
  Ap tx c ay <*> tz = Ap (flip <$> tx <*> tz) c ay

lift :: f a -> Free f a
lift = Ap (Pure id id) id

lower
    :: forall f a c.
       Free f a
    -> (forall xs. FunType xs a -> AppVect f xs -> f c)
    -> f c
lower (Pure c x) f = f (c x) Nil
lower (Ap fs c x :: Free f a) f =
    lower
        (fmap (. c) fs)
        (\ft av ->
              f ft (av :> x))

lowerConstrained
    :: (Constrained.Applicative f, Suitable f a)
    => Free f a -> f a
lowerConstrained x = lower x liftA
