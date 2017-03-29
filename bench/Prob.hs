{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE Strict                     #-}

module Prob where

import           Control.Monad.Constrained

import qualified Prelude

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map

newtype Prob a
  = Prob
  { runProb :: Map a Double
  } deriving (Show, Eq, Ord)

newtype Dist a
  = Dist
  { runDist :: [(a,Double)]
  } deriving (Prelude.Functor, Monoid)

instance Prelude.Applicative Dist where
  pure x = Dist [(x , 1)]
  {-# INLINE pure #-}
  Dist fs <*> Dist xs
    = Dist
    [ (f x, fp * xp)
    | (f , fp) <- fs
    , (x , xp) <- xs ]
  {-# INLINE (<*>) #-}

instance Prelude.Monad Dist where
  Dist xs >>= f
    = Dist
    [ (y, xp * yp)
    | (x , xp) <- xs
    , (y , yp) <- runDist (f x) ]
  {-# INLINE (>>=) #-}

instance Functor Prob where
    type Suitable Prob a = Ord a
    fmap f (Prob xs) = Prob (Map.mapKeysWith (+) f xs)
    {-# INLINE fmap #-}

type instance Unconstrained Prob = Dist

instance Applicative Prob where
    reflect = Dist . Map.toList . runProb
    {-# INLINE reflect #-}
    reify = Prob . Map.fromListWith (+) . runDist
    {-# INLINE reify #-}
    _ *> x = x
    {-# INLINE (*>) #-}
    x <* _ = x
    {-# INLINE (<*) #-}

scaled :: Prob a -> Double -> Prob a
scaled (Prob xs) n = Prob (Map.map (n*) xs)
{-# INLINE scaled #-}

instance Monad Prob where
    Prob xs >>= f =
        Map.foldlWithKey'
            (\a x p ->
                  combScale p a (f x))
            mempty
            xs
    {-# INLINE (>>=) #-}

instance (Ord a) =>
         Monoid (Prob a) where
    mempty = Prob Map.empty
    {-# INLINE mempty #-}
    mappend (Prob xs) (Prob ys) = Prob (Map.unionWith (+) xs ys)
    {-# INLINE mappend #-}

combScale
    :: (Ord a)
    => Double -> Prob a -> Prob a -> Prob a
combScale p (Prob xs) (Prob ys) =
    Prob
        (Map.mergeWithKey
             (\_ x y ->
                   Just $ x + p * y)
             id
             (Map.map (p *))
             xs
             ys)
{-# INLINE combScale #-}

instance Foldable Prob where
    foldMap f (Prob xs) =
        Map.foldMapWithKey
            (\k _ ->
                  f k)
            xs
    {-# INLINE foldMap #-}

uniform
    :: (Ord a)
    => [a] -> Prob a
uniform xs =
    (Prob . Map.fromListWith (+) . map (flip (,) (1 / fromIntegral l))) xs
  where
    l = length xs
{-# INLINE uniform #-}

upTo :: (Integral a) => a -> Prob a
upTo n = uniform [1..n]
{-# INLINE upTo #-}

eighths :: Prob Integer
eighths = uniform [1..8]
{-# INLINE eighths #-}

probOf
    :: (Ord a)
    => a -> Prob a -> Double
probOf x (Prob xs) = Map.findWithDefault 0 x xs
{-# INLINE probOf #-}
