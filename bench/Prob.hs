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
  { runDist :: [Weighted a]
  } deriving (Prelude.Functor, Monoid)

data Weighted a =
    (:!:) !a
          {-# UNPACK #-} !Double
    deriving (Prelude.Functor)

instance Prelude.Applicative Dist where
  pure x = Dist [x :!: 1]
  {-# INLINE pure #-}
  Dist fs <*> Dist xs
    = Dist
    [ f x :!: (fp * xp)
    | (f :!: fp) <- fs
    , (x :!: xp) <- xs ]
  {-# INLINE (<*>) #-}

instance Prelude.Monad Dist where
  Dist xs >>= f
    = Dist
    [ y :!: (xp * yp)
    | (x :!: xp) <- xs
    , (y :!: yp) <- runDist (f x) ]
  {-# INLINE (>>=) #-}

instance Functor Prob where
    type Suitable Prob a = Ord a
    fmap f (Prob xs) = Prob (Map.mapKeysWith (+) f xs)
    {-# INLINE fmap #-}

type instance Unconstrained Prob = Dist

instance Applicative Prob where
    eta = Dist . map (uncurry (:!:)) . Map.toList . runProb
    {-# INLINE eta #-}
    phi = Prob . Map.fromListWith (+) . map (\(x :!: y) -> (x,y)) . runDist
    {-# INLINE phi #-}
    _ *> x = x
    {-# INLINE (*>) #-}
    x <* _ = x
    {-# INLINE (<*) #-}

scaled :: Prob a -> Double -> Prob a
scaled (Prob xs) n = Prob (Map.map (n*) xs)
{-# INLINE scaled #-}

instance Monad Prob where
    Prob xs >>= f = Map.foldMapWithKey (scaled . f) xs
    {-# INLINE (>>=) #-}

instance (Ord a) =>
         Monoid (Prob a) where
    mempty = Prob Map.empty
    {-# INLINE mempty #-}
    mappend (Prob xs) (Prob ys) = Prob (Map.unionWith (+) xs ys)
    {-# INLINE mappend #-}

instance Foldable Prob where
    foldMap f (Prob xs) =
        Map.foldMapWithKey
            (\k _ ->
                  f k)
            xs

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
