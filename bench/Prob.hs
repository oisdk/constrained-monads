{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE Strict           #-}

module Prob where

import           Control.Monad.Constrained

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map

newtype Prob s a
  = Prob
  { runProb :: Map a s
  } deriving (Show, Eq, Ord)

instance Num s =>
         Functor (Prob s) where
    type Suitable (Prob s) a = Ord a
    fmap f (Prob xs) = Prob (Map.mapKeysWith (+) f xs)

instance Num s =>
         Applicative (Prob s) where
    lower = Prob . Map.fromListWith (+) . go
      where
        go :: forall a s. Num s => Ap (Prob s) a -> [(a, s)]
        go (Pure x) = [(x, 1)]
        go (Ap fs xs) =
            [ (f x, fp * xp)
            | (!f,!fp) <- go fs
            , (!x,!xp) <- (Map.toList . runProb) xs ]
        {-# INLINE go #-}
    {-# INLINE lower #-}
    _ *> x = x
    x <* _ = x

scaled :: Num s => Prob s a -> s -> Prob s a
scaled (Prob xs) n = Prob (fmap (n*) xs)

instance Num s =>
         Monad (Prob s) where
    Prob xs >>= f = Map.foldMapWithKey (scaled . f) xs

instance (Num s, Ord a) =>
         Monoid (Prob s a) where
    mempty = Prob Map.empty
    mappend (Prob xs) (Prob ys) = Prob (Map.unionWith (+) xs ys)

instance Foldable (Prob s) where
    foldMap f (Prob xs) =
        Map.foldMapWithKey
            (\k _ ->
                  f k)
            xs

uniform
    :: (Fractional s, Ord a)
    => [a] -> Prob s a
uniform xs =
    (Prob . Map.fromListWith (+) . map (flip (,) (1 / fromIntegral l))) xs
  where
    l = length xs

upTo :: (Fractional s, Integral a) => a -> Prob s a
upTo n = uniform [1..n]

eighths :: Prob Double Integer
eighths = uniform [1..8]

probOf
    :: (Num s, Ord a)
    => a -> Prob s a -> s
probOf x (Prob xs) = Map.findWithDefault 0 x xs
