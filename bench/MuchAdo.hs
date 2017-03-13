{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE RebindableSyntax #-}

module MuchAdo where

import           Control.Monad.Constrained.Ap
import           Data.Set
import           Prob

sumThriceAdo :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdo xs ys zs = size . lower $ do
  x <- liftAp (fromList xs)
  y <- liftAp (fromList ys)
  z <- liftAp (fromList zs)
  t <- liftAp (fromList xs)
  u <- liftAp (fromList ys)
  a <- liftAp (fromList [0..x])
  b <- liftAp (fromList [0..y])
  c <- liftAp (fromList [0..z])
  v <- liftAp (fromList zs)
  pure (x + a + y + b + z + c + t + u + v)

diceAdo :: Integer -> [Integer] -> Double
diceAdo n die' = probOf n . lower $ do
  t <- die
  u <- die
  w <- die
  x <- die
  a <- liftAp (upTo t)
  v <- liftAp (upTo u)
  y <- die
  z <- die
  pure (a + t + u + x + y + z + v + w)
  where die = liftAp (uniform die')
