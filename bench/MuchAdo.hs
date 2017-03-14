{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE RebindableSyntax #-}

module MuchAdo where

import           Control.Monad.Constrained.Ap
import           Data.Set
import           Prob

sumThriceAdo :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdo xs ys zs = size . lower . runAp eta $ do
  x <- fLiftAp (fromList xs)
  y <- fLiftAp (fromList ys)
  z <- fLiftAp (fromList zs)
  t <- fLiftAp (fromList xs)
  u <- fLiftAp (fromList ys)
  a <- fLiftAp (fromList [0..x])
  b <- fLiftAp (fromList [0..y])
  c <- fLiftAp (fromList [0..z])
  v <- fLiftAp (fromList zs)
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
