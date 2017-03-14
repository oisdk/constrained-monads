{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE RebindableSyntax #-}

module MuchAdo where

import           Control.Monad.Constrained.Ap
import           Data.Set
import           Prob

sumThriceAdoFinal :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdoFinal xs ys zs = size . lowerFinal $ do
  x <- liftFinal (fromList xs)
  a <- liftFinal (fromList [0..x])
  y <- liftFinal (fromList ys)
  z <- liftFinal (fromList zs)
  t <- liftFinal (fromList xs)
  u <- liftFinal (fromList ys)
  b <- liftFinal (fromList [0..y])
  c <- liftFinal (fromList [0..z])
  g <- liftFinal (fromList xs)
  h <- liftFinal (fromList ys)
  v <- liftFinal (fromList zs)
  pure (x + a + y + b + z + c + t + u + v + g + h)

sumThriceAdoConstrained :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdoConstrained xs ys zs = size . lowerConstrained $ do
  x <- liftConstrained (fromList xs)
  a <- liftConstrained (fromList [0..x])
  y <- liftConstrained (fromList ys)
  z <- liftConstrained (fromList zs)
  t <- liftConstrained (fromList xs)
  u <- liftConstrained (fromList ys)
  b <- liftConstrained (fromList [0..y])
  c <- liftConstrained (fromList [0..z])
  g <- liftConstrained (fromList xs)
  h <- liftConstrained (fromList ys)
  v <- liftConstrained (fromList zs)
  pure (x + a + y + b + z + c + t + u + v + g + h)

sumThriceAdoCodensity :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdoCodensity xs ys zs = size . lowerCodensity $ do
  x <- liftCodensity (fromList xs)
  a <- liftCodensity (fromList [0..x])
  y <- liftCodensity (fromList ys)
  z <- liftCodensity (fromList zs)
  t <- liftCodensity (fromList xs)
  u <- liftCodensity (fromList ys)
  b <- liftCodensity (fromList [0..y])
  c <- liftCodensity (fromList [0..z])
  g <- liftCodensity (fromList xs)
  h <- liftCodensity (fromList ys)
  v <- liftCodensity (fromList zs)
  pure (x + a + y + b + z + c + t + u + v + g + h)

diceAdoFinal :: Integer -> [Integer] -> Double
diceAdoFinal n die' = probOf n . lowerFinal $ do
  t <- die
  a <- liftFinal (upTo t)
  u <- die
  w <- die
  x <- die
  v <- liftFinal (upTo u)
  y <- liftFinal (upTo w)
  z <- die
  pure (a + t + u + x + y + z + v + w)
  where die = liftFinal (uniform die')

diceAdoConstrained :: Integer -> [Integer] -> Double
diceAdoConstrained n die' = probOf n . lowerConstrained $ do
  t <- die
  a <- liftConstrained (upTo t)
  u <- die
  w <- die
  x <- die
  v <- liftConstrained (upTo u)
  y <- liftConstrained (upTo w)
  z <- die
  pure (a + t + u + x + y + z + v + w)
  where die = liftConstrained (uniform die')

diceAdoCodensity :: Integer -> [Integer] -> Double
diceAdoCodensity n die' = probOf n . lowerCodensity $ do
  t <- die
  a <- liftCodensity (upTo t)
  u <- die
  w <- die
  x <- die
  v <- liftCodensity (upTo u)
  y <- liftCodensity (upTo w)
  z <- die
  pure (a + t + u + x + y + z + v + w)
  where die = liftCodensity (uniform die')
