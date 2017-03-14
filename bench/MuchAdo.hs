{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE RebindableSyntax #-}

module MuchAdo where

import           Control.Monad.Constrained.Ap
import           Data.Set
import           Prob

sumThriceAdoFinal :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdoFinal xs ys zs = size . lowerFinal $ do
  x <- liftFinal (fromList xs)
  y <- liftFinal (fromList ys)
  z <- liftFinal (fromList zs)
  t <- liftFinal (fromList xs)
  u <- liftFinal (fromList ys)
  a <- liftFinal (fromList [0..x])
  b <- liftFinal (fromList [0..y])
  c <- liftFinal (fromList [0..z])
  g <- liftFinal (fromList xs)
  h <- liftFinal (fromList ys)
  v <- liftFinal (fromList zs)
  pure (x + a + y + b + z + c + t + u + v + g + h)

sumThriceAdoConstrained :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdoConstrained xs ys zs = size . lowerConstrained $ do
  x <- liftConstrained (fromList xs)
  y <- liftConstrained (fromList ys)
  z <- liftConstrained (fromList zs)
  t <- liftConstrained (fromList xs)
  u <- liftConstrained (fromList ys)
  a <- liftConstrained (fromList [0..x])
  b <- liftConstrained (fromList [0..y])
  c <- liftConstrained (fromList [0..z])
  g <- liftConstrained (fromList xs)
  h <- liftConstrained (fromList ys)
  v <- liftConstrained (fromList zs)
  pure (x + a + y + b + z + c + t + u + v + g + h)

sumThriceAdoCodensity :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdoCodensity xs ys zs = size . lowerCodensity $ do
  x <- liftCodensity (fromList xs)
  y <- liftCodensity (fromList ys)
  z <- liftCodensity (fromList zs)
  t <- liftCodensity (fromList xs)
  u <- liftCodensity (fromList ys)
  a <- liftCodensity (fromList [0..x])
  b <- liftCodensity (fromList [0..y])
  c <- liftCodensity (fromList [0..z])
  g <- liftCodensity (fromList xs)
  h <- liftCodensity (fromList ys)
  v <- liftCodensity (fromList zs)
  pure (x + a + y + b + z + c + t + u + v + g + h)

diceAdoFinal :: Integer -> [Integer] -> Double
diceAdoFinal n die' = probOf n . lowerFinal $ do
  t <- die
  u <- die
  w <- die
  x <- die
  a <- liftFinal (upTo t)
  v <- liftFinal (upTo u)
  y <- die
  z <- die
  pure (a + t + u + x + y + z + v + w)
  where die = liftFinal (uniform die')

diceAdoConstrained :: Integer -> [Integer] -> Double
diceAdoConstrained n die' = probOf n . lowerConstrained $ do
  t <- die
  u <- die
  w <- die
  x <- die
  a <- liftConstrained (upTo t)
  v <- liftConstrained (upTo u)
  y <- die
  z <- die
  pure (a + t + u + x + y + z + v + w)
  where die = liftConstrained (uniform die')

diceAdoCodensity :: Integer -> [Integer] -> Double
diceAdoCodensity n die' = probOf n . lowerCodensity $ do
  t <- die
  u <- die
  w <- die
  x <- die
  a <- liftCodensity (upTo t)
  v <- liftCodensity (upTo u)
  y <- die
  z <- die
  pure (a + t + u + x + y + z + v + w)
  where die = liftCodensity (uniform die')
