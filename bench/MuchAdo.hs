{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE RebindableSyntax #-}

module MuchAdo where

import           Control.Monad.Constrained.Ap
import           Data.Set
import           Prob

sumThriceAdo :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdo xs ys zs = size . lowerFinal $ do
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

diceAdo :: Integer -> [Integer] -> Double
diceAdo n die' = probOf n . lowerFinal $ do
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
