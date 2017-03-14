{-# LANGUAGE RebindableSyntax #-}


module NoAdo where

import           Control.Monad.Constrained
import           Data.Set
import           Prob

sumThriceNoAdo :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceNoAdo xs ys zs = size $ do
  x <- fromList xs
  y <- fromList ys
  z <- fromList zs
  t <- fromList xs
  u <- fromList ys
  a <- fromList [0..x]
  b <- fromList [0..y]
  c <- fromList [0..z]
  g <- fromList xs
  h <- fromList ys
  v <- fromList zs
  pure (x + a + y + b + z + c + t + u + v + g + h)

diceNoAdo :: Integer -> [Integer] -> Double
diceNoAdo n die' = probOf n $ do
  t <- die
  u <- die
  w <- die
  x <- die
  a <- upTo t
  v <- upTo u
  y <- die
  z <- die
  pure (a + t + u + x + y + z + v + w)
  where die = uniform die'
