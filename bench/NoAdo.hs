{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds        #-}

module NoAdo where

import           Control.Monad.Constrained
import           Data.Set
import           EnumVect
import           Prob
import           Numeric.Sized.WordOfSize

sumThriceNoAdo :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceNoAdo xs ys zs = size $ do
  x <- fromList xs
  a <- fromList [0..x]
  y <- fromList ys
  z <- fromList zs
  t <- fromList xs
  u <- fromList ys
  b <- fromList [0..y]
  c <- fromList [0..z]
  g <- fromList xs
  h <- fromList ys
  v <- fromList zs
  pure (x + a + y + b + z + c + t + u + v + g + h)

diceNoAdo :: Integer -> [Integer] -> Double
diceNoAdo n die' = probOf n $ do
  t <- die
  a <- upTo t
  u <- die
  w <- die
  x <- die
  v <- upTo u
  y <- upTo w
  z <- die
  pure (a + t + u + x + y + z + v + w)
  where die = uniform die'

diceVectNoAdo :: WordOfSize 3 -> [WordOfSize 3] -> Double
diceVectNoAdo n die' = probOfV n $ do
  t <- die
  a <- upToV t
  u <- die
  w <- die
  v <- upToV u
  y <- upToV w
  pure (a + t + u + y + v + w)
  where die = uniformV die'
