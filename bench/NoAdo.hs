{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds        #-}

module NoAdo where

import           Control.Monad.Constrained
import           Data.Set
import           EnumVect
import           Prob
import           Numeric.Sized.WordOfSize

sumThriceNoAdo :: [Int] -> Int
sumThriceNoAdo xs = size $ do
  a <- fromList' xs
  b <- fromList' xs
  c <- upTo' (a + b)
  d <- fromList' xs
  e <- fromList' xs
  pure (c + e + d)
  where
    upTo' n = fromList [1..n]
    fromList' = fromList

diceNoAdo :: Int -> [Int] -> Double
diceNoAdo n die' = probOf n $ do
  a <- die
  b <- die
  c <- upTo' (a + b)
  d <- die
  e <- die
  pure (c + e + d)
  where
    die = uniform die'
    upTo' = upTo

diceVectNoAdo :: WordOfSize 3 -> [WordOfSize 3] -> Double
diceVectNoAdo n die' = probOfV n $ do
  a <- die
  b <- upTo' a
  c <- die
  d <- upTo' c
  pure (b + d)
  where
    die = uniformV die'
    upTo' = upToV
