{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module MuchAdo where

import           Control.Monad.Constrained.Ap
import           Data.Set
import           Prob
import           EnumVect
import           Numeric.Sized.WordOfSize

sumThriceAdoFinal :: [Int] -> Int
sumThriceAdoFinal xs = size . phi @ Final $ do
  a <- fromList' xs
  b <- fromList' xs
  c <- upTo' (a + b)
  d <- fromList' xs
  e <- fromList' xs
  pure (c + e + d)
  where
    upTo' = eta . fromDistinctAscList . enumFromTo 1
    fromList' = eta . fromList

sumThriceAdoInitial :: [Int] -> Int
sumThriceAdoInitial xs = size . phi @ Initial $ do
  a <- fromList' xs
  b <- fromList' xs
  c <- upTo' (a + b)
  d <- fromList' xs
  e <- fromList' xs
  pure (c + e + d)
  where
    upTo' = eta . fromDistinctAscList . enumFromTo 1
    fromList' = eta . fromList

sumThriceAdoConstrained :: [Int] -> Int
sumThriceAdoConstrained xs = size . phi @ ConstrainedWrapper $ do
  a <- fromList' xs
  b <- fromList' xs
  c <- upTo' (a + b)
  d <- fromList' xs
  e <- fromList' xs
  pure (c + e + d)
  where
    upTo' = eta . fromDistinctAscList . enumFromTo 1
    fromList' = eta . fromList

sumThriceAdoCodensity :: [Int] -> Int
sumThriceAdoCodensity xs = size . phi @ Codensity $ do
  a <- fromList' xs
  b <- fromList' xs
  c <- upTo' (a + b)
  d <- fromList' xs
  e <- fromList' xs
  pure (c + e + d)
  where
    upTo' = eta . fromDistinctAscList . enumFromTo 1
    fromList' = eta . fromList

diceAdoFinal :: Int -> [Int] -> Double
diceAdoFinal n die' = probOf n . phi @ Final $ do
  a <- die
  b <- die
  c <- upTo' (a + b)
  d <- die
  e <- die
  pure (c + e + d)
  where
    die = eta (uniform die')
    upTo' = eta . upTo

diceAdoInitial :: Int -> [Int] -> Double
diceAdoInitial n die' = probOf n . phi @ Initial $ do
  a <- die
  b <- die
  c <- upTo' (a + b)
  d <- die
  e <- die
  pure (c + e + d)
  where
    die = eta (uniform die')
    upTo' = eta . upTo

diceAdoConstrained :: Int -> [Int] -> Double
diceAdoConstrained n die' = probOf n . phi @ ConstrainedWrapper $ do
  a <- die
  b <- die
  c <- upTo' (a + b)
  d <- die
  e <- die
  pure (c + e + d)
  where
    die = eta (uniform die')
    upTo' = eta . upTo

diceAdoCodensity :: Int -> [Int] -> Double
diceAdoCodensity n die' = probOf n . phi @ Codensity $ do
  a <- die
  b <- die
  c <- upTo' (a + b)
  d <- die
  e <- die
  pure (c + e + d)
  where
    die = eta (uniform die')
    upTo' = eta . upTo

diceVectAdoInitial :: WordOfSize 3 -> [WordOfSize 3] -> Double
diceVectAdoInitial n die' = probOfV n . phi @ Initial $ do
  a <- die
  b <- upTo' a
  c <- die
  d <- upTo' c
  pure (b + d)
  where
    die = eta (uniformV die')
    upTo' = eta . upToV

diceVectAdoCodensity :: WordOfSize 3 -> [WordOfSize 3] -> Double
diceVectAdoCodensity n die' = probOfV n . phi @ Codensity $ do
  a <- die
  b <- upTo' a
  c <- die
  pure (b + c)
  where
    die = eta (uniformV die')
    upTo' = eta . upToV
