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
sumThriceAdoFinal xs = size . retractAp @ Final $ do
  a <- fromList' xs
  b <- fromList' xs
  c <- upTo' (a + b)
  d <- fromList' xs
  e <- fromList' xs
  pure (c + e + d)
  where
    upTo' = liftAp . fromDistinctAscList . enumFromTo 1
    fromList' = liftAp . fromList

sumThriceAdoInitial :: [Int] -> Int
sumThriceAdoInitial xs = size . retractAp @ Initial $ do
  a <- fromList' xs
  b <- fromList' xs
  c <- upTo' (a + b)
  d <- fromList' xs
  e <- fromList' xs
  pure (c + e + d)
  where
    upTo' = liftAp . fromDistinctAscList . enumFromTo 1
    fromList' = liftAp . fromList

sumThriceAdoConstrained :: [Int] -> Int
sumThriceAdoConstrained xs = size . retractAp @ ConstrainedWrapper $ do
  a <- fromList' xs
  b <- fromList' xs
  c <- upTo' (a + b)
  d <- fromList' xs
  e <- fromList' xs
  pure (c + e + d)
  where
    upTo' = liftAp . fromDistinctAscList . enumFromTo 1
    fromList' = liftAp . fromList

sumThriceAdoCodensity :: [Int] -> Int
sumThriceAdoCodensity xs = size . retractAp @ Codensity $ do
  a <- fromList' xs
  b <- fromList' xs
  c <- upTo' (a + b)
  d <- fromList' xs
  e <- fromList' xs
  pure (c + e + d)
  where
    upTo' = liftAp . fromDistinctAscList . enumFromTo 1
    fromList' = liftAp . fromList

diceAdoFinal :: Int -> [Int] -> Double
diceAdoFinal n die' = probOf n . retractAp @ Final $ do
  a <- die
  b <- die
  c <- upTo' (a + b)
  d <- die
  e <- die
  pure (c + e + d)
  where
    die = liftAp (uniform die')
    upTo' = liftAp . upTo

diceAdoInitial :: Int -> [Int] -> Double
diceAdoInitial n die' = probOf n . retractAp @ Initial $ do
  a <- die
  b <- die
  c <- upTo' (a + b)
  d <- die
  e <- die
  pure (c + e + d)
  where
    die = liftAp (uniform die')
    upTo' = liftAp . upTo

diceAdoConstrained :: Int -> [Int] -> Double
diceAdoConstrained n die' = probOf n . retractAp @ ConstrainedWrapper $ do
  a <- die
  b <- die
  c <- upTo' (a + b)
  d <- die
  e <- die
  pure (c + e + d)
  where
    die = liftAp (uniform die')
    upTo' = liftAp . upTo

diceAdoCodensity :: Int -> [Int] -> Double
diceAdoCodensity n die' = probOf n . retractAp @ Codensity $ do
  a <- die
  b <- die
  c <- upTo' (a + b)
  d <- die
  e <- die
  pure (c + e + d)
  where
    die = liftAp (uniform die')
    upTo' = liftAp . upTo

diceVectAdoInitial :: WordOfSize 3 -> [WordOfSize 3] -> Double
diceVectAdoInitial n die' = probOfV n . retractAp @ Initial $ do
  a <- die
  b <- upTo' a
  c <- die
  d <- upTo' c
  pure (b + d)
  where
    die = liftAp (uniformV die')
    upTo' = liftAp . upToV

diceVectAdoCodensity :: WordOfSize 3 -> [WordOfSize 3] -> Double
diceVectAdoCodensity n die' = probOfV n . retractAp @ Codensity $ do
  a <- die
  b <- upTo' a
  c <- die
  pure (b + c)
  where
    die = liftAp (uniformV die')
    upTo' = liftAp . upToV
