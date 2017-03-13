{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ApplicativeDo    #-}

module MuchAdo where

import Data.Set
import Control.Monad.Constrained.Ap

sumThriceAdo :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceAdo xs ys zs = size . lower $ do
  x <- liftAp (fromList xs)
  y <- liftAp (fromList ys)
  z <- liftAp (fromList zs)
  t <- liftAp (fromList xs)
  u <- liftAp (fromList ys)
  v <- liftAp (fromList zs)
  a <- liftAp (fromList [0..x])
  b <- liftAp (fromList [0..y])
  c <- liftAp (fromList [0..z])
  pure (x + a + y + b + z + c + t + u + v)
