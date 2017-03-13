{-# LANGUAGE RebindableSyntax #-}

module NoAdo where

import Data.Set
import Control.Monad.Constrained

sumThriceNoAdo :: [Integer] -> [Integer] -> [Integer] -> Int
sumThriceNoAdo xs ys zs = size $ do
  x <- fromList xs
  y <- fromList ys
  z <- fromList zs
  t <- fromList xs
  u <- fromList ys
  v <- fromList zs
  a <- fromList [0..x]
  b <- fromList [0..y]
  c <- fromList [0..z]
  pure (x + a + y + b + z + c + t + u + v)
