module Main (main) where

import MuchAdo
import NoAdo

import Criterion.Main

xs, ys, zs :: [Integer]
xs = [1..4]
ys = [1..5]
zs = [1..5]

main :: IO ()
main = defaultMain
  [
    bgroup "prob" [ bench "Ado Final"       $ whnf (diceAdoFinal       30) [1..6]
                  , bench "Ado Constrained" $ whnf (diceAdoConstrained 30) [1..6]
                  , bench "Ado Codensity"   $ whnf (diceAdoCodensity   30) [1..6]
                  , bench "Do" $ whnf (diceNoAdo 30) [1..6]
                  ]
  , bgroup "set"  [ bench "Ado Final"       $ whnf (sumThriceAdoFinal       xs ys) zs
                  , bench "Ado Constrained" $ whnf (sumThriceAdoConstrained xs ys) zs
                  , bench "Ado Codensity"   $ whnf (sumThriceAdoCodensity   xs ys) zs
                  , bench "Do"  $ whnf (sumThriceNoAdo xs ys) zs
                  ]
  ]
