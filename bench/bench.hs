module Main (main) where

import MuchAdo
import NoAdo

import Criterion.Main

xs, ys, zs :: [Integer]
xs = [1..5]
ys = [1..5]
zs = [1..5]

main :: IO ()
main = defaultMain
  [
    bgroup "prob" [ bench "Ado" $ whnf (diceAdo   30) [1..6]
                  , bench "Do"  $ whnf (diceNoAdo 30) [1..6]
                  ]
  , bgroup "set"  [ bench "Ado" $ whnf (sumThriceAdo   xs ys) zs
                  , bench "Do"  $ whnf (sumThriceNoAdo xs ys) zs
                  ]
  ]
