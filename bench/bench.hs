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
  [ bgroup "basic" [ bench "Ado" $ whnf (sumThriceNoAdo xs ys) zs
                   , bench "Do"  $ whnf (sumThriceAdo   xs ys) zs
                   ]
  ]
