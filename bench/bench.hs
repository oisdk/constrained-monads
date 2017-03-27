module Main (main) where

import MuchAdo
import NoAdo

import Criterion.Main

main :: IO ()
main =
    defaultMain
        [ env (pure ([1..6],30)) $
          \ ~(xs,n) ->
               bgroup
                   "prob"
                   [ bench "Ado Final      " $ whnf (diceAdoFinal       n) xs
                   , bench "Ado Constrained" $ whnf (diceAdoConstrained n) xs
                   , bench "Ado Codensity  " $ whnf (diceAdoCodensity   n) xs
                   , bench "Do             " $ whnf (diceNoAdo          n) xs]
        , env (pure ([1..7],[1..4],[1])) $
          \ ~(xs,ys,zs) ->
               bgroup
                   "set"
                   [ bench "Ado Final      " $ whnf (sumThriceAdoFinal       xs ys) zs
                   , bench "Ado Constrained" $ whnf (sumThriceAdoConstrained xs ys) zs
                   , bench "Ado Codensity  " $ whnf (sumThriceAdoCodensity   xs ys) zs
                   , bench "Do             " $ whnf (sumThriceNoAdo          xs ys) zs]
        ]
