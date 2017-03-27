module Main (main) where

import MuchAdo
import NoAdo

import Criterion.Main

import Control.DeepSeq

import GHC.TypeLits

import Numeric.Sized.WordOfSize

instance KnownNat n => NFData (WordOfSize n)

main :: IO ()
main =
    defaultMain
        [ env (pure ([1..7],[1..4],[1])) $
          \ ~(xs,ys,zs) ->
               bgroup
                   "set"
                   [ bench "Ado Final      " $ whnf (sumThriceAdoFinal       xs ys) zs
                   -- , bench "Ado Initial    " $ whnf (sumThriceAdoInitial     xs ys) zs
                   , bench "Ado Constrained" $ whnf (sumThriceAdoConstrained xs ys) zs
                   , bench "Ado Codensity  " $ whnf (sumThriceAdoCodensity   xs ys) zs
                   , bench "Do             " $ whnf (sumThriceNoAdo          xs ys) zs]
        , env (pure ([1..6],30)) $
          \ ~(xs,n) ->
               bgroup
                   "prob map"
                   [ bench "Ado Final      " $ whnf (diceAdoFinal       n) xs
                   , bench "Ado Initial    " $ whnf (diceAdoInitial     n) xs
                   , bench "Ado Constrained" $ whnf (diceAdoConstrained n) xs
                   , bench "Ado Codensity  " $ whnf (diceAdoCodensity   n) xs
                   , bench "Do             " $ whnf (diceNoAdo          n) xs]
        , env (pure ([1..6],30)) $
          \ ~(xs,n) ->
               bgroup
                   "prob vect"
                   [ bench "Ado Initial    " $ whnf (diceVectAdoInitial     n) xs
                   -- , bench "Ado Codensity  " $ whnf (diceVectAdoCodensity   n) xs
                   , bench "Do             " $ whnf (diceVectNoAdo          n) xs ]
        ]
