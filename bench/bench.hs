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
        [ env (pure ([1 .. 6], 8)) $
          \ ~(xs,n) ->
               bgroup
                   "probabilistic inference map"
                   [ bench "Applicative rewriting, Final encoding"       $ whnf (diceAdoFinal       n) xs
                   , bench "Applicative rewriting, Initial encoding"     $ whnf (diceAdoInitial     n) xs
                   , bench "Applicative rewriting, Constrained encoding" $ whnf (diceAdoConstrained n) xs
                   , bench "Applicative rewriting, Codensity encoding"   $ whnf (diceAdoCodensity   n) xs
                   , bench "No rewriting"                                $ whnf (diceNoAdo          n) xs]
        , env (pure [1 .. 5]) $
          \xs ->
               bgroup
                   "set"
                   [ bench "Applicative rewriting, Final encoding"       $ whnf sumThriceAdoFinal       xs
                   , bench "Applicative rewriting, Initial encoding"     $ whnf sumThriceAdoInitial     xs
                   , bench "Applicative rewriting, Constrained encoding" $ whnf sumThriceAdoConstrained xs
                   , bench "Applicative rewriting, Codensity encoding"   $ whnf sumThriceAdoCodensity   xs
                   , bench "No rewriting"                                $ whnf sumThriceNoAdo          xs]
        , env (pure ([1 .. 5], 30)) $
          \ ~(xs,n) ->
               bgroup
                   "probabilistic inference vect"
                   [ bench "Applicative rewriting, Initial encoding"  $ whnf (diceVectAdoInitial   n) xs
                   , bench "Applicative rewriting Codensity encoding" $ whnf (diceVectAdoCodensity n) xs
                   , bench "No rewriting"                             $ whnf (diceVectNoAdo        n) xs]]
