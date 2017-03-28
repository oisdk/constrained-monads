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
        [ env (pure ([1 .. 7], [1 .. 4], [1])) $
          \ ~(xs,ys,zs) ->
               bgroup
                   "set"
                   [ bench "Applicative rewriting, Final encoding" $
                     whnf (sumThriceAdoFinal xs ys) zs
                   ,
                     -- , bench "Applicative Rewriting Initial    " $ whnf (sumThriceApplicative RewritingInitial     xs ys) zs
                     bench
                         "Applicative rewriting, Constrained encoding" $
                     whnf (sumThriceAdoConstrained xs ys) zs
                   , bench "Applicative rewriting, Codensity encoding" $
                     whnf (sumThriceAdoCodensity xs ys) zs
                   , bench "No rewriting             " $
                     whnf (sumThriceNoAdo xs ys) zs]
        , env (pure ([1 .. 6], 30)) $
          \ ~(xs,n) ->
               bgroup
                   "probabilistic inference map"
                   [ bench "Applicative Rewriting, Final encoding" $
                     whnf (diceAdoFinal n) xs
                   , bench "Applicative Rewriting, Initial encoding" $
                     whnf (diceAdoInitial n) xs
                   , bench "Applicative Rewriting, Constrained encoding" $
                     whnf (diceAdoConstrained n) xs
                   , bench "Applicative Rewriting, Codensity encoding" $
                     whnf (diceAdoCodensity n) xs
                   , bench "No rewriting             " $ whnf (diceNoAdo n) xs]
        , env (pure ([1 .. 6], 30)) $
          \ ~(xs,n) ->
               bgroup
                   "probabilistic inference vect"
                   [ bench "Applicative rewriting, Initial encoding" $
                     whnf (diceVectAdoInitial n) xs
                   ,
                     -- , bench "Applicative Rewriting Codensity  " $ whnf (diceVectApplicative RewritingCodensity   n) xs
                     bench
                         "No rewriting" $
                     whnf (diceVectNoAdo n) xs]]
