{-# LANGUAGE RebindableSyntax, GADTs, TypeFamilies, ScopedTypeVariables #-}

module EnumVect where

import Data.Vector.Unboxed hiding (foldl')
import Data.Foldable (foldl')
import Data.Ix
import Control.Monad.Constrained hiding (replicate, zipWith)
import Prob
import qualified Prelude

data EnumVect a where
  EnumVect :: (Ix a, Bounded a) => Vector Double -> EnumVect a

runEnumVect :: EnumVect a -> Vector Double
runEnumVect (EnumVect xs) = xs
{-# INLINE runEnumVect #-}

instance Functor EnumVect where
    type Suitable EnumVect a = (Ix a, Bounded a)
    fmap (f :: a -> b) (EnumVect xs :: EnumVect a) =
        EnumVect $
        accum
            (+)
            (replicate (rangeSize (minBound :: b,maxBound)) 0)
            [ (index (minBound,maxBound) (f x), xs ! index (minBound,maxBound) x)
            | x <- range (minBound, maxBound) ]
    {-# INLINE fmap #-}


instance Applicative EnumVect where
    type Unconstrained EnumVect = Dist
    reflect (EnumVect xs) =
        Dist (Prelude.zip (range (minBound, maxBound)) (toList xs))
    reify (Dist (xs :: [(a, Double)])) =
        EnumVect $
        accum
            (+)
            (replicate (rangeSize (minBound :: a, maxBound)) 0)
            [ (index (minBound, maxBound) x, p)
            | (x,p) <- xs ]
    {-# INLINE reflect #-}
    {-# INLINE reify #-}

instance Monad EnumVect where
    EnumVect xs >>= (f :: a -> EnumVect b) =
        EnumVect $
        foldl'
            g
            (replicate (rangeSize (minBound :: b, maxBound)) 0)
            (range (minBound, maxBound))
      where
        g acc e =
            let fac = xs ! index (minBound, maxBound) e
            in zipWith
                   (\accm n ->
                         accm + fac * n)
                   acc
                   (runEnumVect (f e))
        {-# INLINE g #-}
    {-# INLINE (>>=) #-}

probOfV :: a -> EnumVect a -> Double
probOfV x (EnumVect xs) = xs ! index (minBound,maxBound) x
{-# INLINE probOfV #-}

uniformV
    :: (Ix a, Bounded a)
    => [a] -> EnumVect a
uniformV (xs :: [a]) =
    EnumVect $
    accum
        (+)
        (replicate (rangeSize (minBound :: a,maxBound)) 0)
        [ (index (minBound,maxBound) x, sz)
        | x <- xs ]
        where sz = 1 / fromIntegral (Prelude.length xs)
{-# INLINE uniformV #-}

upToV :: (Integral a, Bounded a, Ix a) => a -> EnumVect a
upToV (n :: a) =
    EnumVect $
    accum
        (+)
        (replicate (rangeSize (minBound :: a,maxBound)) 0)
        [ (index (minBound,maxBound) x, 1 / fromIntegral n)
        | x <- [1 .. n] ]
{-# INLINE upToV #-}
