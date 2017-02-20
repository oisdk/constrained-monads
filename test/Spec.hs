{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           Control.Monad.Constrained
import qualified Prelude

import           Test.DocTest
import           Test.QuickCheck

import           Data.Proxy

instance Functor Gen where
  type Suitable Gen a = ()
  fmap = Prelude.fmap
  (<$) = (Prelude.<$)

instance Applicative Gen where
  liftA = liftAP

instance Monad Gen where
  (>>=) = (Prelude.>>=)

fmapIsSame
    :: (Functor f, Prelude.Functor f, Suitable f b, Eq (f b), Show (f b))
    => Blind (a -> b) -> f a -> Property
fmapIsSame (Blind f) x = fmap f x === Prelude.fmap f x

{-# ANN fmapLaw "HLint: ignore Functor law" #-}
fmapLaw
    :: (Applicative f, Suitable f a, Eq (f a), Show (f a))
    => f a -> Property
fmapLaw xs = fmap id xs === xs

{-# ANN fmapCompLaw "HLint: ignore Functor law" #-}
fmapCompLaw
    :: (Functor f, Suitable f c, Eq (f c), Show (f c), Suitable f b)
    => Blind (b -> c) -> Blind (a -> b) -> f a -> Property
fmapCompLaw (Blind f) (Blind g) xs = fmap (f . g) xs === (fmap f . fmap g) xs

appIdLaw
    :: (Applicative f, Suitable f a, Suitable f (a -> a), Eq (f a), Show (f a))
    => f a -> Property
appIdLaw xs = (pure id <*> xs) === xs

appCompLaw
    :: (Applicative f
       ,Suitable f ((b -> c) -> (a -> b) -> a -> c)
       ,Suitable f ((a -> b) -> a -> c)
       ,Suitable f (a -> c)
       ,Suitable f c
       ,Suitable f b
       ,Eq (f c)
       ,Show (f c))
    => Blind (f (b -> c)) -> Blind (f (a -> b)) -> f a -> Property
appCompLaw (Blind u) (Blind v) w
  = (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

homomorphismLaw
    :: (Suitable f (a -> b)
       ,Suitable f b
       ,Suitable f a
       ,Applicative f
       ,Eq (f b)
       ,Show (f b))
    => Proxy f -> Blind (a -> b) -> a -> Property
homomorphismLaw (_ :: Proxy f) (Blind (f :: a -> b)) x
  = (pure f <*> pure x) === (pure (f x) :: f b)

interchangeLaw
    :: (Applicative f
       ,Suitable f a
       ,Suitable f b
       ,Suitable f ((a -> b) -> b)
       ,Eq (f b)
       ,Show (f b))
    => Blind (f (a -> b)) -> a -> Property
interchangeLaw (Blind u) y = (u <*> pure y) === (pure ($y) <*> u)

monadLawOne
    :: (Monad f, Suitable f a, Suitable f b, Show (f b), Eq (f b))
    => Blind (a -> f b) -> a -> Property
monadLawOne (Blind k) a = (pure a >>= k) === k a

monadLawTwo
    :: (Monad f, Suitable f a, Eq (f a), Show (f a))
    => f a -> Property
monadLawTwo xs = (xs >>= pure) === xs

monadLawThree
    :: (Monad f, Suitable f c, Eq (f c), Suitable f b, Show (f c))
    => f a -> Blind (a -> f b) -> Blind (b -> f c) -> Property
monadLawThree m (Blind k) (Blind h) =
  (m >>= (k >=> h)) === ((m >>= k) >>= h)

main :: IO ()
main = do
  quickCheck (fmapIsSame @ [] @ Int @ Int)
  quickCheck (fmapLaw @ [] @ Int)
  quickCheck (appIdLaw @ [] @ Int)
  quickCheck (appCompLaw @ [] @ Integer @ Int @ Word)
  quickCheck (homomorphismLaw (Proxy :: Proxy []) :: Blind (Integer -> Word) -> Integer -> Property)
  quickCheck (interchangeLaw @ [] @ Word @ Int)
  quickCheck (monadLawOne @ [] @ Word @ Int)
  quickCheck (monadLawTwo @ [] @ Word)
  quickCheck (monadLawThree @ [] @ Word @ Int @ Integer)
  doctest
    [ "-isrc"
    , "src/" ]
