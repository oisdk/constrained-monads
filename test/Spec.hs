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

import qualified Control.Applicative

import           Data.Set (Set)
import           Control.Monad.Constrained.IntSet (IntSet)

import           GHC.Exts (fromList)

instance Functor Gen where
  type Suitable Gen a = ()
  fmap = Prelude.fmap
  (<$) = (Prelude.<$)

instance Applicative Gen where
  liftA = liftAP

instance Monad Gen where
  (>>=) = (Prelude.>>=)

instance a ~ Int => Arbitrary (IntSet a) where
  arbitrary = fmap fromList arbitrary

fmapIsSame
    :: (Functor f, Prelude.Functor f, Suitable f b, Eq (f b), Show (f b))
    => Blind (a -> b) -> f a -> Property
fmapIsSame (Blind f) x = fmap f x === Prelude.fmap f x

replaceIsSame
    :: (Functor f, Prelude.Functor f, Suitable f a, Eq (f a), Show (f a))
    => f b -> a -> Property
replaceIsSame xs x = (x <$ xs) === (x Prelude.<$ xs)

pureIsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f a
       ,Eq (f a)
       ,Show (f a))
    => Proxy f -> a -> Property
pureIsSame (_ :: Proxy f) (x :: a) = (pure x :: f a) === Prelude.pure x

seqRightIsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f b
       ,Eq (f b)
       ,Show (f b))
    => f a -> f b -> Property
seqRightIsSame xs ys = (xs *> ys) === (xs Prelude.*> ys)

seqLeftIsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f b
       ,Eq (f b)
       ,Show (f b))
    => f b -> f a -> Property
seqLeftIsSame xs ys = (xs <* ys) === (xs Prelude.<* ys)

applyIsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f b
       ,Eq (f b)
       ,Show (f b))
    => Blind (f (a -> b)) -> f a -> Property
applyIsSame (Blind fs) xs = (fs <*> xs) === (fs Prelude.<*> xs)

liftA2IsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f c
       ,Eq (f c)
       ,Show (f c))
    => Blind (a -> b -> c) -> f a -> f b -> Property
liftA2IsSame (Blind f) xs ys =
    liftA2 f xs ys === Control.Applicative.liftA2 f xs ys

bindIsSame
    :: (Monad f, Prelude.Monad f, Suitable f b, Show (f b), Eq (f b))
    => f a -> Blind (a -> f b) -> Property
bindIsSame xs (Blind f) = (xs >>= f) === (xs Prelude.>>= f)

checkSame
    :: (Monad f
       ,Prelude.Monad f
       ,Show (f b)
       ,Show (f c)
       ,CoArbitrary c
       ,Arbitrary (f c)
       ,Arbitrary (f a)
       ,Arbitrary (f (b -> a))
       ,CoArbitrary b
       ,Arbitrary a
       ,Arbitrary (f b)
       ,Suitable f a
       ,Eq (f a)
       ,Show (f a)
       ,Show a)
    => Proxy f -> Proxy a -> Proxy b -> Proxy c -> IO ()
checkSame (_ :: Proxy f) (_ :: Proxy a) (_ :: Proxy b) (_ :: Proxy c) = do
  quickCheck (fmapIsSame @ f @ a @ b)
  quickCheck (replaceIsSame @ f @ a @ b)
  quickCheck (pureIsSame (Proxy :: Proxy f) :: a -> Property)
  quickCheck (seqRightIsSame @ f @ a @ b)
  quickCheck (seqLeftIsSame @ f @ a @ b)
  quickCheck (applyIsSame @ f @ a @ b)
  quickCheck (liftA2IsSame @ f @ a @ b @ c)
  quickCheck (bindIsSame @ f @ a @ b)

checkConstrained
    :: (Show (f a)
       ,Arbitrary (f a)
       ,Suitable f a
       ,Eq (f a)
       ,Show (f c)
       ,CoArbitrary c
       ,CoArbitrary b
       ,Arbitrary a
       ,Arbitrary (f c)
       ,Suitable f b
       ,Show (f b)
       ,Arbitrary (f b)
       ,Show a
       ,CoArbitrary a
       ,Eq (f b)
       ,Monad f
       ,Arbitrary b)
    => Proxy f -> Proxy a -> Proxy b -> Proxy c -> IO ()
checkConstrained (_ :: Proxy f) (_ :: Proxy a) (_ :: Proxy b) (_ :: Proxy c) = do
  quickCheck (fmapLaw @ f @ a)
  quickCheck (fmapCompLaw @ f @ a @ b @ c)
  quickCheck (seqRightLaw @ f @ a @ b)
  quickCheck (seqLeftLaw @ f @ a @ b)
  quickCheck (monadLawOne @ f @ a @ b)
  quickCheck (monadLawTwo @ f @ a)
  quickCheck (monadLawThree @ f @ a @ b @ c)

checkUnConstrained
    :: (Show (f a)
       ,Arbitrary (f a)
       ,Suitable f a
       ,Suitable f ((a -> b) -> (c -> a) -> c -> b)
       ,Arbitrary (f (a -> b))
       ,Arbitrary (f (c -> a))
       ,Suitable f ((c -> a) -> c -> b)
       ,Suitable f (c -> b)
       ,Suitable f (a -> b)
       ,Suitable f ((a -> b) -> b)
       ,Eq (f a)
       ,Show (f c)
       ,CoArbitrary c
       ,CoArbitrary b
       ,Arbitrary a
       ,Arbitrary (f c)
       ,Suitable f b
       ,Show (f b)
       ,Arbitrary (f b)
       ,Show a
       ,CoArbitrary a
       ,Eq (f b)
       ,Monad f
       ,Suitable f (a -> a)
       ,Arbitrary b)
    => Proxy f -> Proxy a -> Proxy b -> Proxy c -> IO ()
checkUnConstrained (pf :: Proxy f) (pa :: Proxy a) (pb :: Proxy b) (pc :: Proxy c) = do
  checkConstrained pf pa pb pc
  quickCheck (appIdLaw @ f @ a)
  quickCheck (appCompLaw @ f @ a @ b @ c)
  quickCheck (homomorphismLaw (Proxy :: Proxy f) :: Blind (a -> b) -> a -> Property)
  quickCheck (interchangeLaw @ f @ a @ b)


{-# ANN fmapLaw "HLint: ignore Functor law" #-}
fmapLaw
    :: (Functor f, Suitable f a, Eq (f a), Show (f a))
    => f a -> Property
fmapLaw xs = fmap id xs === xs

{-# ANN fmapCompLaw "HLint: ignore Functor law" #-}
fmapCompLaw
    :: (Functor f, Suitable f c, Eq (f c), Show (f c), Suitable f b)
    => Blind (b -> c) -> Blind (a -> b) -> f a -> Property
fmapCompLaw (Blind f) (Blind g) xs = fmap (f . g) xs === (fmap f . fmap g) xs

seqRightLaw
    :: (Applicative f, Suitable f b, Eq (f b), Show (f b))
    => f a -> f b -> Property
seqRightLaw xs ys = (xs *> ys) === (liftA2 (const id) xs ys)

seqLeftLaw
    :: (Applicative f, Suitable f a, Eq (f a), Show (f a))
    => f a -> f b -> Property
seqLeftLaw xs ys = (xs <* ys) === (liftA2 const xs ys)

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
  checkSame          (Proxy :: Proxy [] )    (Proxy :: Proxy Integer) (Proxy :: Proxy Word) (Proxy :: Proxy Int)
  checkUnConstrained (Proxy :: Proxy [] )    (Proxy :: Proxy Integer) (Proxy :: Proxy Word) (Proxy :: Proxy Int)
  checkConstrained   (Proxy :: Proxy Set)    (Proxy :: Proxy Integer) (Proxy :: Proxy Word) (Proxy :: Proxy Int)
  checkConstrained   (Proxy :: Proxy IntSet) (Proxy :: Proxy Int) (Proxy :: Proxy Int) (Proxy :: Proxy Int)

  doctest
    [ "-isrc"
    , "src/" ]
