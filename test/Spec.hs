{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeApplications     #-}
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
import           Data.Functor.Classes

import           Control.Monad.Trans.Reader (ReaderT(..), Reader)
import           Control.Monad.Trans.State.Strict (StateT(..), State)
import           Data.Functor.Identity

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
fmapIsSame (Blind f) x = label "fmap is same" $ fmap f x === Prelude.fmap f x

replaceIsSame
    :: (Functor f, Prelude.Functor f, Suitable f a, Eq (f a), Show (f a))
    => f b -> a -> Property
replaceIsSame xs x = label "replace is same" $ (x <$ xs) === (x Prelude.<$ xs)

pureIsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f a
       ,Eq (f a)
       ,Show (f a))
    => Proxy f -> a -> Property
pureIsSame (_ :: Proxy f) (x :: a) = label "pure is same" $ (pure x :: f a) === Prelude.pure x

seqRightIsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f b
       ,Eq (f b)
       ,Show (f b))
    => f a -> f b -> Property
seqRightIsSame xs ys = label "*> is same" $ (xs *> ys) === (xs Prelude.*> ys)

seqLeftIsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f b
       ,Eq (f b)
       ,Show (f b))
    => f b -> f a -> Property
seqLeftIsSame xs ys = label "<* is same" $ (xs <* ys) === (xs Prelude.<* ys)

applyIsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f b
       ,Eq (f b)
       ,Show (f b))
    => Blind (f (a -> b)) -> f a -> Property
applyIsSame (Blind fs) xs = label "<*> is same" $ (fs <*> xs) === (fs Prelude.<*> xs)

liftA2IsSame
    :: (Applicative f
       ,Prelude.Applicative f
       ,Suitable f c
       ,Eq (f c)
       ,Show (f c))
    => Blind (a -> b -> c) -> f a -> f b -> Property
liftA2IsSame (Blind f) xs ys =
    label "liftA2 is same" $
    liftA2 f xs ys === Control.Applicative.liftA2 f xs ys

bindIsSame
    :: (Monad f, Prelude.Monad f, Suitable f b, Show (f b), Eq (f b))
    => f a -> Blind (a -> f b) -> Property
bindIsSame xs (Blind f) =
    label ">>= is same" $ (xs >>= f) === (xs Prelude.>>= f)

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
fmapLaw xs = label "fmap law" $ fmap id xs === xs

{-# ANN fmapCompLaw "HLint: ignore Functor law" #-}
fmapCompLaw
    :: (Functor f, Suitable f c, Eq (f c), Show (f c), Suitable f b)
    => Blind (b -> c) -> Blind (a -> b) -> f a -> Property
fmapCompLaw (Blind f) (Blind g) xs =
    label "fmap comp law" $ fmap (f . g) xs === (fmap f . fmap g) xs

seqRightLaw
    :: (Applicative f, Suitable f b, Eq (f b), Show (f b))
    => f a -> f b -> Property
seqRightLaw xs ys = label "*> law" $ (xs *> ys) === (liftA2 (const id) xs ys)

seqLeftLaw
    :: (Applicative f, Suitable f a, Eq (f a), Show (f a))
    => f a -> f b -> Property
seqLeftLaw xs ys = label "<* law" $ (xs <* ys) === (liftA2 const xs ys)

appIdLaw
    :: (Applicative f, Suitable f a, Suitable f (a -> a), Eq (f a), Show (f a))
    => f a -> Property
appIdLaw xs = label "app id law" $ (pure id <*> xs) === xs

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
  = label "app comp law" $ (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

homomorphismLaw
    :: (Suitable f (a -> b)
       ,Suitable f b
       ,Suitable f a
       ,Applicative f
       ,Eq (f b)
       ,Show (f b))
    => Proxy f -> Blind (a -> b) -> a -> Property
homomorphismLaw (_ :: Proxy f) (Blind (f :: a -> b)) x
  = label "homomorphism law" $ (pure f <*> pure x) === (pure (f x) :: f b)

interchangeLaw
    :: (Applicative f
       ,Suitable f a
       ,Suitable f b
       ,Suitable f ((a -> b) -> b)
       ,Eq (f b)
       ,Show (f b))
    => Blind (f (a -> b)) -> a -> Property
interchangeLaw (Blind u) y = label "interchange law" $ (u <*> pure y) === (pure ($y) <*> u)

monadLawOne
    :: (Monad f, Suitable f a, Suitable f b, Show (f b), Eq (f b))
    => Blind (a -> f b) -> a -> Property
monadLawOne (Blind k) a = label "monad law one" $ (pure a >>= k) === k a

monadLawTwo
    :: (Monad f, Suitable f a, Eq (f a), Show (f a))
    => f a -> Property
monadLawTwo xs = label "monad law two" $ (xs >>= pure) === xs

monadLawThree
    :: (Monad f, Suitable f c, Eq (f c), Suitable f b, Show (f c))
    => f a -> Blind (a -> f b) -> Blind (b -> f c) -> Property
monadLawThree m (Blind k) (Blind h) =
    label "monad law three" $ (m >>= (k >=> h)) === ((m >>= k) >>= h)

instance (Enum a, Bounded a, Eq1 m, Eq b) => Eq (ReaderT a m b) where
  ReaderT fs == ReaderT gs = all (\x -> eq1 (fs x) (gs x)) [minBound..maxBound]

instance (CoArbitrary a, Arbitrary (m b)) => Arbitrary (ReaderT a m b) where
  arbitrary = fmap ReaderT arbitrary

instance (CoArbitrary a, Arbitrary (m (b,a))) => Arbitrary (StateT a m b) where
  arbitrary = fmap StateT arbitrary

instance (Enum a, Bounded a, Eq (m (b,a))) => Eq (StateT a m b) where
  StateT fs == StateT gs = all (\x -> (fs x) == (gs x)) [minBound..maxBound]

instance (Enum a, Show a, Show (m b), Bounded a) => Show (ReaderT a m b) where
  show (ReaderT xs) = show (map ((,) <*> xs) [minBound..maxBound])

instance (Enum a, Show a, Show (m (b,a)), Bounded a) => Show (StateT a m b) where
  show (StateT xs) = show (map ((,) <*> xs) [minBound..maxBound])

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

main :: IO ()
main = do
  putStrLn "[]"
  checkSame          (Proxy @ [] )                  (Proxy @ Integer) (Proxy @ Word) (Proxy @ Int)
  checkUnConstrained (Proxy @ [] )                  (Proxy @ Integer) (Proxy @ Word) (Proxy @ Int)
  putStrLn "Set"
  checkConstrained   (Proxy @ Set)                  (Proxy @ Integer) (Proxy @ Word) (Proxy @ Int)
  putStrLn "IntSet"
  checkConstrained   (Proxy @ IntSet)               (Proxy @ Int    ) (Proxy @ Int ) (Proxy @ Int)
  putStrLn "Reader Bool"
  checkUnConstrained (Proxy @ (Reader Bool))        (Proxy @ Int    ) (Proxy @ Int ) (Proxy @ Int)
  checkSame          (Proxy @ (Reader Bool))        (Proxy @ Int    ) (Proxy @ Int ) (Proxy @ Int)
  putStrLn "ReaderT Bool Maybe"
  checkUnConstrained (Proxy @ (ReaderT Bool Maybe)) (Proxy @ Int    ) (Proxy @ Int ) (Proxy @ Int)
  checkSame          (Proxy @ (ReaderT Bool Maybe)) (Proxy @ Int    ) (Proxy @ Int ) (Proxy @ Int)
  putStrLn "State Bool"
  checkUnConstrained (Proxy @ (State Bool))         (Proxy @ Int    ) (Proxy @ Int ) (Proxy @ Int)
  checkSame          (Proxy @ (State Bool))         (Proxy @ Int    ) (Proxy @ Int ) (Proxy @ Int)
  putStrLn "StateT Bool Maybe"
  checkUnConstrained (Proxy @ (StateT Bool Maybe))  (Proxy @ Int    ) (Proxy @ Int ) (Proxy @ Int)
  checkSame          (Proxy @ (StateT Bool Maybe))  (Proxy @ Int    ) (Proxy @ Int ) (Proxy @ Int)

  doctest [ "-isrc", "src/" ]
