{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeFamilies      #-}

module Control.Monad.Constrained.IntSet
  (IntSet
  ,(\\)
  ,lookupLT
  ,lookupLE
  ,lookupGT
  ,lookupGE
  ,insert
  ,delete
  ,difference
  ,intersection
  ,filter
  ,partition
  ,split
  ,fromList
  ,toAscList
  ,toDescList
  ,maxView
  ,minView)
  where

import           Control.Monad.Constrained hiding (filter)

import qualified Data.IntSet               as IntSet

import           Data.Foldable             (Foldable (..))
import           Data.Functor.Classes
import           Data.Semigroup

import           Control.Arrow             (first)

data IntSet a where
        IntSet :: IntSet.IntSet -> IntSet Int

instance Foldable IntSet where
    foldr f b (IntSet xs) = IntSet.foldr f b xs
    foldl f b (IntSet xs) = IntSet.foldl f b xs
    foldr' f b (IntSet xs) = IntSet.foldr' f b xs
    foldl' f b (IntSet xs) = IntSet.foldl' f b xs
    null (IntSet xs) = IntSet.null xs
    length (IntSet xs) = IntSet.size xs
    minimum (IntSet xs) = IntSet.findMin xs
    maximum (IntSet xs) = IntSet.findMax xs
    elem x (IntSet xs) = IntSet.member x xs

instance Functor IntSet where
    type Suitable IntSet a = a ~ Int
    fmap f (IntSet xs) = IntSet (IntSet.map f xs)
    x <$ IntSet xs =
        IntSet
            (if IntSet.null xs
                 then IntSet.empty
                 else IntSet.singleton x)

instance Semigroup (IntSet a) where
    IntSet xs <> IntSet ys = IntSet (IntSet.union xs ys)

instance Monoid (IntSet Int) where
    mempty = IntSet IntSet.empty
    mappend = (<>)

instance Applicative IntSet where
    pure x = IntSet (IntSet.singleton x)
    _ <*> _ = undefined
    xs *> ys =
        if null xs
            then mempty
            else ys
    xs <* ys =
        if null ys
            then mempty
            else xs
    liftA = liftAM

instance Alternative IntSet where
    empty = mempty
    (<|>) = mappend

instance Monad IntSet where
    (>>=) = flip foldMap

infixl 9 \\
(\\) :: IntSet a -> IntSet a -> IntSet a
IntSet xs \\ IntSet ys = IntSet (xs IntSet.\\ ys)

lookupLT :: a -> IntSet a -> Maybe a
lookupLT x (IntSet xs) = IntSet.lookupLT x xs

lookupGT :: a -> IntSet a -> Maybe a
lookupGT x (IntSet xs) = IntSet.lookupGT x xs

lookupLE :: a -> IntSet a -> Maybe a
lookupLE x (IntSet xs) = IntSet.lookupLE x xs

lookupGE :: a -> IntSet a -> Maybe a
lookupGE x (IntSet xs) = IntSet.lookupGE x xs

insert :: a -> IntSet a -> IntSet a
insert x (IntSet xs) = IntSet (IntSet.insert x xs)

delete :: a -> IntSet a -> IntSet a
delete x (IntSet xs) = IntSet (IntSet.delete x xs)

difference :: IntSet a -> IntSet a -> IntSet a
difference (IntSet xs) (IntSet ys) = IntSet (IntSet.difference xs ys)

intersection :: IntSet a -> IntSet a -> IntSet a
intersection (IntSet xs) (IntSet ys) = IntSet (IntSet.intersection xs ys)

filter :: (a -> Bool) -> IntSet a -> IntSet a
filter p (IntSet xs) = IntSet (IntSet.filter p xs)

partition :: (a -> Bool) -> IntSet a -> (IntSet a, IntSet a)
partition p (IntSet xs) =
    let (ys,zs) = IntSet.partition p xs
    in (IntSet ys, IntSet zs)

split :: a -> IntSet a -> (IntSet a, IntSet a)
split x (IntSet xs) =
    let (ys,zs) = IntSet.split x xs
    in (IntSet ys, IntSet zs)

fromList :: [Int] -> IntSet Int
fromList xs = IntSet (IntSet.fromList xs)

toAscList :: IntSet a -> [a]
toAscList (IntSet xs) = IntSet.toAscList xs

toDescList :: IntSet a -> [a]
toDescList (IntSet xs) = IntSet.toDescList xs

maxView :: IntSet a -> Maybe (a, IntSet a)
maxView (IntSet xs) = (fmap.fmap) IntSet (IntSet.maxView xs)

minView :: IntSet a -> Maybe (a, IntSet a)
minView (IntSet xs) = (fmap.fmap) IntSet (IntSet.minView xs)

instance Show1 IntSet where
  liftShowsPrec _ _ d (IntSet xs) = showsPrec d xs

instance Show a => Show (IntSet a) where
  showsPrec = showsPrec1

instance (a ~ Int) => Read (IntSet a) where
  readsPrec n = (fmap.first) IntSet . readsPrec n

instance Eq1 IntSet where
  liftEq _ (IntSet xs) (IntSet ys) = xs == ys

instance Eq a => Eq (IntSet a) where
  (==) = eq1

instance Ord1 IntSet where
  liftCompare _ (IntSet xs) (IntSet ys) = compare xs ys

instance Ord a => Ord (IntSet a) where
  compare = compare1
