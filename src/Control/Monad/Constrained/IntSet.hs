{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module creates an 'IntSet' type with a phantom type variable, allowing
-- it to conform to 'Functor', 'Foldable', etc. Other than that, it's a
-- duplication of the "Data.IntSet" module.
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
  ,maxView
  ,minView)
  where

import           Control.Monad.Constrained hiding (filter)

import qualified Data.IntSet               as IntSet

import           Data.Foldable             (Foldable (..))
import           Data.Functor.Classes
import           Data.Semigroup

import           Control.Arrow             (first)
import           GHC.Exts

-- | This type is a wrapper around 'Data.IntSet.IntSet', with a phantom type
-- variable which must always be 'Int'. This allows it to conform to 'Functor',
-- 'Foldable', 'Applicative', 'Monad', etc.
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

instance a ~ Int => Monoid (IntSet a) where
    mempty = IntSet IntSet.empty
    mappend = (<>)

instance Applicative IntSet where
    pure x = IntSet (IntSet.singleton x)
    xs *> ys =
        if null xs
            then mempty
            else ys
    xs <* ys =
        if null ys
            then mempty
            else xs
    lower = lowerM
    eta = liftAp

type instance Unconstrained IntSet = Ap IntSet

instance Alternative IntSet where
    empty = mempty
    (<|>) = mappend

instance Monad IntSet where
    (>>=) = flip foldMap

instance a ~ Int => IsList (IntSet a) where
  type Item (IntSet a) = a
  fromList = IntSet . IntSet.fromList
  toList = foldr (:) []

infixl 9 \\
-- | /O(n+m)/. See 'difference'.
(\\) :: IntSet a -> IntSet a -> IntSet a
IntSet xs \\ IntSet ys = IntSet (xs IntSet.\\ ys)

-- | /O(log n)/. Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList [3, 5]) == Nothing
-- > lookupLT 5 (fromList [3, 5]) == Just 3
lookupLT :: a -> IntSet a -> Maybe a
lookupLT x (IntSet xs) = IntSet.lookupLT x xs

-- | /O(log n)/. Find smallest element greater than the given one.
--
-- > lookupGT 4 (fromList [3, 5]) == Just 5
-- > lookupGT 5 (fromList [3, 5]) == Nothing
lookupGT :: a -> IntSet a -> Maybe a
lookupGT x (IntSet xs) = IntSet.lookupGT x xs

-- | /O(log n)/. Find largest element smaller or equal to the given one.
--
-- > lookupLE 2 (fromList [3, 5]) == Nothing
-- > lookupLE 4 (fromList [3, 5]) == Just 3
-- > lookupLE 5 (fromList [3, 5]) == Just 5
lookupLE :: a -> IntSet a -> Maybe a
lookupLE x (IntSet xs) = IntSet.lookupLE x xs

-- | /O(log n)/. Find smallest element greater or equal to the given one.
--
-- > lookupGE 3 (fromList [3, 5]) == Just 3
-- > lookupGE 4 (fromList [3, 5]) == Just 5
-- > lookupGE 6 (fromList [3, 5]) == Nothing
lookupGE :: a -> IntSet a -> Maybe a
lookupGE x (IntSet xs) = IntSet.lookupGE x xs

-- | /O(min(n,W))/. Add a value to the set. There is no left- or right bias for
-- IntSets.
insert :: a -> IntSet a -> IntSet a
insert x (IntSet xs) = IntSet (IntSet.insert x xs)

-- | /O(min(n,W))/. Delete a value in the set. Returns the
-- original set when the value was not present.
delete :: a -> IntSet a -> IntSet a
delete x (IntSet xs) = IntSet (IntSet.delete x xs)

-- | /O(n+m)/. Difference between two sets.
difference :: IntSet a -> IntSet a -> IntSet a
difference (IntSet xs) (IntSet ys) = IntSet (IntSet.difference xs ys)

-- | /O(n+m)/. The intersection of two sets.
intersection :: IntSet a -> IntSet a -> IntSet a
intersection (IntSet xs) (IntSet ys) = IntSet (IntSet.intersection xs ys)

-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: (a -> Bool) -> IntSet a -> IntSet a
filter p (IntSet xs) = IntSet (IntSet.filter p xs)

-- | /O(n)/. partition the set according to some predicate.
partition :: (a -> Bool) -> IntSet a -> (IntSet a, IntSet a)
partition p (IntSet xs) =
    let (ys,zs) = IntSet.partition p xs
    in (IntSet ys, IntSet zs)

-- | /O(min(n,W))/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split 3 (fromList [1..5]) == (fromList [1,2], fromList [4,5])
split :: a -> IntSet a -> (IntSet a, IntSet a)
split x (IntSet xs) =
    let (ys,zs) = IntSet.split x xs
    in (IntSet ys, IntSet zs)

-- | /O(min(n,W))/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: IntSet a -> Maybe (a, IntSet a)
maxView (IntSet xs) = (fmap.fmap) IntSet (IntSet.maxView xs)

-- | /O(min(n,W))/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: IntSet a -> Maybe (a, IntSet a)
minView (IntSet xs) = (fmap.fmap) IntSet (IntSet.minView xs)

instance Show1 IntSet where
    liftShowsPrec _ _ d (IntSet xs) = showsPrec d xs

instance Show a =>
         Show (IntSet a) where
    showsPrec = showsPrec1

instance a ~ Int =>
         Read (IntSet a) where
    readsPrec n = (fmap . first) IntSet . readsPrec n

instance Eq1 IntSet where
    liftEq _ (IntSet xs) (IntSet ys) = xs == ys

instance Eq a =>
         Eq (IntSet a) where
    (==) = eq1

instance Ord1 IntSet where
    liftCompare _ (IntSet xs) (IntSet ys) = compare xs ys

instance Ord a =>
         Ord (IntSet a) where
    compare = compare1
