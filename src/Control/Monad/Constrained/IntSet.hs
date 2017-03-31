{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RebindableSyntax   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- | This module creates an 'IntSet' type with a phantom type variable, allowing
-- it to conform to 'Functor', 'Foldable', etc. Other than that, it's a
-- duplication of the "Data.IntSet" module.
module Control.Monad.Constrained.IntSet
  ( -- * IntSet type
    IntSet
    -- * Operators
  ,(\\)
    -- * Query
  ,lookupLT
  ,lookupLE
  ,lookupGT
  ,lookupGE
  ,isSubsetOf
  ,isProperSubsetOf
   -- * Construction
  ,insert
  ,delete
   -- * Combine
  ,difference
  ,intersection
   -- * Filter
  ,filter
  ,partition
  ,split
  ,splitMember
  ,splitRoot
  -- * Min/Max
  ,maxView
  ,minView
  ,deleteMin
  ,deleteMax
  -- * Ordered List
  ,toAscList
  ,toDescList
  ,fromAscList
  ,fromDistinctAscList)
  where

import           Control.Monad.Constrained                        hiding
                                                                   (filter)

import qualified Data.IntSet                                      as IntSet

import           Data.Foldable                                    (Foldable (..))
import           Data.Functor.Classes
import           Data.Semigroup

import           Control.Arrow                                    (first)
import           GHC.Exts

import           Control.Monad.Constrained.Internal.Unconstrained

import           Data.Data                                        (Data)
import           Data.Typeable                                    (Typeable)

import           Control.DeepSeq                                  (NFData (..))

-- | This type is a wrapper around 'Data.IntSet.IntSet', with a phantom type
-- variable which must always be 'Int'. This allows it to conform to 'Functor',
-- 'Foldable', 'Applicative', 'Monad', etc.
data IntSet a where
        IntSet :: !IntSet.IntSet -> IntSet Int

deriving instance Typeable (IntSet a)
deriving instance a ~ Int => Data (IntSet a)

instance NFData (IntSet a) where
    rnf (IntSet xs) = rnf xs

instance Foldable IntSet where
    foldr f b (IntSet xs) = IntSet.foldr f b xs
    {-# INLINE foldr #-}
    foldl f b (IntSet xs) = IntSet.foldl f b xs
    {-# INLINE foldl #-}
    foldr' f b (IntSet xs) = IntSet.foldr' f b xs
    {-# INLINE foldr' #-}
    foldl' f b (IntSet xs) = IntSet.foldl' f b xs
    {-# INLINE foldl' #-}
    null (IntSet xs) = IntSet.null xs
    {-# INLINE null #-}
    length (IntSet xs) = IntSet.size xs
    {-# INLINE length #-}
    minimum (IntSet xs) = IntSet.findMin xs
    {-# INLINE minimum #-}
    maximum (IntSet xs) = IntSet.findMax xs
    {-# INLINE maximum #-}
    elem x (IntSet xs) = IntSet.member x xs
    {-# INLINE elem #-}

instance Functor IntSet where
    type Suitable IntSet a = a ~ Int
    fmap f (IntSet xs) = IntSet (IntSet.map f xs)
    {-# INLINE fmap #-}
    x <$ IntSet xs =
        IntSet
            (if IntSet.null xs
                 then IntSet.empty
                 else IntSet.singleton x)
    {-# INLINE (<$) #-}

instance Semigroup (IntSet a) where
    IntSet xs <> IntSet ys = IntSet (IntSet.union xs ys)
    {-# INLINE (<>) #-}

instance a ~ Int => Monoid (IntSet a) where
    mempty = IntSet IntSet.empty
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Applicative IntSet where
    pure x = IntSet (IntSet.singleton x)
    {-# INLINE pure #-}
    xs *> ys =
        if null xs
            then mempty
            else ys
    {-# INLINE (*>) #-}
    xs <* ys =
        if null ys
            then mempty
            else xs
    {-# INLINE (<*) #-}
    reify (StrictLeftFold xs) = IntSet (xs (flip IntSet.insert) IntSet.empty)
    {-# INLINE reify #-}
    reflect (IntSet xs) = StrictLeftFold (\f b -> IntSet.foldl' f b xs)
    {-# INLINE reflect #-}

type instance Unconstrained IntSet = StrictLeftFold

instance Alternative IntSet where
    empty = mempty
    {-# INLINE empty #-}
    (<|>) = mappend
    {-# INLINE (<|>) #-}

instance Monad IntSet where
    (>>=) = flip foldMap
    {-# INLINE (>>=) #-}

instance a ~ Int =>
         IsList (IntSet a) where
    type Item (IntSet a) = a
    fromList = IntSet . IntSet.fromList
    {-# INLINE fromList #-}
    toList = foldr (:) []
    {-# INLINE toList #-}

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
{-# INLINE lookupLT #-}

-- | /O(log n)/. Find smallest element greater than the given one.
--
-- > lookupGT 4 (fromList [3, 5]) == Just 5
-- > lookupGT 5 (fromList [3, 5]) == Nothing
lookupGT :: a -> IntSet a -> Maybe a
lookupGT x (IntSet xs) = IntSet.lookupGT x xs
{-# INLINE lookupGT #-}

-- | /O(log n)/. Find largest element smaller or equal to the given one.
--
-- > lookupLE 2 (fromList [3, 5]) == Nothing
-- > lookupLE 4 (fromList [3, 5]) == Just 3
-- > lookupLE 5 (fromList [3, 5]) == Just 5
lookupLE :: a -> IntSet a -> Maybe a
lookupLE x (IntSet xs) = IntSet.lookupLE x xs
{-# INLINE lookupLE #-}

-- | /O(log n)/. Find smallest element greater or equal to the given one.
--
-- > lookupGE 3 (fromList [3, 5]) == Just 3
-- > lookupGE 4 (fromList [3, 5]) == Just 5
-- > lookupGE 6 (fromList [3, 5]) == Nothing
lookupGE :: a -> IntSet a -> Maybe a
lookupGE x (IntSet xs) = IntSet.lookupGE x xs
{-# INLINE lookupGE #-}

-- | /O(min(n,W))/. Add a value to the set. There is no left- or right bias for
-- IntSets.
insert :: a -> IntSet a -> IntSet a
insert x (IntSet xs) = IntSet (IntSet.insert x xs)
{-# INLINE insert #-}

-- | /O(min(n,W))/. Delete a value in the set. Returns the
-- original set when the value was not present.
delete :: a -> IntSet a -> IntSet a
delete x (IntSet xs) = IntSet (IntSet.delete x xs)
{-# INLINE delete #-}

-- | /O(n+m)/. Difference between two sets.
difference :: IntSet a -> IntSet a -> IntSet a
difference (IntSet xs) (IntSet ys) = IntSet (IntSet.difference xs ys)
{-# INLINE difference #-}

-- | /O(n+m)/. The intersection of two sets.
intersection :: IntSet a -> IntSet a -> IntSet a
intersection (IntSet xs) (IntSet ys) = IntSet (IntSet.intersection xs ys)
{-# INLINE intersection #-}

-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: (a -> Bool) -> IntSet a -> IntSet a
filter p (IntSet xs) = IntSet (IntSet.filter p xs)
{-# INLINE filter #-}

-- | /O(n)/. partition the set according to some predicate.
partition :: (a -> Bool) -> IntSet a -> (IntSet a, IntSet a)
partition p (IntSet xs) =
    let (ys,zs) = IntSet.partition p xs
    in (IntSet ys, IntSet zs)
{-# INLINE partition #-}

-- | /O(min(n,W))/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split 3 (fromList [1..5]) == (fromList [1,2], fromList [4,5])
split :: a -> IntSet a -> (IntSet a, IntSet a)
split x (IntSet xs) =
    let (ys,zs) = IntSet.split x xs
    in (IntSet ys, IntSet zs)
{-# INLINE split #-}

-- | /O(min(n,W))/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: IntSet a -> Maybe (a, IntSet a)
maxView (IntSet xs) = (fmap.fmap) IntSet (IntSet.maxView xs)
{-# INLINE maxView #-}

-- | /O(min(n,W))/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: IntSet a -> Maybe (a, IntSet a)
minView (IntSet xs) = (fmap.fmap) IntSet (IntSet.minView xs)
{-# INLINE minView #-}

instance Show1 IntSet where
    liftShowsPrec _ _ d (IntSet xs) = showsPrec d xs
    {-# INLINE liftShowsPrec #-}

instance Show a =>
         Show (IntSet a) where
    showsPrec = showsPrec1
    {-# INLINE showsPrec #-}

instance a ~ Int =>
         Read (IntSet a) where
    readsPrec n = (fmap . first) IntSet . readsPrec n
    {-# INLINE readsPrec #-}

instance Eq1 IntSet where
    liftEq _ (IntSet xs) (IntSet ys) = xs == ys
    {-# INLINE liftEq #-}

instance Eq a =>
         Eq (IntSet a) where
    (==) = eq1
    {-# INLINE (==) #-}

instance Ord1 IntSet where
    liftCompare _ (IntSet xs) (IntSet ys) = compare xs ys
    {-# INLINE liftCompare #-}

instance Ord a =>
         Ord (IntSet a) where
    compare = compare1
    {-# INLINE compare #-}

isSubsetOf :: IntSet a -> IntSet a -> Bool
isSubsetOf (IntSet xs) (IntSet ys) = IntSet.isSubsetOf xs ys
{-# INLINE isSubsetOf #-}

isProperSubsetOf :: IntSet a -> IntSet a -> Bool
isProperSubsetOf (IntSet xs) (IntSet ys) = IntSet.isProperSubsetOf xs ys
{-# INLINE isProperSubsetOf #-}

splitMember :: a -> IntSet a -> (IntSet a, Bool, IntSet a)
splitMember x (IntSet xs) =
    let (ys,m,zs) = IntSet.splitMember x xs
    in (IntSet ys, m, IntSet zs)
{-# INLINE splitMember #-}

splitRoot :: IntSet a -> [IntSet a]
splitRoot (IntSet xs) = fmap IntSet (IntSet.splitRoot xs)
{-# INLINE splitRoot #-}

deleteMin :: IntSet a -> IntSet a
deleteMin (IntSet xs) = IntSet (IntSet.deleteMin xs)
{-# INLINE deleteMin #-}

deleteMax :: IntSet a -> IntSet a
deleteMax (IntSet xs) = IntSet (IntSet.deleteMax xs)
{-# INLINE deleteMax #-}

toAscList :: IntSet a -> [a]
toAscList (IntSet xs) = IntSet.toAscList xs
{-# INLINE toAscList #-}

toDescList :: IntSet a -> [a]
toDescList (IntSet xs) = IntSet.toAscList xs
{-# INLINE toDescList #-}

fromAscList :: [Int] -> IntSet Int
fromAscList  = IntSet . IntSet.fromAscList
{-# INLINE fromAscList #-}

fromDistinctAscList :: [Int] -> IntSet Int
fromDistinctAscList = IntSet . IntSet.fromDistinctAscList
{-# INLINE fromDistinctAscList #-}
