{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RebindableSyntax          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}


module Control.Monad.Constrained.Appl where

import           Control.Arrow
import           Data.Foldable
import           Data.Ord
import           Data.Proxy
import           Data.Ratio
import           GHC.Exts
import           GHC.TypeLits
import Data.Constraint

import           Prelude       (Bool (..), Eq (..), Integer, Num (..),
                                Show (..), flip, map, undefined, (.))

infixr 5 :-
data Vect xs where
  Nil :: Vect '[]
  (:-) :: x -> Vect xs -> Vect (x ': xs)

data AppVect f xs where
  NilA :: AppVect f '[]
  (:*) :: f x -> AppVect f xs -> AppVect f (x ': xs)

data ApplVect :: (* -> *) -> [Constraint] -> [*] -> * where
  NilAl :: ApplVect f '[] '[]
  (:*-) :: Appl f c x -> ApplVect f d xs -> ApplVect f (All c ': d) (x ': xs)

data Appl :: (* -> *) -> [Constraint] -> * -> * where
  Pure :: a -> Appl f '[Suitable f a] a
  Ap :: (Vect xs -> a) -> ApplVect f cs xs -> Appl f (Suitable f a ': cs) a

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

(<*>) :: Appl f (c ': cs) (a -> b) -> Appl f ds a -> Appl f (Suitable f b ': All ds ': cs) b
Pure f <*> x = Ap (\(xx :- Nil) -> f xx) (x :*- NilAl)
Ap fs xs <*> ys = Ap (\(v :- vs) -> fs vs v) (ys :*- xs)

class Functor f where
  type Suitable f a :: Constraint
  fmap :: Suitable f b => (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: Suitable f a => a -> f a
  liftA :: Suitable f a => (Vect xs -> a) -> AppVect f xs -> f a

type family All (cs :: [Constraint]) :: Constraint where
  All '[] = ()
  All (c ': cs) = (c, All cs)

interpret :: (Applicative f, All cs) => Appl f cs a -> f a
interpret (Pure x)   = pure x
interpret (Ap fs xs) = liftA fs (interpretApplVect xs)

interpretApplVect :: (Applicative f, All cs) => ApplVect f cs xs -> AppVect f xs
interpretApplVect = interpretApplVectD Dict

interpretApplVectD :: Applicative f => Dict (All cs) -> ApplVect f cs xs -> AppVect f xs
interpretApplVectD _ NilAl = NilA
interpretApplVectD d@Dict (x :*- xs) = interpret x :* interpretApplVectD (mapDict weaken2 d) xs
