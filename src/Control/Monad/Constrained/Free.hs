{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ApplicativeDo    #-}
module Control.Monad.Constrained.Free where

import Control.Monad.Constrained hiding ((<$>), (<*>), pure, fmap)
import qualified Control.Monad.Constrained as Constrained

data App :: (* -> *) -> * -> * where
        Pure :: a -> App f a
        Appl :: FunType xs a -> AppVect f xs -> App f a
        Lift :: f a -> App f a

infixl 4 <*>
(<*>) :: App f (a -> b) -> f a -> App f b
Pure f <*> x = Appl f (Nil :> x)
Lift f <*> x = Appl ($) (Nil :> f :> x)
Appl fs xs <*> ys = Appl fs (xs :> ys)

fmap :: (a -> b) -> f a -> App f b
fmap f x = Appl f (Nil :> x)

(<$>) :: (a -> b) -> f a -> App f b
(<$>) = fmap

pure :: a -> App f a
pure = Pure

lower
    :: (Applicative f, Suitable f a)
    => App f a -> f a
lower (Lift x) = x
lower (Pure x) = Constrained.pure x
lower (Appl fs xs) = liftA fs (lowerVect xs)
  where
    lowerVect
        :: Applicative f
        => AppVect f xs -> AppVect f xs
    lowerVect Nil = Nil
    lowerVect (ys :> y) = lowerVect ys :> y
