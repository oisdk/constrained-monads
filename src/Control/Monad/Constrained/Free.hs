{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Monad.Constrained.Free where

import Control.Monad.Constrained hiding ((<$>), (<*>), pure, fmap)
import qualified Control.Monad.Constrained as Constrained

infixl 5 :>|
data FreeAppVect :: (* -> *) -> [*] -> * where
        NilAppl :: FreeAppVect f '[]
        (:>|) ::
            Suitable f x =>
            FreeAppVect f xs -> App f x -> FreeAppVect f (x ': xs)

data App :: (* -> *) -> * -> * where
        Pure :: a -> App f a
        Appl :: FunType xs a -> FreeAppVect f xs -> App f a
        Lift :: Suitable f a => f a -> App f a

infixl 4 <*>
(<*>) :: Suitable f a => App f (a -> b) -> App f a -> App f b
Pure f <*> x = Appl f (NilAppl :>| x)
Appl fs xs <*> ys = Appl fs (xs :>| ys)
Lift f <*> x = Appl ($) (NilAppl :>| Lift f :>| x)

infixl 4 <$>
(<$>) :: Suitable f a => (a -> b) -> App f a -> App f b
f <$> x = Pure f <*> x

fmap :: Suitable f a => (a -> b) -> App f a -> App f b
fmap = (<$>)

pure :: a -> App f a
pure = Pure

join :: App f (App f a) -> App f a
join = undefined

lower
    :: (Applicative f, Suitable f a)
    => App f a -> f a
lower (Pure x) = Constrained.pure x
lower (Lift x) = x
lower (Appl fs xs) = liftA fs (lowerVect xs)
  where
    lowerVect
        :: Applicative f
        => FreeAppVect f xs -> AppVect f xs
    lowerVect NilAppl = Nil
    lowerVect (ys :>| y) = lowerVect ys :> lower y
