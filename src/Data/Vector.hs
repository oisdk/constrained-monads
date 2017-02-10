{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}

module Data.Vector where

infixr 5 :-
data Vect xs where
  One :: x -> Vect '[x]
  (:-) :: x -> Vect xs -> Vect (x ': xs)

infixr 5 :*
data AppVect f xs where
  OneA :: f x -> AppVect f '[x]
  (:*) :: f x -> AppVect f xs -> AppVect f (x ': xs)

ex :: Applicative f => f Integer -> f Integer -> f Integer
ex x y = do
  x' <- x
  y' <- y
  return (x' + y')
