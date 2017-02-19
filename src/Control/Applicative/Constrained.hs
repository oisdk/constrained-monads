{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module Control.Applicative.Constrained where

import Data.List (sort)

-- This is close! just figure out the cases in One, Cons. Translate them into typeclasses
-- liftA f (OneA xs) = fmap (f . One) xs
-- liftA f (x :* xs) = x >>= \y -> liftA (f . (y:-)) xs

type family FuncRec (xs :: [*]) (res :: *) = (fnc :: *) | fnc -> xs res where
  FuncRec '[] res = Res res
  FuncRec (x ': xs) res = x -> Func xs res

newtype Func xs res = Func
  { runFunc :: FuncRec xs res }

newtype Res res = Res
  { runRes :: res }

type family AppFuncRec (f :: * -> *) (xs :: [*]) (res :: *) = (appfnc :: *) | appfnc -> f xs res where
  AppFuncRec f '[] res = AppRes f res
  AppFuncRec f (x ': xs) res = f x -> AppFunc f xs res

newtype AppFunc f xs res = AppFunc
  { runAppFunc :: AppFuncRec f xs res }

newtype AppRes f res = AppRes { runAppRes :: f res }

class AppAble f where
  app :: Func xs res -> AppFunc f xs res

newtype OrdZip a = OrdZip { runOrdZip :: [a] }

fmap' :: Ord b => (a -> b) -> OrdZip a -> OrdZip b
fmap' f (OrdZip xs) = OrdZip (sort $ map f xs)
