{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | A module for constrained monads (set, etc)
module Control.Monad.Constrained
  (
   -- * Basic Classes
   Functor(..)
  ,Applicative(..)
  ,Monad(..)
  ,Alternative(..)
  ,Traversable(..)
  ,
   -- * Useful functions
   guard
  , (<$>)
  , (=<<)
  , (<=<)
  , (>=>)
  , foldM
  , traverse_
  , replicateM
  , void
  , (<$)
  , (<*)
  , some
  , many
  , liftA2
  , liftA3
  , liftA4
  , liftA5
  ,
   -- * Syntax
   ifThenElse
  ,fail
  ,(>>)
  ,return
  ,
   -- * Rest of Prelude
   module RestPrelude)
  where

import           GHC.Exts

import Prelude as RestPrelude
       hiding (Applicative(..), Functor(..), Monad(..), (<$>), Traversable(..), (=<<))

import qualified Prelude
import qualified Control.Applicative
import qualified Control.Monad

import Data.Functor.Identity (Identity(..))

import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntMap.Strict (IntMap)
import Data.Sequence (Seq)

import Control.Monad.Cont (ContT)
import Control.Monad.State (StateT(..))
import Control.Monad.Reader (ReaderT(..), mapReaderT)

import Data.Vector

--------------------------------------------------------------------------------
-- Standard classes
--------------------------------------------------------------------------------

class Functor f  where
    type Suitable f a :: Constraint
    fmap
        :: Suitable f b
        => (a -> b) -> f a -> f b
    default fmap :: Prelude.Functor f => (a -> b) -> f a -> f b
    fmap = Prelude.fmap

class Functor f =>
      Applicative f  where
    pure
        :: Suitable f a
        => a -> f a
    {-# INLINE pure #-}
    infixl 4 <*>
    (<*>)
        :: Suitable f b
        => f (a -> b) -> f a -> f b
    default pure :: Prelude.Applicative f => a -> f a
    pure = Prelude.pure
    default (<*>) :: Prelude.Applicative f => f (a -> b) -> f a -> f b
    (<*>) = (Prelude.<*>)
    infixl 4 *>
    (*>) :: (Suitable f b) => f a -> f b -> f b
    default (*>) :: Prelude.Applicative f => f a -> f b -> f b
    (*>) = (Prelude.*>)
    {-# INLINE (*>) #-}
    liftA :: Suitable f b => (Vect xs -> b) -> AppVect f xs -> f b

liftA' :: (Prelude.Applicative f) => (Vect xs -> b) -> (AppVect f xs -> f b)
liftA' f (OneA xs) = Prelude.fmap (f . One) xs
liftA' f (x :* xs) =  ((f .) . (:-)) Prelude.<$> x Prelude.<*> liftA' id xs

class Applicative f =>
      Monad f  where
    join
        :: Suitable f a
        => f (f a) -> f a
    {-# INLINE join #-}
    infixl 1 >>=
    (>>=)
        :: Suitable f b
        => f a -> (a -> f b) -> f b
    {-# INLINE (>>=) #-}
    default join :: Prelude.Monad f => f (f a) -> f a
    join = Control.Monad.join
    default (>>=) :: Prelude.Monad f => f a -> (a -> f b) -> f b
    (>>=) = (Prelude.>>=)

class Applicative f =>
      Alternative f  where
    empty :: f a
    {-# INLINE empty #-}
    default empty :: Control.Applicative.Alternative f => f a
    empty = Control.Applicative.empty
    {-# INLINE (<|>) #-}
    infixl 3 <|>
    (<|>)
        :: Suitable f a
        => f a -> f a -> f a
    default (<|>) :: Control.Applicative.Alternative f => f a -> f a -> f a
    (<|>) = (Control.Applicative.<|>)

class (Foldable t, Functor t) =>
      Traversable t  where
    traverse
        :: (Suitable t b, Applicative f, Suitable f (t b))
        => (a -> f b) -> t a -> f (t b)

--------------------------------------------------------------------------------
-- useful functions
--------------------------------------------------------------------------------

liftA2 :: (Applicative f, Suitable f c) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f xs ys = liftA (\(x :- One y) -> f x y) (xs :* OneA ys)

liftA3 :: (Applicative f, Suitable f d) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f xs ys zs = liftA (\(x :- y :- One z) -> f x y z) (xs :* ys :* OneA zs)

liftA4 :: (Applicative f, Suitable f e) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f ws xs ys zs = liftA (\(w :- x :- y :- One z) -> f w x y z) (ws :* xs :* ys :* OneA zs)

liftA5 :: (Applicative f, Suitable f g) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f vs ws xs ys zs = liftA (\(v :- w :- x :- y :- One z) -> f v w x y z) (vs :* ws :* xs :* ys :* OneA zs)

infixl 4 <$>
(<$>) :: (Functor f, Suitable f b) => (a -> b) -> f a -> f b
(<$>) = fmap

infixr 1 =<<, <=<
(=<<) :: (Monad f, Suitable f b) => (a -> f b) -> f a -> f b
(=<<) = flip (>>=)

(<=<) :: (Monad f, Suitable f c) => (b -> f c) -> (a -> f b) -> a -> f c
(f <=< g) x = f =<< g x

infixl 1 >=>
(>=>) :: (Monad f, Suitable f c) => (a -> f b) -> (b -> f c) -> a -> f c
(f >=> g) x = f x >>= g

foldM :: (Monad m, Suitable m a, Foldable f) => (a -> b -> m a) -> a -> f b -> m a
foldM f b = foldl (\a e -> flip f e =<< a) (pure b)

traverse_ :: (Applicative f, Foldable t, Suitable f ()) => (a -> f b) -> t a -> f ()
traverse_ f = foldr (\e a -> f e *> a) (pure ())

guard :: (Alternative f, Suitable f ()) => Bool -> f ()
guard True = pure ()
guard False = empty

-- | @'replicateM' n act@ performs the action @n@ times,
-- gathering the results.
replicateM        :: (Applicative m, Suitable m [a]) => Int -> m a -> m [a]
{-# INLINEABLE replicateM #-}
{-# SPECIALISE replicateM :: Int -> IO a -> IO [a] #-}
{-# SPECIALISE replicateM :: Int -> Maybe a -> Maybe [a] #-}
replicateM cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure []
        | otherwise = liftA2 (:) f (loop (cnt - 1))

void :: (Functor f, Suitable f ()) => f a -> f ()
void = fmap (const ())

infixl 4 <$
(<$) :: (Functor f, Suitable f a) => a -> f b -> f a
(<$) = fmap . const

infixl 4 <*
(<*) :: (Applicative f, Suitable f a) => f a -> f b -> f a
(<*) = flip (*>)

-- | One or more.
some :: (Alternative f, Suitable f [a]) => f a -> f [a]
some v = some_v
  where
    many_v = some_v <|> pure []
    some_v = liftA2 (:) v many_v

-- | Zero or more.
many :: (Alternative f, Suitable f [a]) => f a -> f [a]
many v = many_v
  where
    many_v = some_v <|> pure []
    some_v = liftA2 (:) v many_v

--------------------------------------------------------------------------------
-- syntax
--------------------------------------------------------------------------------

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f

fail :: String -> a
fail = error

(>>) :: (Applicative f, Suitable f b) => f a -> f b -> f b
(>>) = (*>)

return :: (Applicative f, Suitable f a) => a -> f a
return = pure

--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------

instance Functor [] where
    type Suitable [] a = ()
instance Applicative [] where liftA = liftA'
instance Alternative []
instance Monad       []
instance Traversable [] where
  traverse f = foldr (liftA2 (:) . f) (pure [])

instance Functor Maybe where
    type Suitable Maybe a = ()
instance Applicative Maybe where liftA = liftA'
instance Alternative Maybe
instance Monad Maybe
instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = fmap Just (f x)

instance Functor IO where
    type Suitable IO a = ()
instance Applicative IO where liftA = liftA'
instance Alternative IO
instance Monad IO

instance Functor Identity where
    type Suitable Identity a = ()
instance Applicative Identity where liftA = liftA'
instance Monad Identity
instance Traversable Identity where
  traverse f (Identity x) = fmap Identity (f x)

instance Functor (Either a) where
    type Suitable (Either a) a = ()
instance Applicative (Either a) where liftA = liftA'
instance Monad (Either a)
instance Traversable (Either a) where
  traverse f = either (pure . Left) (fmap Right . f)

instance Functor Set where
    type Suitable Set a = Ord a
    fmap = Set.map

instance Applicative Set where
    pure = Set.singleton
    fs <*> xs = foldMap (`Set.map` xs) fs
    _ *> x = x
    liftA f (OneA xs) = fmap (f . One) xs
    liftA f (x :* xs) = x >>= \y -> liftA (f . (y:-)) xs

instance Monad Set where
    join = foldMap id
    (>>=) = flip foldMap

instance Alternative Set where
  empty = Set.empty
  (<|>) = Set.union

instance Functor (Map a) where
  type Suitable (Map a) b = ()

instance Functor ((,) a) where
  type Suitable ((,) a) b = ()
instance Monoid a => Applicative ((,) a) where liftA = liftA'
instance Monoid a => Monad ((,) a)
instance Traversable ((,) a) where
  traverse f (x,y) = fmap ((,) x) (f y)

instance Functor IntMap where
  type Suitable IntMap a = ()

instance Functor Seq where
  type Suitable Seq a = ()
instance Applicative Seq where liftA = liftA'
instance Alternative Seq
instance Monad Seq

instance Functor ((->) a) where
  type Suitable ((->) a) b = ()
instance Applicative ((->) a) where liftA = liftA'
instance Monad ((->) a)

instance Functor (ContT r m) where
  type Suitable (ContT r m) a = ()
instance Applicative (ContT r m) where liftA = liftA'
instance Monad (ContT r m)

instance Functor m => Functor (StateT s m) where
    type Suitable (StateT s m) a = Suitable m (a, s)
    fmap f m = StateT $ \ s ->
        (\ ~(a, s') -> (f a, s')) <$> runStateT m s
    {-# INLINE fmap #-}

instance (Monad m) =>
         Applicative (StateT s m) where
    pure a =
        StateT $
        \s ->
             pure (a, s)
    {-# INLINE pure #-}
    StateT mf <*> StateT mx =
        StateT $
        \s -> do
            ~(f,s') <- mf s
            ~(x,s'') <- mx s'
            pure (f x, s'')
    StateT xs *> StateT ys =
        StateT $
        \s -> do
            ~(_,s') <- xs s
            ys s'
    liftA f (OneA xs) = fmap (f . One) xs
    liftA f (x :* xs) = StateT $ \s -> do
      ~(x',s') <- runStateT x s
      runStateT (liftA (f . (x':-)) xs) s'

instance (Monad m, Alternative m) => Alternative (StateT s m) where
    empty = StateT (const empty)
    {-# INLINE empty #-}
    StateT m <|> StateT n = StateT $ \ s -> m s <|> n s
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (StateT s m) where
    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    {-# INLINE (>>=) #-}
    join (StateT xs) = StateT $ \s -> do
      ~(x,s') <- xs s
      runStateT x s'
    {-# INLINE join #-}

instance (Functor m) => Functor (ReaderT r m) where
    type Suitable (ReaderT r m) a = Suitable m a
    fmap f  = mapReaderT (fmap f)
    {-# INLINE fmap #-}

-- instance (Applicative m) => Applicative (ReaderT r m) where
--     pure = liftReaderT . pure
--     {-# INLINE pure #-}
--     f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r
--     {-# INLINE (<*>) #-}

-- instance (Alternative m) => Alternative (ReaderT r m) where
--     empty   = liftReaderT empty
--     {-# INLINE empty #-}
--     m <|> n = ReaderT $ \ r -> runReaderT m r <|> runReaderT n r
--     {-# INLINE (<|>) #-}

-- instance (Monad m) => Monad (ReaderT r m) where
--     m >>= k  = ReaderT $ \ r -> do
--         a <- runReaderT m r
--         runReaderT (k a) r
--     {-# INLINE (>>=) #-}

-- liftReaderT :: m a -> ReaderT r m a
-- liftReaderT m = ReaderT (const m)
-- {-# INLINE liftReaderT #-}
