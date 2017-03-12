{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE RankNTypes           #-}

-- | A module for constrained monads. This module is intended to be imported
-- with the @-XRebindableSyntax@ extension turned on: everything from the
-- "Prelude" (that doesn't conflict with the new 'Functor', 'Applicative', etc) is
-- reexported, so these type classes can be used the same way that the "Prelude"
-- classes are used.
module Control.Monad.Constrained
  (
   -- * Basic Classes
   Functor(..)
  ,Applicative(..)
  ,Monad(..)
  ,Alternative(..)
  ,Traversable(..)
  ,MonadFail(..)
  ,
   -- * Horrible type-level stuff
  Ap(..)
  ,lowerP
  ,lowerM
  ,liftAp
  ,
   -- * Useful functions
   guard
  ,ensure
  ,(<**>)
  ,(<$>)
  ,(=<<)
  ,(<=<)
  ,(>=>)
  ,foldM
  ,traverse_
  ,sequenceA
  ,sequenceA_
  ,mapAccumL
  ,replicateM
  ,void
  ,forever
  ,for_
  ,
   -- * Syntax
   ifThenElse
  ,(>>)
  ,return
  ,module RestPrelude)
  where

import           GHC.Exts

import           Prelude                          as RestPrelude hiding (Applicative (..),
                                                                  Functor (..),
                                                                  Monad (..),
                                                                  Traversable (..),
                                                                  (<$>), (=<<))

import qualified Control.Applicative
import qualified Prelude

import           Data.Functor.Identity            (Identity (..))

import           Data.IntMap.Strict               (IntMap)
import           Data.Map.Strict                  (Map)
import           Data.Sequence                    (Seq)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Tree                        (Tree(..))

import           Control.Monad.Trans.Cont         (ContT)
import           Control.Monad.Trans.Except       (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Identity     (IdentityT (..))
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Control.Monad.Trans.Reader       (ReaderT (..), mapReaderT)
import           Control.Monad.Trans.State        (StateT (..))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT (..))

import           Control.Arrow (first)
import           Data.Tuple
import           Control.Monad.Trans.State.Strict (state, runState)

--------------------------------------------------------------------------------
-- Type-level shenanigans
--------------------------------------------------------------------------------

-- | A free applicative. Applicative operations are defined in terms of
-- /interpretations/ of this.
data Ap f a where
  Pure :: a -> Ap f a
  Ap :: Ap f (a -> b) -> f a -> Ap f b

instance Prelude.Functor (Ap f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Ap x y) = Ap ((f .) Prelude.<$> x) y

instance Prelude.Applicative (Ap f) where
  pure = Pure
  Pure f <*> y = Prelude.fmap f y
  Ap x y <*> z = Ap (flip Prelude.<$> x Prelude.<*> z) y

liftAp :: f a -> Ap f a
liftAp = Ap (Pure id)

--------------------------------------------------------------------------------
-- Standard classes
--------------------------------------------------------------------------------

-- | This is the same class as 'Prelude.Functor' from the Prelude. Most of the
-- functions here are simply rewritten versions of those, with one difference:
-- types can indicate /which/ types they can contain. This allows
-- 'Data.Set.Set' to be made into a monad, as well as some other exotic types.
-- (but, to be fair, 'Data.Set.Set' is kind of the poster child for this
-- technique).
--
-- The way that types indicate what they can contain is with the 'Suitable'
-- associated type.
--
-- The default implementation is for types which conform to the "Prelude"'s
-- 'Prelude.Functor'. The way to make a standard 'Prelude.Functor' conform
-- is by indicating that it has no constraints. For instance, for @[]@:
--
-- @instance 'Functor' [] where
--  type 'Suitable' [] a = ()
--  fmap = map
--  (<$) = (Prelude.<$)@
--
-- Monomorphic types can also conform, using GADT aliases. For instance,
-- if you create an alias for 'Data.IntSet.IntSet' of kind @* -> *@:
--
-- @data IntSet a where
--  IntSet :: IntSet.'Data.IntSet.IntSet' -> IntSet 'Int'@
--
-- It can be made to conform to 'Functor' like so:
--
-- @instance 'Functor' IntSet where
--  type 'Suitable' IntSet a = a ~ 'Int'
--  'fmap' f (IntSet xs) = IntSet (IntSet.'Data.IntSet.map' f xs)
--  x '<$' xs = if 'null' xs then 'empty' else 'pure' x@
--
-- It can also be made conform to 'Foldable', etc. This type is provided in
-- "Control.Monad.Constrained.IntSet".
class Functor f  where
    {-# MINIMAL fmap #-}
    -- | Indicate which types can be contained by 'f'. For instance,
    -- 'Data.Set.Set' conforms like so:
    --
    -- @instance 'Functor' 'Set' where
    --    type 'Suitable' 'Set' a = 'Ord' a
    --    'fmap' = Set.'Set.map'
    --    x '<$' xs = if Set.'Set.null' xs then Set.'Set.empty' else Set.'Set.singleton' x@
    type Suitable f a :: Constraint

    -- | Maps a function over a functor
    fmap
        :: Suitable f b
        => (a -> b) -> f a -> f b

    -- | Replace all values in the input with a default value.
    infixl 4 <$
    (<$) :: Suitable f a => a -> f b -> f a
    (<$) = fmap . const
    {-# INLINE (<$) #-}

-- | A functor with application.
--
-- This class is slightly different (although equivalent) to the class
-- provided in the Prelude. This is to facilitate the lifting of functions
-- to arbitrary numbers of arguments.
--
-- A minimal complete definition must include implementations of 'lower'
-- functions satisfying the following laws:
--
-- [/identity/]
--
--      @'pure' 'id' '<*>' v = v@
--
-- [/composition/]
--
--      @'pure' (.) '<*>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@
--
-- [/homomorphism/]
--
--      @'pure' f '<*>' 'pure' x = 'pure' (f x)@
--
-- [/interchange/]
--
--      @u '<*>' 'pure' y = 'pure' ('$' y) '<*>' u@
--
-- The other methods have the following default definitions, which may
-- be overridden with equivalent specialized implementations:
--
--   * @u '*>' v = 'pure' ('const' 'id') '<*>' u '<*>' v@
--
--   * @u '<*' v = 'pure' 'const' '<*>' u '<*>' v@
--
-- As a consequence of these laws, the 'Functor' instance for @f@ will satisfy
--
--   * @'fmap' f x = 'pure' f '<*>' x@
--
-- If @f@ is also a 'Monad', it should satisfy
--
--   * @'pure' = 'return'@
--
--   * @('<*>') = 'ap'@
--
-- (which implies that 'pure' and '<*>' satisfy the applicative functor laws).
class Functor f =>
      Applicative f  where
    {-# MINIMAL lower #-}

    -- | Lift a value.
    pure
        :: Suitable f a
        => a -> f a
    pure x = lower (Pure x)
    {-# INLINE pure #-}

    infixl 4 <*>

    -- | Sequential application.
    (<*>)
        :: Suitable f b
        => f (a -> b) -> f a -> f b
    (<*>) = liftA2 ($)
    {-# INLINE (<*>) #-}

    infixl 4 *>
    -- | Sequence actions, discarding the value of the first argument.
    (*>)
        :: Suitable f b
        => f a -> f b -> f b
    (*>) = liftA2 (const id)
    {-# INLINE (*>) #-}

    infixl 4 <*
    -- | Sequence actions, discarding the value of the second argument.
    (<*)
        :: Suitable f a
        => f a -> f b -> f a
    (<*) = liftA2 const
    {-# INLINE (<*) #-}
    -- | The shenanigans introduced by this function are to account for the fact
    -- that you can't (I don't think) write an arbitrary lift function on
    -- non-monadic applicatives that have constrained types. For instance, if
    -- the only present functions are:
    --
    -- @'pure'  :: 'Suitable' f a => a -> f b
    --'fmap'  :: 'Suitable' f b => (a -> b) -> f a -> f b
    --('<*>') :: 'Suitable' f b => f (a -> b) -> f a -> f b@
    --
    -- I can't see a way to define:
    --
    -- @'liftA2' :: 'Suitable' f c => (a -> b -> c) -> f a -> f b -> f c@
    --
    -- Of course, if:
    --
    -- @('>>=') :: 'Suitable' f b => f a -> (a -> f b) -> f b@
    --
    -- is available, 'liftA2' could be defined as:
    --
    -- @'liftA2' f xs ys = do
    --    x <- xs
    --    y <- ys
    --    'pure' (f x y)@
    --
    -- But now we can't define the 'lower' functions for things which are
    -- 'Applicative' but not 'Monad' (square matrices,
    -- 'Control.Applicative.ZipList's, etc). Also, some types have a more
    -- efficient @('<*>')@ than @('>>=')@ (see, for instance, the
    -- <https://simonmar.github.io/posts/2015-10-20-Fun-With-Haxl-1.html Haxl>
    -- monad).
    --
    -- The one missing piece is @-XApplicativeDo@: I can't figure out a way
    -- to get do-notation to desugar to using the 'lower' functions, rather
    -- than @('<*>')@.
    --
    -- From some preliminary performance testing, it seems that this approach
    -- has /no/ performance overhead.
    --
    -- Utility definitions of this function are provided: if your 'Applicative'
    -- is a @Prelude.'Prelude.Applicative'@, 'lower' can be defined in terms of
    -- @('<*>')@. 'lowerP' does exactly this.
    --
    -- Alternatively, if your applicative is a 'Monad', 'lower' can be defined
    -- in terms of @('>>=')@, which is what 'lowerM' does.
    lower
        :: Suitable f a
        => Ap f a -> f a

    liftA2
        :: Suitable f c
        => (a -> b -> c) -> f a -> f b -> f c
    liftA2 f xs ys =
        lower (Ap (Ap (Pure f) xs) ys)

    liftA3
        :: Suitable f d
        => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    liftA3 f xs ys zs =
        lower (Ap (Ap (Ap (Pure f) xs) ys) zs)

    {-# INLINE liftA2 #-}
    {-# INLINE liftA3 #-}

infixl 4 <**>
-- | A variant of '<*>' with the arguments reversed.
(<**>) :: (Applicative f, Suitable f b) => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

-- | A definition of 'lower' that uses monadic operations.
lowerM :: (Monad f, Suitable f a) => Ap f a -> f a
lowerM = go pure where
  go :: (Suitable f b, Monad f) => (a -> f b) -> Ap f a -> f b
  go f (Pure x) = f x
  go f (Ap xs x) = go (\c -> x >>= f . c) xs

-- | A definition of 'lower' which uses the "Prelude"'s @('Prelude.<*>')@.
lowerP :: Prelude.Applicative f => Ap f a -> f a
lowerP (Pure x) = Prelude.pure x
lowerP (Ap (Pure f) xs) = Prelude.fmap f xs
lowerP (Ap ys xs) = lowerP ys Prelude.<*> xs
{-# INLINABLE lowerP #-}

{-# INLINE liftA2P #-}
{-# INLINE liftA3P #-}
-- | Definitions for the various lifts using only "Prelude" functions.
liftA2P
    :: (Prelude.Applicative f)
    => (a -> b -> c) -> f a -> f b -> f c
liftA2P f x y = f Prelude.<$> x Prelude.<*> y

liftA3P
    :: Prelude.Applicative f
    => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3P f xs ys zs = f Prelude.<$> xs Prelude.<*> ys Prelude.<*> zs

{- | The 'Monad' class defines the basic operations over a /monad/,
a concept from a branch of mathematics known as /category theory/.
From the perspective of a Haskell programmer, however, it is best to
think of a monad as an /abstract datatype/ of actions.
Haskell's @do@ expressions provide a convenient syntax for writing
monadic expressions.

Instances of 'Monad' should satisfy the following laws:

* @'return' a '>>=' k  =  k a@
* @m '>>=' 'return'  =  m@
* @m '>>=' (\\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@

Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:

* @'pure' = 'return'@
* @('<*>') = 'ap'@

The above laws imply:

* @'fmap' f xs  =  xs '>>=' 'return' . f@
* @('>>') = ('*>')@

and that 'pure' and ('<*>') satisfy the applicative functor laws.

The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
defined in the ""Prelude"" satisfy these laws.
-}
class Applicative f =>
      Monad f  where
    infixl 1 >>=
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)
        :: Suitable f b
        => f a -> (a -> f b) -> f b

-- | See
-- <https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad-Fail.html here>
-- for more details.
class Monad f => MonadFail f where
  -- | Called when a pattern match fails in do-notation.
  fail :: Suitable f a => String -> f a

-- | A monoid on applicative functors.
--
-- If defined, 'some' and 'many' should be the least solutions
-- of the equations:
--
-- * @some v = (:) '<$>' v '<*>' many v@
--
-- * @many v = some v '<|>' 'pure' []@
class Applicative f =>
      Alternative f  where
    {-# MINIMAL empty, (<|>) #-}
    -- | The identity of '<|>'
    empty :: Suitable f a => f a
    infixl 3 <|>
    -- | An associative binary operation
    (<|>)
        :: Suitable f a
        => f a -> f a -> f a
    -- | One or more.
    some :: Suitable f [a] => f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v

    -- | Zero or more.
    many :: Suitable f [a] => f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v

-- | Functors representing data structures that can be traversed from
-- left to right.
--
-- A definition of 'traverse' must satisfy the following laws:
--
-- [/naturality/]
--   @t . 'traverse' f = 'traverse' (t . f)@
--   for every applicative transformation @t@
--
-- [/identity/]
--   @'traverse' Identity = Identity@
--
-- [/composition/]
--   @'traverse' (Compose . 'fmap' g . f) = Compose . 'fmap' ('traverse' g) . 'traverse' f@
--
-- A definition of 'sequenceA' must satisfy the following laws:
--
-- [/naturality/]
--   @t . 'sequenceA' = 'sequenceA' . 'fmap' t@
--   for every applicative transformation @t@
--
-- [/identity/]
--   @'sequenceA' . 'fmap' Identity = Identity@
--
-- [/composition/]
--   @'sequenceA' . 'fmap' Compose = Compose . 'fmap' 'sequenceA' . 'sequenceA'@
--
-- where an /applicative transformation/ is a function
--
-- @t :: (Applicative f, Applicative g) => f a -> g a@
--
-- preserving the 'Applicative' operations, i.e.
--
--  * @t ('pure' x) = 'pure' x@
--
--  * @t (x '<*>' y) = t x '<*>' t y@
--
-- and the identity functor @Identity@ and composition of functors @Compose@
-- are defined as
--
-- >   newtype Identity a = Identity a
-- >
-- >   instance Functor Identity where
-- >     fmap f (Identity x) = Identity (f x)
-- >
-- >   instance Applicative Identity where
-- >     pure x = Identity x
-- >     Identity f <*> Identity x = Identity (f x)
-- >
-- >   newtype Compose f g a = Compose (f (g a))
-- >
-- >   instance (Functor f, Functor g) => Functor (Compose f g) where
-- >     fmap f (Compose x) = Compose (fmap (fmap f) x)
-- >
-- >   instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- >     pure x = Compose (pure (pure x))
-- >     Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
--
-- (The naturality law is implied by parametricity.)
--
-- Instances are similar to 'Functor', e.g. given a data type
--
-- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
--
-- a suitable instance would be
--
-- > instance Traversable Tree where
-- >    traverse f Empty = pure Empty
-- >    traverse f (Leaf x) = Leaf <$> f x
-- >    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
--
-- This is suitable even for abstract types, as the laws for '<*>'
-- imply a form of associativity.
--
-- The superclass instances should satisfy the following:
--
--  * In the 'Functor' instance, 'fmap' should be equivalent to traversal
--    with the identity applicative functor ('fmapDefault').
--
--  * In the 'Foldable' instance, 'Data.Foldable.foldMap' should be
--    equivalent to traversal with a constant applicative functor
--    ('foldMapDefault').
--
class (Foldable t, Functor t) =>
      Traversable t  where
    -- | Map each element of a structure to an action, evaluate these actions
    -- from left to right, and collect the results. For a version that ignores
    -- the results see 'traverse_'.
    traverse
        :: (Suitable t b, Applicative f, Suitable f (t b))
        => (a -> f b) -> t a -> f (t b)


--------------------------------------------------------------------------------
-- useful functions
--------------------------------------------------------------------------------

infixl 4 <$>
-- | An infix synonym for 'fmap'.
--
-- The name of this operator is an allusion to '$'.
-- Note the similarities between their types:
--
-- >  ($)  ::              (a -> b) ->   a ->   b
-- > (<$>) :: Functor f => (a -> b) -> f a -> f b
--
-- Whereas '$' is function application, '<$>' is function
-- application lifted over a 'Functor'.
--
-- ==== __Examples__
--
-- Convert from a @'Maybe' 'Int'@ to a @'Maybe' 'String'@ using 'show':
--
-- >>> show <$> Nothing
-- Nothing
-- >>> show <$> Just 3
-- Just "3"
--
-- Convert from an @'Either' 'Int' 'Int'@ to an @'Either' 'Int'@
-- 'String' using 'show':
--
-- >>> show <$> Left 17
-- Left 17
-- >>> show <$> Right 17
-- Right "17"
--
-- Double each element of a list:
--
-- >>> (*2) <$> [1,2,3]
-- [2,4,6]
--
-- Apply 'even' to the second element of a pair:
--
-- >>> even <$> (2,2)
-- (2,True)
--
(<$>) :: (Functor f, Suitable f b) => (a -> b) -> f a -> f b
(<$>) = fmap

infixr 1 =<<, <=<
-- | A flipped version of '>>='
(=<<) :: (Monad f, Suitable f b) => (a -> f b) -> f a -> f b
(=<<) = flip (>>=)

-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped.
--
-- Note how this operator resembles function composition @('.')@:
--
-- > (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
-- > (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) :: (Monad f, Suitable f c) => (b -> f c) -> (a -> f b) -> a -> f c
(f <=< g) x = f =<< g x

infixl 1 >=>

-- | Left-to-right Kleisli composition of monads.
(>=>) :: (Monad f, Suitable f c) => (a -> f b) -> (b -> f c) -> a -> f c
(f >=> g) x = f x >>= g

-- | @'forever' act@ repeats the action infinitely.
forever     :: (Applicative f, Suitable f b) => f a -> f b
{-# INLINE forever #-}
forever a   = let a' = a *> a' in a'

-- | Monadic fold over the elements of a structure,
-- associating to the left, i.e. from left to right.
foldM :: (Foldable t, Monad m, Suitable m b) => (b -> a -> m b) -> b -> t a -> m b
foldM f z0 xs = foldr f' pure xs z0
  where f' x k z = f z x >>= k

-- | 'for_' is 'traverse_' with its arguments flipped. For a version
-- that doesn't ignore the results see 'Data.Traversable.for'.
--
-- >>> for_ [1..4] print
-- 1
-- 2
-- 3
-- 4
for_ :: (Foldable t, Applicative f, Suitable f ()) => t a -> (a -> f b) -> f ()
{-# INLINE for_ #-}
for_ = flip traverse_

-- | Map each element of a structure to an action, evaluate these
-- actions from left to right, and ignore the results. For a version
-- that doesn't ignore the results see 'traverse'.
traverse_ :: (Applicative f, Foldable t, Suitable f ()) => (a -> f b) -> t a -> f ()
traverse_ f = foldr (\e a -> f e *> a) (pure ())

-- | Evaluate each action in the structure from left to right, and
-- ignore the results. For a version that doesn't ignore the results
-- see 'Data.Traversable.sequenceA'.
sequenceA_ :: (Foldable t, Applicative f, Suitable f ()) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

-- | @'guard' b@ is @'pure' ()@ if @b@ is 'True',
-- and 'empty' if @b@ is 'False'.
guard :: (Alternative f, Suitable f ()) => Bool -> f ()
guard True = pure ()
guard False = empty

-- | @'ensure' b x@ is @x@ if @b@ is 'True',
-- and 'empty' if @b@ is 'False'.
ensure :: (Alternative f, Suitable f a) => Bool -> f a -> f a
ensure True x = x
ensure False _ = empty

-- | Evaluate each action in the structure from left to right, and
-- and collect the results. For a version that ignores the results
-- see 'sequenceA_'.
sequenceA
    :: (Applicative f, Suitable t a, Suitable f (t a), Traversable t)
    => t (f a) -> f (t a)
sequenceA = traverse id

-- |The 'mapAccumL' function behaves like a combination of 'fmap'
-- and 'foldl'; it applies a function to each element of a structure,
-- passing an accumulating parameter from left to right, and returning
-- a final value of this accumulator together with the new structure.
mapAccumL :: (Traversable t, Suitable t c) => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumL f s t = swap $ runState (traverse (state . (swap .: flip f)) t) s where
  (.:) = (.).(.)

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

-- | @'void' value@ discards or ignores the result of evaluation, such
-- as the return value of an 'System.IO.IO' action.
--
-- ==== __Examples__
--
-- Replace the contents of a @'Maybe' 'Int'@ with unit:
--
-- >>> void Nothing
-- Nothing
-- >>> void (Just 3)
-- Just ()
--
-- Replace the contents of an @'Either' 'Int' 'Int'@ with unit,
-- resulting in an @'Either' 'Int' '()'@:
--
-- >>> void (Left 8675309)
-- Left 8675309
-- >>> void (Right 8675309)
-- Right ()
--
-- Replace every element of a list with unit:
--
-- >>> void [1,2,3]
-- [(),(),()]
--
-- Replace the second element of a pair with unit:
--
-- >>> void (1,2)
-- (1,())
--
-- Discard the result of an 'System.IO.IO' action:
--
-- >>> traverse print [1,2]
-- 1
-- 2
-- [(),()]
-- >>> void $ traverse print [1,2]
-- 1
-- 2
void :: (Functor f, Suitable f ()) => f a -> f ()
void = (<$) ()

--------------------------------------------------------------------------------
-- syntax
--------------------------------------------------------------------------------

-- | Function to which the @if ... then ... else@ syntax desugars to
ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ f = f

infixl 1 >>
-- | Sequence two actions, discarding the result of the first. Alias for
-- @('*>')@.
(>>)
    :: (Applicative f, Suitable f b)
    => f a -> f b -> f b
(>>) = (*>)

-- | Alias for 'pure'.
return
    :: (Applicative f, Suitable f a)
    => a -> f a
return = pure

--------------------------------------------------------------------------------
-- instances
--------------------------------------------------------------------------------

instance Functor [] where
    type Suitable [] a = ()
    fmap = map
    (<$) = (Prelude.<$)

instance Applicative [] where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Monad [] where
    (>>=) = (Prelude.>>=)

instance MonadFail [] where
    fail _ = []

instance Traversable [] where
    traverse f = foldr (liftA2 (:) . f) (pure [])

instance Functor Maybe where
    type Suitable Maybe a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative Maybe where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Alternative Maybe where
    empty = Control.Applicative.empty
    (<|>) = (Control.Applicative.<|>)

instance Monad Maybe where
    (>>=) = (Prelude.>>=)

instance MonadFail Maybe where
    fail _ = Nothing

instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse f (Just x) = fmap Just (f x)

instance Functor IO where
    type Suitable IO a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative IO where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Alternative IO where
    empty = Control.Applicative.empty
    (<|>) = (Control.Applicative.<|>)

instance Monad IO where
    (>>=) = (Prelude.>>=)

instance MonadFail IO where
    fail = Prelude.fail

instance Functor Identity where
    type Suitable Identity a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative Identity where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Monad Identity where
    (>>=) = (Prelude.>>=)

instance Traversable Identity where
    traverse f (Identity x) = fmap Identity (f x)

instance Functor (Either e) where
    type Suitable (Either e) a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative (Either a) where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Monad (Either a) where
    (>>=) = (Prelude.>>=)

instance IsString a =>
         MonadFail (Either a) where
    fail = Left . fromString

instance Traversable (Either a) where
    traverse f = either (pure . Left) (fmap Right . f)

instance Functor Set where
    type Suitable Set a = Ord a
    fmap = Set.map
    x <$ xs = if null xs then Set.empty else Set.singleton x

instance Applicative Set where
    pure = Set.singleton
    fs <*> xs = foldMap (`Set.map` xs) fs
    xs *> ys = if null xs then Set.empty else ys
    xs <* ys = if null ys then Set.empty else xs
    lower = lowerM

instance Monad Set where
    (>>=) = flip foldMap

instance MonadFail Set where
    fail _ = Set.empty

instance Alternative Set where
    empty = Set.empty
    (<|>) = Set.union

instance Functor (Map a) where
    type Suitable (Map a) b = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Functor ((,) a) where
    type Suitable ((,) a) b = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Monoid a => Applicative ((,) a) where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Monoid a => Monad ((,) a) where
    (>>=) = (Prelude.>>=)

instance Traversable ((,) a) where
    traverse f (x,y) = fmap ((,) x) (f y)

instance Functor IntMap where
    type Suitable IntMap a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Functor Seq where
    type Suitable Seq a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative Seq where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Alternative Seq where
    empty = Control.Applicative.empty
    (<|>) = (Control.Applicative.<|>)

instance Monad Seq where
    (>>=) = (Prelude.>>=)

instance MonadFail Seq where
    fail _ = empty

instance Functor Tree where
    type Suitable Tree a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative Tree where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Monad Tree where
    (>>=) = (Prelude.>>=)

instance Functor ((->) a) where
    type Suitable ((->) a) b = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative ((->) a) where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Monad ((->) a) where
  (>>=) = (Prelude.>>=)

instance Functor (ContT r m) where
    type Suitable (ContT r m) a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative (ContT r m) where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Monad (ContT r m) where
    (>>=) = (Prelude.>>=)

instance Functor Control.Applicative.ZipList where
    type Suitable Control.Applicative.ZipList a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative Control.Applicative.ZipList where
    lower = lowerP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure
    liftA2 = liftA2P
    liftA3 = liftA3P

instance Functor m => Functor (Strict.StateT s m) where
    type Suitable (Strict.StateT s m) a = Suitable m (a, s)
    fmap f m = Strict.StateT $ \ s ->
        (\ (!a, !s') -> (f a, s')) <$> Strict.runStateT m s
    {-# INLINE fmap #-}
    x <$ xs = Strict.StateT ((fmap.first) (const x) . Strict.runStateT xs)

instance Monad m =>
         Applicative (Strict.StateT s m) where
    pure a =
        Strict.StateT $
        \(!s) ->
             pure (a, s)
    {-# INLINE pure #-}
    Strict.StateT mf <*> Strict.StateT mx =
        Strict.StateT $
        \s -> do
            (f,s') <- mf s
            (x,s'') <- mx s'
            pure (f x, s'')
    Strict.StateT xs *> Strict.StateT ys =
        Strict.StateT $
        \(!s) -> do
            (_,s') <- xs s
            ys s'
    Strict.StateT xs <* Strict.StateT ys =
        Strict.StateT $
        \(!s) -> do
            (x,s') <- xs s
            (_,s'') <- ys s'
            pure (x,s'')
    lower = lowerM

instance (Monad m, Alternative m) => Alternative (Strict.StateT s m) where
    empty = Strict.StateT (const empty)
    {-# INLINE empty #-}
    Strict.StateT m <|> Strict.StateT n = Strict.StateT $ \ s -> m s <|> n s
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (Strict.StateT s m) where
    m >>= k = Strict.StateT $ \ s -> do
        (a, s') <- Strict.runStateT m s
        Strict.runStateT (k a) s'
    {-# INLINE (>>=) #-}

instance Functor m => Functor (StateT s m) where
    type Suitable (StateT s m) a = Suitable m (a, s)
    fmap f m = StateT $ \ s ->
        (\ ~(a, s') -> (f a, s')) <$> runStateT m s
    {-# INLINE fmap #-}
    x <$ StateT xs = StateT ((fmap.first) (const x) . xs)

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
    StateT xs <* StateT ys =
        StateT $
        \s -> do
            ~(x,s') <- xs s
            ~(_,s'') <- ys s'
            pure (x,s'')
    lower = lowerM

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

instance (Functor m) => Functor (ReaderT r m) where
    type Suitable (ReaderT r m) a = Suitable m a
    fmap f = mapReaderT (fmap f)
    {-# INLINE fmap #-}
    x <$ ReaderT xs = ReaderT (\r -> x <$ xs r)

instance (Applicative m) => Applicative (ReaderT r m) where
    pure = liftReaderT . pure
    {-# INLINE pure #-}
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r
    {-# INLINE (<*>) #-}
    lower ys = ReaderT $ \r -> lower (tr r ys) where
      tr :: r -> Ap (ReaderT r m) xs -> Ap m xs
      tr _ (Pure x) = Pure x
      tr r (Ap xs x) = Ap (tr r xs) (runReaderT x r)
    ReaderT xs *> ReaderT ys = ReaderT (\c -> xs c *> ys c)
    ReaderT xs <* ReaderT ys = ReaderT (\c -> xs c <* ys c)

instance (Alternative m) => Alternative (ReaderT r m) where
    empty   = liftReaderT empty
    {-# INLINE empty #-}
    m <|> n = ReaderT $ \ r -> runReaderT m r <|> runReaderT n r
    {-# INLINE (<|>) #-}

instance MonadFail m =>
         MonadFail (ReaderT r m) where
    fail = ReaderT . const . fail

instance (Monad m) => Monad (ReaderT r m) where
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    {-# INLINE (>>=) #-}

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)
{-# INLINE liftReaderT #-}

instance Functor m =>
         Functor (MaybeT m) where
    type Suitable (MaybeT m) a = (Suitable m (Maybe a), Suitable m a)
    fmap f (MaybeT xs) = MaybeT ((fmap . fmap) f xs)
    x <$ MaybeT xs = MaybeT (fmap (x <$) xs)

instance Monad m =>
         Applicative (MaybeT m) where
    pure x = MaybeT (pure (Just x))
    MaybeT fs <*> MaybeT xs = MaybeT (liftA2 (<*>) fs xs)
    lower = lowerM
    MaybeT xs *> MaybeT ys = MaybeT (liftA2 (*>) xs ys)
    MaybeT xs <* MaybeT ys = MaybeT (liftA2 (<*) xs ys)

instance Monad m =>
         Monad (MaybeT m) where
    MaybeT x >>= f = MaybeT (x >>= maybe (pure Nothing) (runMaybeT . f))

instance Monad m =>
         MonadFail (MaybeT m) where
    fail _ = empty

instance Monad m =>
         Alternative (MaybeT m) where
    empty = MaybeT (pure Nothing)
    MaybeT x <|> MaybeT y = MaybeT (x >>= maybe y (pure . Just))

instance Functor m =>
         Functor (ExceptT e m) where
    type Suitable (ExceptT e m) a = Suitable m (Either e a)
    fmap f (ExceptT xs) = ExceptT ((fmap . fmap) f xs)
    x <$ ExceptT xs = ExceptT (fmap (x <$) xs)

instance Monad m =>
         Applicative (ExceptT e m) where
    pure x = ExceptT (pure (Right x))
    ExceptT fs <*> ExceptT xs = ExceptT (liftA2 (<*>) fs xs)
    lower = lowerM
    ExceptT xs *> ExceptT ys = ExceptT (xs *> ys)
    ExceptT xs <* ExceptT ys = ExceptT (xs <* ys)

instance (Monad m, IsString e) => MonadFail (ExceptT e m) where
    fail = ExceptT . pure . Left . fromString

instance Monad m => Monad (ExceptT e m) where
  ExceptT xs >>= f = ExceptT (xs >>= either (pure . Left) (runExceptT . f))

instance (Monad m, Monoid e) => Alternative (ExceptT e m) where
  empty = ExceptT (pure (Left mempty))
  ExceptT xs <|> ExceptT ys = ExceptT (xs >>= either (const ys) (pure . Right))

instance Functor m =>
         Functor (IdentityT m) where
    type Suitable (IdentityT m) a = Suitable m a
    fmap =
        (coerce :: ((a -> b) -> f a -> f b) -> (a -> b) -> IdentityT f a -> IdentityT f b)
            fmap
    (<$) =
        (coerce :: (a -> f b -> f a) -> a -> IdentityT f b -> IdentityT f a)
            (<$)

instance Applicative m =>
         Applicative (IdentityT m) where
    pure = (coerce :: (a -> f a) -> a -> IdentityT f a) pure
    (<*>) =
        (coerce :: (f (a -> b) -> f a -> f b) -> IdentityT f (a -> b) -> IdentityT f a -> IdentityT f b)
            (<*>)
    lower =
        (coerce :: (Ap f xs -> f b) -> (Ap (IdentityT f) xs -> IdentityT f b))
            lower
    IdentityT xs *> IdentityT ys = IdentityT (xs *> ys)
    IdentityT xs <* IdentityT ys = IdentityT (xs <* ys)

instance Monad m =>
         Monad (IdentityT m) where
    (>>=) =
        (coerce :: (f a -> (a -> f b) -> f b) -> IdentityT f a -> (a -> IdentityT f b) -> IdentityT f b)
            (>>=)

instance MonadFail m =>
         MonadFail (IdentityT m) where
    fail = IdentityT . fail
