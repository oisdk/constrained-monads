{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

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
   -- * Horrible type-level stuff
   Vect(..)
  ,AppVect(..)
  ,liftAP
  ,liftAM
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
  ,replicateM
  ,void
  ,some
  ,many
  ,
   -- * Syntax
   ifThenElse
  ,fail
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

import           Control.Monad.Trans.Cont         (ContT)
import           Control.Monad.Trans.Except       (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Identity     (IdentityT (..))
import           Control.Monad.Trans.Maybe        (MaybeT (..))
import           Control.Monad.Trans.Reader       (ReaderT (..), mapReaderT)
import           Control.Monad.Trans.State        (StateT (..))
import qualified Control.Monad.Trans.State.Strict as Strict (StateT (..))

import           Control.Arrow (first)

--------------------------------------------------------------------------------
-- Type-level shenanigans
--------------------------------------------------------------------------------

-- | A heterogeneous list, for storing the arguments to 'liftAP. (There /has/ to
-- be a better way to do this).
infixr 5 :-
data Vect xs where
  Nil  :: Vect '[]
  (:-) :: x -> Vect xs -> Vect (x ': xs)

-- | Another heterogeneous list, for storing the arguments to 'liftAP, wrapped
-- in their applicatives.
infixr 5 :*
data AppVect f xs where
  NilA :: AppVect f '[]
  (:*) :: f x -> AppVect f xs -> AppVect f (x ': xs)

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
-- The default implementation is for types which conform to the Prelude's
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
-- @Control.Monad.Constrained.IntSet@.
class Functor f  where
    {-# MINIMAL fmap #-}
    -- | Indicate which types can be contained by 'f'. For instance,
    -- 'Data.Set.Set' conforms like so:
    --
    -- @instance 'Functor' 'Set' where
    --    type 'Suitable' 'Set' a = 'Ord' a
    --    'fmap' = 'Set.map'@
    type Suitable f a :: Constraint

    -- | Maps a function over a functor
    fmap
        :: Suitable f b
        => (a -> b) -> f a -> f b

    -- | Replace all values in the input with a default value.
    infixl 4 <$
    (<$) :: Suitable f a => a -> f b -> f a
    (<$) = fmap . const

-- | A functor with application, providing operations to
--
-- * embed pure expressions ('pure'), and
--
-- * sequence computations and combine their results ('<*>').
--
-- A minimal complete definition must include implementations of these
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
    {-# MINIMAL liftA #-}

    -- | Lift a value.
    pure
        :: Suitable f a
        => a -> f a
    pure x = liftA (\Nil -> x) NilA

    infixl 4 <*>

    -- | Sequential application.
    (<*>)
        :: Suitable f b
        => f (a -> b) -> f a -> f b
    fs <*> xs = liftA (\(f :- x :- Nil) -> f x) (fs :* xs :* NilA)

    infixl 4 *>
    -- | Sequence actions, discarding the value of the first argument.
    (*>)
        :: Suitable f b
        => f a -> f b -> f b
    (*>) = liftA2 (flip const)

    infixl 4 <*
    -- | Sequence actions, discarding the value of the second argument.
    (<*)
        :: Suitable f a
        => f a -> f b -> f a
    (<*) = liftA2 const
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
    --    'pure' (f x)@
    --
    -- But now we can't define the 'liftA' functions for things which are
    -- 'Applicative' but not 'Monad' (square matrices,
    -- 'Control.Applicative.ZipList's, etc). Also, some types have a more
    -- efficient @('<*>')@ than @('>>=')@ (see, for instance, the
    -- <https://simonmar.github.io/posts/2015-10-20-Fun-With-Haxl-1.html Haxl>
    -- monad).
    --
    -- The one missing piece is @-XApplicativeDo@: I can't figure out a way
    -- to get do-notation to desugar to using the 'liftAP functions, rather
    -- than @('<*>')@.
    --
    -- It would also be preferable to avoid the two intermediate structures
    -- ('Vect', 'AppVect', etc). Ideally GHC would optimize them away, but
    -- it seems unlikely.
    --
    -- Utility definitions of this function are provided: if your 'Applicative'
    -- is a @Prelude.'Prelude.Applicative'@, 'liftA' can be defined in terms of
    -- @('<*>')@. 'liftAP' does exactly this.
    --
    -- Alternatively, if your applicative is a 'Monad', 'liftA' can be defined
    -- in terms of @('>>=')@, which is what 'liftAM' does.
    liftA
        :: Suitable f b
        => (Vect xs -> b) -> AppVect f xs -> f b

    liftA2
        :: Suitable f c
        => (a -> b -> c) -> f a -> f b -> f c
    liftA2 f xs ys =
        liftA
            (\(x :- y :- Nil) ->
                  f x y)
            (xs :* ys :* NilA)
    liftA3
        :: Suitable f d
        => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
    liftA3 f xs ys zs =
        liftA
            (\(x :- y :- z :- Nil) ->
                  f x y z)
            (xs :* ys :* zs :* NilA)
    liftA4
        :: Suitable f e
        => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
    liftA4 f ws xs ys zs =
        liftA
            (\(w :- x :- y :- z :- Nil) ->
                  f w x y z)
            (ws :* xs :* ys :* zs :* NilA)
    liftA5
        :: Suitable f g
        => (a -> b -> c -> d -> e -> g)
        -> f a
        -> f b
        -> f c
        -> f d
        -> f e
        -> f g
    liftA5 f vs ws xs ys zs =
        liftA
            (\(v :- w :- x :- y :- z :- Nil) ->
                  f v w x y z)
            (vs :* ws :* xs :* ys :* zs :* NilA)

    liftA6
        :: Suitable f h
        => (a -> b -> c -> d -> e -> g -> h)
        -> f a
        -> f b
        -> f c
        -> f d
        -> f e
        -> f g
        -> f h
    liftA6 f us vs ws xs ys zs =
        liftA
            (\(u :- v :- w :- x :- y :- z :- Nil) ->
                  f u v w x y z)
            (us :* vs :* ws :* xs :* ys :* zs :* NilA)

    liftA7
        :: Suitable f i
        => (a -> b -> c -> d -> e -> g -> h -> i)
        -> f a
        -> f b
        -> f c
        -> f d
        -> f e
        -> f g
        -> f h
        -> f i
    liftA7 f ts us vs ws xs ys zs =
        liftA
            (\(t :- u :- v :- w :- x :- y :- z :- Nil) ->
                  f t u v w x y z)
            (ts :* us :* vs :* ws :* xs :* ys :* zs :* NilA)

    liftA8
        :: Suitable f j
        => (a -> b -> c -> d -> e -> g -> h -> i -> j)
        -> f a
        -> f b
        -> f c
        -> f d
        -> f e
        -> f g
        -> f h
        -> f i
        -> f j
    liftA8 f ss ts us vs ws xs ys zs =
        liftA
            (\(s :- t :- u :- v :- w :- x :- y :- z :- Nil) ->
                  f s t u v w x y z)
            (ss :* ts :* us :* vs :* ws :* xs :* ys :* zs :* NilA)

    liftA9
        :: Suitable f k
        => (a -> b -> c -> d -> e -> g -> h -> i -> j -> k)
        -> f a
        -> f b
        -> f c
        -> f d
        -> f e
        -> f g
        -> f h
        -> f i
        -> f j
        -> f k
    liftA9 f rs ss ts us vs ws xs ys zs =
        liftA
            (\(r :- s :- t :- u :- v :- w :- x :- y :- z :- Nil) ->
                  f r s t u v w x y z)
            (rs :* ss :* ts :* us :* vs :* ws :* xs :* ys :* zs :* NilA)

-- | A variant of '<*>' with the arguments reversed.
(<**>) :: (Applicative f, Suitable f b) => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

-- | A definition of 'liftA' which uses the Prelude's @('Prelude.<*>')@.
liftAP :: (Prelude.Applicative f) => (Vect xs -> b) -> (AppVect f xs -> f b)
liftAP f NilA = Prelude.pure (f Nil)
liftAP f (x :* NilA) = Prelude.fmap (f . (:-Nil)) x
liftAP f (x :* xs) =  ((f .) . (:-)) Prelude.<$> x Prelude.<*> liftAP id xs

-- | A definition of 'liftA' which uses 's @('>>=')@.
liftAM :: (Monad f, Suitable f b) => (Vect xs -> b) -> (AppVect f xs -> f b)
liftAM f NilA = pure (f Nil)
liftAM f (x :* NilA) = fmap (f . (:-Nil)) x
liftAM f (x :* xs) = x >>= \y -> liftAM (f . (y:-)) xs
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
defined in the "Prelude" satisfy these laws.
-}
class Applicative f =>
      Monad f  where
    infixl 1 >>=
    (>>=)
        :: Suitable f b
        => f a -> (a -> f b) -> f b

class Applicative f =>
      Alternative f  where
    empty :: Suitable f a => f a
    infixl 3 <|>
    (<|>)
        :: Suitable f a
        => f a -> f a -> f a

class (Foldable t, Functor t) =>
      Traversable t  where
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

ensure :: (Alternative f, Suitable f a) => Bool -> f a -> f a
ensure True x = x
ensure False _ = empty

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
void = (<$) ()

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
    fmap = map
    (<$) = (Prelude.<$)

instance Applicative [] where
  liftA = liftAP
  (<*>) = (Prelude.<*>)
  (*>) = (Prelude.*>)
  (<*) = (Prelude.<*)
  pure = Prelude.pure

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Monad [] where
  (>>=) = (Prelude.>>=)

instance Traversable [] where
  traverse f = foldr (liftA2 (:) . f) (pure [])

instance Functor Maybe where
    type Suitable Maybe a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative Maybe where
  liftA = liftAP
  (<*>) = (Prelude.<*>)
  (*>) = (Prelude.*>)
  (<*) = (Prelude.<*)
  pure = Prelude.pure

instance Alternative Maybe where
  empty = Control.Applicative.empty
  (<|>) = (Control.Applicative.<|>)

instance Monad Maybe where
  (>>=) = (Prelude.>>=)

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = fmap Just (f x)

instance Functor IO where
    type Suitable IO a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative IO where
    liftA = liftAP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure

instance Alternative IO where
    empty = Control.Applicative.empty
    (<|>) = (Control.Applicative.<|>)

instance Monad IO where
    (>>=) = (Prelude.>>=)

instance Functor Identity where
    type Suitable Identity a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative Identity where
    liftA = liftAP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure

instance Monad Identity where
    (>>=) = (Prelude.>>=)

instance Traversable Identity where
  traverse f (Identity x) = fmap Identity (f x)

instance Functor (Either e) where
    type Suitable (Either e) a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative (Either a) where
    liftA = liftAP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure

instance Monad (Either a) where
    (>>=) = (Prelude.>>=)

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
    liftA = liftAM

instance Monad Set where
    (>>=) = flip foldMap

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
    liftA = liftAP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure

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
    liftA = liftAP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure

instance Alternative Seq where
  empty = Control.Applicative.empty
  (<|>) = (Control.Applicative.<|>)

instance Monad Seq where
  (>>=) = (Prelude.>>=)

instance Functor ((->) a) where
    type Suitable ((->) a) b = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative ((->) a) where
    liftA = liftAP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure

instance Monad ((->) a) where
  (>>=) = (Prelude.>>=)

instance Functor (ContT r m) where
    type Suitable (ContT r m) a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)
instance Applicative (ContT r m) where
    liftA = liftAP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure

instance Monad (ContT r m) where
    (>>=) = (Prelude.>>=)

instance Functor Control.Applicative.ZipList where
    type Suitable Control.Applicative.ZipList a = ()
    fmap = Prelude.fmap
    (<$) = (Prelude.<$)

instance Applicative Control.Applicative.ZipList where
    liftA = liftAP
    (<*>) = (Prelude.<*>)
    (*>) = (Prelude.*>)
    (<*) = (Prelude.<*)
    pure = Prelude.pure

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
            (_,s') <- ys s
            xs s'
    liftA = liftAM

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
            ~(_,s') <- ys s
            xs s'
    liftA = liftAM

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
    liftA f ys = ReaderT $ \r -> liftA f (tr r ys) where
      tr :: Functor m => r -> AppVect (ReaderT r m) xs -> AppVect m xs
      tr _ NilA = NilA
      tr r (xs :* NilA) = runReaderT xs r :* NilA
      tr r (x :* xs) = runReaderT x r :* tr r xs
    ReaderT xs *> ReaderT ys = ReaderT (\c -> xs c *> ys c)
    ReaderT xs <* ReaderT ys = ReaderT (\c -> xs c <* ys c)

instance (Alternative m) => Alternative (ReaderT r m) where
    empty   = liftReaderT empty
    {-# INLINE empty #-}
    m <|> n = ReaderT $ \ r -> runReaderT m r <|> runReaderT n r
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (ReaderT r m) where
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    {-# INLINE (>>=) #-}

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)
{-# INLINE liftReaderT #-}

instance Functor m => Functor (MaybeT m) where
  type Suitable (MaybeT m) a = (Suitable m (Maybe a), Suitable m a)
  fmap f (MaybeT xs) = MaybeT ((fmap.fmap) f xs)
  x <$ MaybeT xs = MaybeT (fmap (x<$) xs)

instance Monad m => Applicative (MaybeT m) where
  pure x = MaybeT (pure (Just x))
  MaybeT fs <*> MaybeT xs = MaybeT (liftA2 (<*>) fs xs)
  liftA = liftAM
  MaybeT xs *> MaybeT ys = MaybeT (xs *> ys)
  MaybeT xs <* MaybeT ys = MaybeT (xs <* ys)

instance Monad m => Monad (MaybeT m) where
  MaybeT x >>= f = MaybeT (x >>= maybe (pure Nothing) (runMaybeT . f))

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
    liftA = liftAM
    ExceptT xs *> ExceptT ys = ExceptT (xs *> ys)
    ExceptT xs <* ExceptT ys = ExceptT (xs <* ys)

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
    liftA f =
        (coerce :: (AppVect f xs -> f b) -> (AppVect (IdentityT f) xs -> IdentityT f b))
            (liftA f)
    IdentityT xs *> IdentityT ys = IdentityT (xs *> ys)
    IdentityT xs <* IdentityT ys = IdentityT (xs <* ys)

instance Monad m =>
         Monad (IdentityT m) where
    (>>=) =
        (coerce :: (f a -> (a -> f b) -> f b) -> IdentityT f a -> (a -> IdentityT f b) -> IdentityT f b)
            (>>=)
