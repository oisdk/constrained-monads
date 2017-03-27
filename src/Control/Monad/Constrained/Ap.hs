{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module allows the use of the Applicative Do extension with
-- constrained monads.
module Control.Monad.Constrained.Ap
  (Monad(..)
  ,MonadFail(..)
  ,Codensity(..)
  ,ConstrainedWrapper(..)
  ,return
  ,ifThenElse
  ,(>>)
  ,Initial
  ,Final
  ,FreeApplicative(..)
  ,module RestPrelude)
  where

import qualified Control.Monad.Constrained        as Constrained

import           GHC.Exts

import qualified Control.Monad
import           Prelude                          as RestPrelude hiding (Monad (..))
import qualified Prelude

import           Control.Monad.Trans.Cont         (ContT)
import           Control.Monad.Trans.Except       (ExceptT(..))
import           Control.Monad.Trans.Identity     (IdentityT (..))
import           Control.Monad.Trans.Maybe        (MaybeT(..))
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State        (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import           Data.Functor.Identity            (Identity)
import           Data.Sequence                    (Seq)

import qualified Control.Applicative.Free.Final as Final
import qualified Control.Applicative.Free as Initial

-- | This class is for types which have no constraints on their applicative
-- operations, but /do/ have constraints on the monadic operations.
--
-- Most types which can conform are just standard unconstrained monads, with
-- the exception of the free applicative. The type @'Ap' f a@ is an applicative
-- for /any/ @f@. However, it can only be made a monad by interpreting the
-- underlying type (which may be constrained), running the monadic operation,
-- and then lifting the result. In practice, this allows you to write code on
-- on the @Ap@ type, using applicative do notation, and have it be interpreted
-- correctly.
--
-- For instance, take the following expression:
--
-- @example = do
--   x <- pure 1
--   y <- pure 2
--   pure (x + y)@
--
-- With the standard constrained monad module, you can instantiate that at
-- any type which is a constrained monad. 'Data.Set.Set', for instance. However,
-- if @-XApplicativeDo@ is turned on, you will get the error:
--
-- @No instance for ('Ord' ('Integer' -> 'Data.Set.Set' 'Integer'))@
--
-- The solution is to use @'Ap' 'Data.Set.Set'@ instead, which has the same
-- constraints on expressions built with '<*>' as those built with '>>='.
class Applicative f =>
      Monad f  where
    type Suitable f a :: Constraint
    infixl 1 >>=
    (>>=)
        :: (Suitable f a, Suitable f b)
        => f a -> (a -> f b) -> f b
    join
        :: Suitable f a
        => f (f a) -> f a

-- | See
-- <https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad-Fail.html here>
-- for more details.
class Monad f => MonadFail f where
  -- | Called when a pattern match fails in do-notation.
  fail :: Suitable f a => String -> f a

instance (Constrained.Monad f) =>
         Monad (Initial f) where
    type Suitable (Initial f) a = Constrained.Suitable f a
    (>>=) ap f = Initial.liftAp (phi ap Constrained.>>= (phi . f))
    {-# INLINE (>>=) #-}
    join = Initial.liftAp . go phi
      where
        go
            :: forall a f b.
               (Constrained.Suitable f b, Constrained.Monad f)
            => (a -> f b) -> Initial f a -> f b
        go c (Initial.Pure x) = c x
        go f (Initial.Ap x xs) = x Constrained.>>= (\xx -> go (\c -> (f . c) xx) xs)
    {-# INLINE join #-}

type Initial = Initial.Ap
type Final = Final.Ap

instance (Constrained.Monad f) =>
         Monad (Final f) where
    type Suitable (Final f) a = (Constrained.Suitable f a, Constrained.Suitable f (f a))
    (>>=) ap f = Final.liftAp (phi ap Constrained.>>= (phi . f))
    {-# INLINE (>>=) #-}
    join = Final.liftAp . Constrained.join . phi . fmap phi
    {-# INLINE join #-}

newtype Codensity f a = Codensity
    { runCodensity :: forall b. Constrained.Suitable f b =>
                                (a -> f b) -> f b
    }

instance Functor (Codensity k) where
  fmap f (Codensity m) = Codensity (\k -> m (k . f))
  {-# INLINE fmap #-}

instance Applicative (Codensity f) where
  pure x = Codensity (\k -> k x)
  {-# INLINE pure #-}
  Codensity f <*> Codensity g = Codensity (\bfr -> f (\ab -> g (bfr . ab)))
  {-# INLINE (<*>) #-}

instance (Constrained.Monad f) => Monad (Codensity f) where
  type Suitable (Codensity f) a = Constrained.Suitable f a
  m >>= k = eta (phi m Constrained.>>= (phi . k))
  {-# INLINE (>>=) #-}
  join (Codensity xs) = Codensity (Constrained.=<< xs phi)
  {-# INLINE join #-}

class FreeApplicative ap f where
  eta :: f a -> ap f a
  phi :: (Constrained.Suitable f a) => ap f a -> f a

newtype ConstrainedWrapper f a
  = ConstrainedWrapper
  { unwrapConstrained :: Constrained.Unconstrained f a }

instance Constrained.Applicative f => FreeApplicative ConstrainedWrapper f where
  eta = ConstrainedWrapper . Constrained.eta
  {-# INLINE eta #-}
  phi (ConstrainedWrapper xs) = Constrained.phi xs
  {-# INLINE phi #-}

instance Constrained.Applicative f =>
         Functor (ConstrainedWrapper f) where
    fmap f (ConstrainedWrapper xs) = ConstrainedWrapper (fmap f xs)
    {-# INLINE fmap #-}

instance Constrained.Applicative f =>
         Applicative (ConstrainedWrapper f) where
    pure = ConstrainedWrapper . pure
    ConstrainedWrapper fs <*> ConstrainedWrapper xs =
        ConstrainedWrapper (fs <*> xs)
    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Constrained.Monad f =>
         Monad (ConstrainedWrapper f) where
    type Suitable (ConstrainedWrapper f) a
        = (Constrained.Suitable f a, Constrained.Suitable f (f a))
    ConstrainedWrapper xs >>= f =
        eta (Constrained.phi xs Constrained.>>= (phi . f))
    {-# INLINE (>>=) #-}
    join =
        eta .
        Constrained.join . phi . fmap phi
    {-# INLINE join #-}

instance Constrained.Applicative f => FreeApplicative Final f where
  eta = Final.liftAp
  {-# INLINE eta #-}
  phi = Constrained.phi . Final.runAp Constrained.eta
  {-# INLINE phi #-}

instance Constrained.Applicative f => FreeApplicative Initial f where
  eta = Initial.liftAp
  {-# INLINE eta #-}
  phi = Constrained.phi . Initial.runAp Constrained.eta
  {-# INLINE phi #-}

instance Constrained.Monad f => FreeApplicative Codensity f where
  eta xs = Codensity (xs Constrained.>>=)
  {-# INLINE eta #-}
  phi (Codensity fs) = fs Constrained.pure
  {-# INLINE phi #-}

-- | An alias for 'pure'
return :: Applicative f => a -> f a
return = pure
{-# INLINE return #-}

-- | Function to which the @if ... then ... else@ syntax desugars to
ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ f = f

infixl 1 >>
-- | Sequence two actions, discarding the result of the first. Alias for
-- @('*>')@.
(>>)
    :: Applicative f
    => f a -> f b -> f b
(>>) = (*>)
{-# INLINE (>>) #-}

instance Monad [] where
    type Suitable [] a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance MonadFail [] where
    fail _ = []

instance Monad Maybe where
    type Suitable Maybe a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance MonadFail Maybe where
    fail _ = Nothing

instance Monad IO where
    type Suitable IO a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance MonadFail IO where
    fail = Prelude.fail

instance Monad Identity where
    type Suitable Identity a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad (Either e) where
    type Suitable (Either e) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance IsString a =>
         MonadFail (Either a) where
    fail = Left . fromString

instance Monoid m =>
         Monad ((,) m) where
    type Suitable ((,) m) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad Seq where
    type Suitable Seq a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance MonadFail Seq where
    fail _ = Constrained.empty

instance Monad ((->) b) where
    type Suitable ((->) b) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad (ContT r m) where
    type Suitable (ContT r m) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Prelude.Monad m =>
         Monad (Strict.StateT s m) where
    type Suitable (Strict.StateT s m) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Prelude.Monad m =>
         Monad (StateT s m) where
    type Suitable (StateT s m) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad m =>
         Monad (ReaderT s m) where
    type Suitable (ReaderT s m) a = Suitable m a
    m >>= k =
        ReaderT $
        \r -> do
            a <- runReaderT m r
            runReaderT (k a) r
    {-# INLINE (>>=) #-}
    join (ReaderT x) =
        ReaderT
            (\r ->
                  join (flip runReaderT r <$> x r))
    {-# INLINE join #-}

instance MonadFail m =>
         MonadFail (ReaderT r m) where
    fail = ReaderT . const . fail

instance Prelude.Monad m =>
         Monad (MaybeT m) where
    type Suitable (MaybeT m) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Prelude.Monad m =>
         MonadFail (MaybeT m) where
    fail _ = Control.Monad.mzero

instance Prelude.Monad m =>
         Monad (ExceptT e m) where
    type Suitable (ExceptT e m) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance (Prelude.Monad m, IsString e) => MonadFail (ExceptT e m) where
    fail = ExceptT . pure . Left . fromString

instance Monad m =>
         Monad (IdentityT m) where
    type Suitable (IdentityT m) a = Suitable m a
    (>>=) =
        (coerce
           :: (f a -> (a -> f b) -> f b)
           -> IdentityT f a -> (a -> IdentityT f b) -> IdentityT f b)
            (>>=)
    join (IdentityT x) = IdentityT (join (fmap runIdentityT x))

instance MonadFail m =>
         MonadFail (IdentityT m) where
    fail = IdentityT . fail
