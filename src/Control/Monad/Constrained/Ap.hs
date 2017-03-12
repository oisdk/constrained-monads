{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies     #-}

-- | This module allows the use of the Applicative Do extension with
-- constrained monads.
module Control.Monad.Constrained.Ap where

import           Control.Monad.Constrained        (Ap (..), liftAp, lower)
import qualified Control.Monad.Constrained        as Constrained

import           GHC.Exts

import qualified Control.Monad
import           Prelude                          hiding (Monad (..))
import qualified Prelude

import           Control.Monad.Trans.Cont         (ContT)
import           Control.Monad.Trans.Except       (ExceptT)
import           Control.Monad.Trans.Identity     (IdentityT (..))
import           Control.Monad.Trans.Maybe        (MaybeT)
import           Control.Monad.Trans.Reader       (ReaderT (..))
import           Control.Monad.Trans.State        (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import           Data.Functor.Identity            (Identity)
import           Data.Sequence                    (Seq)

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
instance Constrained.Monad f =>
         Monad (Ap f) where
    type Suitable (Ap f) a = Constrained.Suitable f a
    (>>=) ap f = liftAp (lower ap Constrained.>>= (lower . f))
    join = liftAp . go id . fmap lower
      where
        go
            :: forall a f b.
               (Constrained.Suitable f b, Constrained.Monad f)
            => (a -> f b) -> Ap f a -> f b
        go c (Pure x) = c x
        go f (Ap xs x) =
            go
                (\c ->
                      x Constrained.>>= (f . c))
                xs

return :: Applicative f => a -> f a
return = pure

fail :: String -> a
fail = error

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

instance Monad [] where
    type Suitable [] a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad Maybe where
    type Suitable Maybe a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad IO where
    type Suitable IO a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad Identity where
    type Suitable Identity a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad (Either e) where
    type Suitable (Either e) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monoid m =>
         Monad ((,) m) where
    type Suitable ((,) m) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad Seq where
    type Suitable Seq a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

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

instance Prelude.Monad m =>
         Monad (MaybeT m) where
    type Suitable (MaybeT m) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Prelude.Monad m =>
         Monad (ExceptT e m) where
    type Suitable (ExceptT e m) a = ()
    (>>=) = (Prelude.>>=)
    join = Control.Monad.join

instance Monad m =>
         Monad (IdentityT m) where
    type Suitable (IdentityT m) a = Suitable m a
    (>>=) =
        (coerce :: (f a -> (a -> f b) -> f b) -> IdentityT f a -> (a -> IdentityT f b) -> IdentityT f b)
            (>>=)
    join (IdentityT x) = IdentityT (join (fmap runIdentityT x))
