{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE ViewPatterns           #-}

-- | This module duplicates the Control.Monad.Writer module from the mtl, for
-- constrained monads. It also provides a non-leaky writer monad.
module Control.Monad.Constrained.Writer
  (MonadWriter(..)
  ,WriterT
  ,pattern WriterT
  ,Writer
  ,runWriterT
  ,execWriterT
  ,execWriter
  ,runWriter
  ,listen
  ,pass
  ,evalWriterT
  ,evalWriter
  )where

import           GHC.Exts

import           Control.Monad.Constrained
import           Control.Monad.Constrained.Trans

import qualified Control.Monad.Trans.State.Lazy   as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Identity     as Identity
import qualified Control.Monad.Trans.Maybe        as Maybe
import qualified Control.Monad.Trans.Reader       as Reader
import qualified Control.Monad.Trans.Except       as Except

import           Control.Monad.Constrained.State
import           Control.Monad.Constrained.Error
import           Control.Monad.Constrained.Reader

import           Data.Functor.Identity
import           Data.Functor.Classes

-- | A class for monads with logging ability.
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    type WriterSuitable m a :: Constraint
    -- | Embed a simple writer action.
    writer  :: WriterSuitable m a => (a,w) -> m a
    -- | Log some output.
    tell    :: WriterSuitable m () => w -> m ()
    -- | This is equivalent to the 'Control.Monad.Trans.Writer.Lazy.listen'
    -- function, except it is church encoded, to make the constraints a little
    -- easier to manage.
    listenC :: WriterSuitable m b => (a -> w -> b) -> m a -> m b
    -- | This is equivalent to the 'Control.Monad.Trans.Writer.Lazy.pass'
    -- function, except it is church encoded, to make the constraints a little
    -- easier to manage.
    passC   :: WriterSuitable m a => (a -> w -> w) -> m a -> m a

instance MonadWriter w m =>
         MonadWriter w (Except.ExceptT e m) where
    type WriterSuitable (Except.ExceptT e m) a
        = (WriterSuitable m a
          ,WriterSuitable m (Either e a)
          ,Suitable m (Either e a))
    writer = lift . writer
    tell = lift . tell
    listenC f = (Except.mapExceptT . listenC . flip) (fmap . flip f)
    passC = Except.mapExceptT . passC . either (const id)

-- | @'listen' m@ is an action that executes the action @m@ and adds
-- its output to the value of the computation.
listen
    :: (MonadWriter w m, WriterSuitable m (a, w))
    => m a -> m (a, w)
listen = listenC (,)

-- | @'pass' m@ is an action that executes the action @m@, which
-- returns a value and a function, and returns the value, applying
-- the function to the output.
pass
    :: (MonadWriter w m, Suitable m a, WriterSuitable m (a, w -> w))
    => m (a, w -> w) -> m a
pass = fmap fst . passC snd

instance MonadWriter w m =>
         MonadWriter w (State.Lazy.StateT s m) where
    type WriterSuitable (State.Lazy.StateT s m) a
        = (WriterSuitable m a
          ,WriterSuitable m (a, s)
          ,Suitable m (a, s))
    writer = lift . writer
    tell = lift . tell
    listenC f m =
        State.Lazy.StateT
            (listenC
                 (\ ~(a,s') w ->
                       (f a w, s')) .
             State.Lazy.runStateT m)
    passC c m = State.Lazy.StateT (passC (c . fst) . State.Lazy.runStateT m)

instance MonadWriter w m =>
         MonadWriter w (State.Strict.StateT s m) where
    type WriterSuitable (State.Strict.StateT s m) a
        = (WriterSuitable m a
          ,WriterSuitable m (a, s)
          ,Suitable m (a, s))
    writer = lift . writer
    tell = lift . tell
    listenC f m =
        State.Strict.StateT
            (listenC
                 (\ (a,s') w ->
                       (f a w, s')) .
             State.Strict.runStateT m)
    passC c m = State.Strict.StateT (passC (c . fst) . State.Strict.runStateT m)

instance MonadWriter w m =>
         MonadWriter w (Identity.IdentityT m) where
    type WriterSuitable (Identity.IdentityT m) a = WriterSuitable m a
    writer = lift . writer
    tell = lift . tell
    listenC f = Identity.mapIdentityT (listenC f)
    passC f = Identity.mapIdentityT (passC f)

instance MonadWriter w m => MonadWriter w (Maybe.MaybeT m) where
    type WriterSuitable (Maybe.MaybeT m) a
        = (WriterSuitable m a
          ,WriterSuitable m (Maybe a)
          ,Suitable m (Maybe a))
    writer = lift . writer
    tell = lift . tell
    listenC f = (Maybe.mapMaybeT . listenC . flip) (fmap . flip f)
    passC = Maybe.mapMaybeT . passC . maybe id

instance MonadWriter w m => MonadWriter w (Reader.ReaderT r m) where
    type WriterSuitable (Reader.ReaderT r m) a = WriterSuitable m a
    writer = lift . writer
    tell = lift . tell
    listenC f = Reader.mapReaderT (listenC f)
    passC f = Reader.mapReaderT (passC f)

-- | A monad transformer similar to 'Control.Monad.Writer.Strict.WriterT', except
-- that it does not leak space. It is implemented using a state monad, so that
-- `mappend` is tail recursive. See
-- <https://mail.haskell.org/pipermail/libraries/2013-March/019528.html this>
-- email to the Haskell libraries committee for more information.
--
-- Wherever possible, coercions are used to eliminate any overhead from the
-- newtype wrapper.
newtype WriterT s m a =
    WriterT_ { unWriterT :: State.Strict.StateT s m a }

instance Functor m => Functor (WriterT s m) where
  type Suitable (WriterT  s m) a = Suitable m (a,s)
  fmap f (WriterT_ x) = WriterT_ (fmap f x)
  x <$ WriterT_ xs = WriterT_ (x <$ xs)

instance Monad m =>
         Applicative (WriterT s m) where
    pure x = WriterT_ (pure x)
    WriterT_ fs <*> WriterT_ xs = WriterT_ (fs <*> xs)
    WriterT_ xs *> WriterT_ ys = WriterT_ (xs *> ys)
    WriterT_ xs <* WriterT_ ys = WriterT_ (xs <* ys)
    liftA = liftAM

instance Monad m => Monad (WriterT s m) where
  WriterT_ xs >>= f = WriterT_ (xs >>= (unWriterT . f))

-- first_  :: (Functor f, Suitable f (b, c)) => (a -> f b) -> (a, c) -> f (b, c)
-- first_  f (x,y) = fmap (flip (,) y) (f x)

-- | Run a writer computation in the underlying monad.
runWriterT
    :: Monoid s
    => WriterT s m a -> m (a, s)
runWriterT =
    (coerce :: (State.Strict.StateT s m a -> m (a, s)) -> WriterT s m a -> m (a, s))
        (`State.Strict.runStateT` mempty)

-- | This pattern gives the newtype wrapper around 'StateT' the same interface
-- as 'Control.Monad.Writer.Strict.WriterT'. Unfortunately, GHC currently warns
-- that a function is incomplete wherever this pattern is used. This issue
-- should be solved in a future version of GHC, when the
-- <https://ghc.haskell.org/trac/ghc/ticket/8779 COMPLETE> pragma is
-- implemented.
pattern WriterT :: (Functor m, Monoid s, Suitable m (a, s)) =>
        m (a, s) -> WriterT s m a

pattern WriterT x <- (runWriterT -> x)
  where WriterT y
          = WriterT_ (State.Strict.StateT (\ s -> (fmap.fmap) (mappend s) y))

-- | A type synonym for the plain (non-transformer) version of 'WriterT'. This
-- can be used as if it were defined as:
--
-- > newtype Writer w a = Writer { runWriter :: (a, w) }
type Writer s = WriterT s Identity

-- | Run a writer computation.
--
-- >>> runWriter $ traverse (\x -> writer (show x, [x])) [1..5]
-- (["1","2","3","4","5"],[1,2,3,4,5])
runWriter
    :: Monoid s
    => Writer s a -> (a, s)
runWriter =
    (coerce
       :: (WriterT s Identity a -> Identity (a, s))
       -> (WriterT s Identity a -> (a, s))
    ) runWriterT

{-# INLINE runWriter #-}

instance (Monoid s, Monad m) =>
         MonadWriter s (WriterT s m) where
    type WriterSuitable (WriterT s m) a = Suitable m (a, s)
    tell s = WriterT (pure ((), s))
    writer (x,s) = WriterT (pure (x, s))
    {-# INLINE writer #-}
    listenC f (WriterT_ xs) =
        WriterT_
            (State.Strict.StateT
                 (fmap
                      (\(x,s') ->
                            (f x s', s')) .
                  State.Strict.runStateT xs))
    {-# INLINE listenC #-}
    passC f (WriterT_ xs) =
        WriterT_
            (State.Strict.StateT
                 (fmap
                           (\(x,s') ->
                                 (x, f x s')) . State.Strict.runStateT xs))
    {-# INLINE passC #-}

instance MonadTrans (WriterT w) where
  type SuitableLift (WriterT w) m a = Suitable m (a, w)
  lift xs = WriterT_ . State.Strict.StateT $ (\s -> fmap (flip (,) s) xs)

instance MonadState s m =>
         MonadState s (WriterT w m) where
    type StateSuitable (WriterT w m) a = (StateSuitable m a, Suitable m (a, w))
    get = lift get
    put = lift . put
    state = lift . state

instance MonadError e m =>
         MonadError e (WriterT w m) where
    type SuitableError (WriterT w m) a = SuitableError m (a, w)
    throwError e = WriterT_ . State.Strict.StateT $ const (throwError e)
    catchError (WriterT_ xs) f =
        WriterT_ (State.Strict.liftCatch catchError xs (unWriterT . f))

instance MonadReader r m =>
         MonadReader r (WriterT w m) where
    type ReaderSuitable (WriterT w m) a
        = (ReaderSuitable m a
          ,Suitable m (a, w)
          ,ReaderSuitable m (a, w))
    ask = WriterT_ ask
    reader x = WriterT_ (reader x)
    local f (WriterT_ xs) = WriterT_ (local f xs)

-- | Run a writer computation in the underlying monad, and return its result.
evalWriterT
    :: (Monad m, Monoid s, Suitable m a)
    => WriterT s m a -> m a
evalWriterT = fmap fst . runWriterT

{-# INLINE evalWriterT #-}

-- | Run a writer computation in the underlying monad, and collect its output.
execWriterT
    :: (Monad m, Monoid s, Suitable m s)
    => WriterT s m a -> m s
execWriterT = fmap snd . runWriterT

{-# INLINE execWriterT #-}

-- | Run a writer computation, and return its result.
evalWriter
    :: Monoid s
    => Writer s a -> a
evalWriter = fst . runWriter

{-# INLINE evalWriter #-}

-- | Run a writer computation, and collect its output.
execWriter
    :: Monoid s
    => Writer s a -> s
execWriter = snd . runWriter

{-# INLINE execWriter #-}

instance (Foldable m, Monoid w) =>
         Foldable (WriterT w m) where
    foldMap f =
        foldMap
            (\(x,_) ->
                  f x) .
        runWriterT

-- instance (Traversable m, Monoid w) =>
--          Traversable (WriterT w m) where
--     traverse f x = WriterT <$> (traverse . first_) f (runWriterT x)

instance (Eq1 m, Eq w, Monoid w) =>
         Eq1 (WriterT w m) where
    liftEq eq x y =
        liftEq
            (\(xx,xy) (yx,yy) ->
                  eq xx yx && xy == yy)
            (runWriterT x)
            (runWriterT y)

instance (Ord1 m, Ord w, Monoid w) =>
         Ord1 (WriterT w m) where
    liftCompare cmp x y =
        liftCompare
            (\(xx,xy) (yx,yy) ->
                  cmp xx yx `mappend` compare xy yy)
            (runWriterT x)
            (runWriterT y)

-- instance (Read w, Read1 m, Monoid w, Functor m) =>
--          Read1 (WriterT w m) where
--     liftReadsPrec rp rl =
--         readsData $ readsUnaryWith (liftReadsPrec rp' rl') "WriterT" WriterT_
--       where
--         rp' = liftReadsPrec2 rp rl readsPrec readList
--         rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m, Monoid w) =>
         Show1 (WriterT w m) where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp' sl') "WriterT" d (runWriterT m)
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList

instance (Eq w, Eq1 m, Eq a, Monoid w) =>
         Eq (WriterT w m a) where
    (==) = eq1

instance (Ord w, Ord1 m, Ord a, Monoid w) =>
         Ord (WriterT w m a) where
    compare = compare1

-- instance (Read w, Read1 m, Read a, Monoid w, Functor m) =>
--          Read (WriterT w m a) where
--     readsPrec = readsPrec1

instance (Show w, Show1 m, Show a, Monoid w) =>
         Show (WriterT w m a) where
    showsPrec = showsPrec1
