{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, FlexibleContexts, Safe #-}
{-|
  This module defines two classes. @'Forkable' m n@ means a monad @n@ may be forked in @m@.
  @'ForkableT' t@ means that applying the transformer to @n@ and @m@ will mean you can still fork @t n@ in @t m@.

  The reason we need a separate class for monad transformers is because often times the \"forkability\" of a transformed monad does not depend on the underlying monad, only it's forkability. This is the case for example for most standard monad transformers.
-}
module Control.Concurrent.ForkableT
    (
      ForkableT
    , Forkable
    )
where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Trans.Control

-- |ForkableT. The default instance uses 'MonadTransControl' to lift the underlying 'fork'
class ForkableT t where
    forkT :: (Forkable m n) => t n () -> t m ThreadId
    default forkT :: (MonadTransControl t, Forkable m n) => t n () -> t m ThreadId
    forkT t = liftWith $ \run -> fork $ run t >> return ()

-- |Forkable. The default instance uses 'ForkableT' and simply calls 'forkT'
class (MonadIO m, MonadIO n) => Forkable m n where
    fork :: n () -> m ThreadId
    default fork :: ForkableT t => t n () -> t m ThreadId
    fork = forkT

instance Forkable IO IO where
    fork = forkIO

instance ForkableT (ReaderT r)
instance ForkableT (StateT s)
instance (Monoid w) => ForkableT (WriterT w)
instance (Error e) => ForkableT (ErrorT e)
instance (Forkable m n) => Forkable (ReaderT r m) (ReaderT r n)
instance (Forkable m n) => Forkable (StateT s m) (StateT s n)
instance (Forkable m n, Error e) => Forkable (ErrorT e m) (ErrorT e n)

instance (Forkable m n) => Forkable (StateT s m) (ReaderT s n) where
    fork r = lift . runReaderT (fork r) =<< get
instance (Forkable m n) => Forkable (ReaderT s m) (StateT s n) where
    fork r = lift . evalStateT (fork r) =<< ask
