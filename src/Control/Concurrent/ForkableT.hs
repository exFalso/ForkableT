{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses #-}
module Control.Concurrent.ForkableT where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans.Control

-- ForkableT
class ForkableT t where
    forkT :: (Forkable m n) => t n () -> t m ThreadId
    default forkT :: (MonadTransControl t, Forkable m n) => t n () -> t m ThreadId
    forkT t = liftWith $ \run -> fork $ run t >> return ()

-- Forkable
class (MonadIO m, MonadIO n) => Forkable m n where
    fork :: n () -> m ThreadId
    default fork :: ForkableT t => t n () -> t m ThreadId
    fork = forkT

instance Forkable IO IO where
    fork = forkIO

instance ForkableT (ReaderT r)
instance (Forkable m n) => Forkable (ReaderT r m) (ReaderT r n)
