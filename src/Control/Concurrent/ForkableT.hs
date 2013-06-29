{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
module Control.Concurrent.ForkableT where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans.Resource

-- ForkableT
class ForkableT t where
    forkT :: (Forkable m n) => t n () -> t m ThreadId

-- Forkable
class (MonadIO m, MonadIO n) => Forkable m n where
    fork :: n () -> m ThreadId
    default fork :: ForkableT t => t n () -> t m ThreadId
    fork = forkT

instance ForkableT (ReaderT r) where
    forkT m = lift . fork . runReaderT m =<< ask

instance Forkable IO IO where
    fork = forkIO

instance (Forkable m n) => Forkable (ReaderT r m) (ReaderT r n)

instance (MonadBaseControl IO m, MonadIO m) => Forkable (ResourceT m) (ResourceT m) where
    fork = resourceForkIO


