{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-|
  This module defines non-Prelude 'Forkable'/'ForkableT' instances. It is separated from "Control.Concurrent.Forkable" because imported modules might not be -XSafe
-}
module Control.Concurrent.ForkableT.Instances
    ( module Control.Concurrent.ForkableT
    )
where

import Control.Concurrent.ForkableT

import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.State

-- ResourceT -should- be an instance of ForkableT, however the exposed functionality does not allow this for now.
instance (MonadBaseControl IO m, MonadIO m) => Forkable (ResourceT m) (ResourceT m) where
    fork = resourceForkIO
