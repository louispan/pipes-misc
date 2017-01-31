{-# LANGUAGE RankNTypes #-}

module Pipes.Misc.Concurrent where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Pipes as P
import qualified Pipes.Concurrent as PC

-- | Like Pipes.Concurrent.fromInput, but stays in STM.
-- Using @hoist atomically@ to convert to IO monad seems to work.
-- Do not use @unsafeHoist atomically@.
fromInputSTM :: PC.Input a -> P.Producer' a STM ()
fromInputSTM as = void $ runMaybeT $ forever $ do
    a <- MaybeT $ lift $ PC.recv as
    lift $ P.yield a

-- | Like Pipes.Concurrent.toOutput, but stays in STM.
-- Using @hoist atomically@ to convert to IO monad seems to work.
-- Do not use @unsafeHoist atomically@.
toOutputSTM :: PC.Output a -> P.Consumer' a STM ()
toOutputSTM output = void $ runMaybeT $ forever $ do
    a <- lift P.await
    p <- lift $ lift $ PC.send output a
    guard p

-- | Converts a Producer in IO monad to a producer in STM monad.
toSTM :: PC.Buffer a -> P.Producer a IO () -> IO (P.Producer a STM ())
toSTM b xs = do
    (output, input) <- PC.spawn b
    void . forkIO . void . forever . P.runEffect $ xs P.>-> PC.toOutput output
    pure (fromInputSTM input)

-- | Converts a Producer in IO monad to a producer in STM monad. Also returns the seal.
toSTM' :: PC.Buffer a -> P.Producer a IO () -> IO (STM (), P.Producer a STM ())
toSTM' b xs = do
    (output, input, seal) <- PC.spawn' b
    void . forkIO . void . forever . P.runEffect $ xs P.>-> PC.toOutput output
    pure (seal, fromInputSTM input)
