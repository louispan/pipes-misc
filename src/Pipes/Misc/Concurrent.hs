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
-- Each transaction is `atomically` scoped around each yield,
-- so be careful when `Pipes.Prelude.filter` or similar pipes to remove yields
-- as this results in larger transactions and it may cause BlockIndefinitelyOnSTM exceptions.
-- Intead, use Monoids to yield mempty so that the STM state changes.
fromInputSTM :: PC.Input a -> P.Producer' a STM ()
fromInputSTM as = void $ runMaybeT $ forever $ do
    a <- MaybeT $ lift $ PC.recv as
    lift $ P.yield a
{-# INLINABLE fromInputSTM #-}

-- | Like Pipes.Concurrent.toOutput, but stays in STM.
-- Using @hoist atomically@ to convert to IO monad seems to work.
-- Do not use @unsafeHoist atomically@.
toOutputSTM :: PC.Output a -> P.Consumer' a STM ()
toOutputSTM output = void $ runMaybeT $ forever $ do
    a <- lift P.await
    p <- lift $ lift $ PC.send output a
    guard p
{-# INLINABLE toOutputSTM #-}

-- | Convert PC.Output @a -> STM Bool@ to @a -> MaybeT STM ()@
toOutputMaybeT :: PC.Output a -> a -> MaybeT STM ()
toOutputMaybeT output = (MaybeT . fmap guard) <$> PC.send output
{-# INLINABLE toOutputMaybeT #-}

-- | Converts a Producer in IO monad to a producer in STM monad.
mkProducerSTM :: PC.Buffer a -> P.Producer a IO () -> IO (P.Producer a STM ())
mkProducerSTM b xs = do
    (output, input) <- PC.spawn b
    void . forkIO . void . forever . P.runEffect $ xs P.>-> PC.toOutput output
    pure (fromInputSTM input)
{-# INLINABLE mkProducerSTM #-}

-- | Converts a Producer in IO monad to a producer in STM monad. Also returns the seal.
mkProducerSTM' :: PC.Buffer a -> P.Producer a IO () -> IO (STM (), P.Producer a STM ())
mkProducerSTM' b xs = do
    (output, input, seal) <- PC.spawn' b
    void . forkIO . void . forever . P.runEffect $ xs P.>-> PC.toOutput output
    pure (seal, fromInputSTM input)
{-# INLINABLE mkProducerSTM' #-}
