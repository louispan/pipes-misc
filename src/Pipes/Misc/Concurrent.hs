{-# LANGUAGE RankNTypes #-}

module Pipes.Misc.Concurrent where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import qualified Data.List.NonEmpty as NE
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as PP

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

-- | Like Pipes.Concurrent.toOutput, but stays in STM.
-- Using @hoist atomically@ to convert to IO monad seems to work.
-- Do not use @unsafeHoist atomically@.
toOutputSTM :: PC.Output a -> P.Consumer' a STM ()
toOutputSTM output = void $ runMaybeT $ forever $ do
    a <- lift P.await
    p <- lift $ lift $ PC.send output a
    guard p

-- | Convert PC.Output @a -> STM Bool@ to @a -> MaybeT STM ()@
toOutputMaybeT :: PC.Output a -> a -> MaybeT STM ()
toOutputMaybeT output = (MaybeT . fmap guard) <$> PC.send output

-- | Converts a Producer in IO monad to a producer in STM monad.
mkProducerSTM :: PC.Buffer a -> P.Producer a IO () -> IO (P.Producer a STM ())
mkProducerSTM b xs = do
    (output, input) <- PC.spawn b
    void . forkIO . void . forever . P.runEffect $ xs P.>-> PC.toOutput output
    pure (fromInputSTM input)

-- | Converts a Producer in IO monad to a producer in STM monad. Also returns the seal.
mkProducerSTM' :: PC.Buffer a -> P.Producer a IO () -> IO (STM (), P.Producer a STM ())
mkProducerSTM' b xs = do
    (output, input, seal) <- PC.spawn' b
    void . forkIO . void . forever . P.runEffect $ xs P.>-> PC.toOutput output
    pure (seal, fromInputSTM input)

-- | Reads as much as possible from an input and return a list of all unblocked values read.
-- Blocks if the first value read is blocked.
batch :: PC.Input a -> PC.Input (NE.NonEmpty a)
batch (PC.Input xs) = PC.Input $ do
    x <- xs
    case x of
        Nothing -> pure Nothing
        Just x' -> do
            xs' <- runExceptT . tryNext $ x' NE.:| []
            case xs' of
                Left ys -> pure (Just ys)
                Right ys -> pure (Just ys)
  where
      tryNext ys = do
          ys' <- ExceptT $ (tryCons ys <$> xs) <|> pure (Left ys)
          tryNext ys'
      tryCons ys x = case x of
          Nothing -> Left ys -- return successful reads so far
          Just x' -> Right $ x' NE.<| ys

-- | Combine a 'Pipes.Concurrent.Input' and a 'a -> t STM b' into a 'Pipes.Producer' of the result b.
-- That is, given a input of messages, and something that executes the messages to produce a result b,
-- combine them to get a Producer of the executed results.
execInput
    :: (MonadTrans t, Monad (t STM))
    => PC.Input a -> (a -> (t STM) b) -> P.Producer' b (t STM) ()
execInput input f = hoist lift (fromInputSTM input) P.>-> PP.mapM f
