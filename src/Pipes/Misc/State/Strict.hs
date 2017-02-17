{-# LANGUAGE RankNTypes #-}

module Pipes.Misc.State.Strict where

import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Misc.Concurrent as PM
import qualified Pipes.Prelude as PP

-- | Store the output of the pipe into a MonadState.
store :: MonadState s m => Getter a b -> Setter' s b -> P.Pipe a a m r
store v s = forever $ do
  a <- P.await
  s .= view v a
  P.yield a
{-# INLINABLE store #-}

-- | Yields a view into the stored value.
retrieve :: MonadState s m => Getter s b -> P.Pipe a (b, a) m r
retrieve v = forever $ do
  a <- P.await
  s <- get
  P.yield (view v s, a)
{-# INLINABLE retrieve #-}

-- | Yields a view into the stored value
retrieve' :: MonadState s m => Getter s b -> P.Pipe () b m r
retrieve' v = forever $ do
  P.await
  s <- get
  P.yield (view v s)
{-# INLINABLE retrieve' #-}

-- | Do something with the state everytime there is a yield.
onState :: (MonadState s m) => (s -> m ()) -> P.Pipe a a m r
onState f = PP.mapM $ \a -> do
    s <- get
    f s
    pure a
{-# INLINABLE onState #-}

-- | Converts a 'Glazier.Gadget' into a 'Pipes.Pipe'
rsPipe :: (Monad m, MonadTrans t, MonadState s (t m)) => ReaderT a (StateT s m) b -> P.Pipe a b (t m) r
rsPipe m = forever $ do
    a <- P.await
    s <- get
    -- This is the only line that is different between the Strict and Lazy version
    (c, s') <- lift . lift $ runStateT (runReaderT m a) s
    put s'
    P.yield c
{-# INLINABLE rsPipe #-}

-- | Convert a 'Pipes.Concurrent.Input' and a 'Glazier.Gadget' into a stateful 'Pipes.Producer' of commands to interpret.
rsProducer ::
  (MonadState s (t STM), MonadTrans t) =>
  PC.Input a -> ReaderT a (StateT s STM) b -> P.Producer' b (t STM) ()
rsProducer input m = hoist lift (PM.fromInputSTM input) P.>-> rsPipe m
{-# INLINABLE rsProducer #-}
