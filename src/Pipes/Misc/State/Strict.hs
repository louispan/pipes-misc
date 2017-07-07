{-# LANGUAGE RankNTypes #-}

module Pipes.Misc.State.Strict where

import Control.Lens
import Control.Monad.State.Strict
import qualified Pipes as P
import qualified Pipes.Prelude as PP

-- | Store the output of the pipe into a MonadState.
store :: MonadState s m => Getter a b -> Setter' s b -> P.Pipe a a m r
store v s = forever $ do
  a <- P.await
  s .= view v a
  P.yield a

-- | Yields a view into the stored value.
retrieve :: MonadState s m => Getter s b -> P.Pipe a (b, a) m r
retrieve v = forever $ do
  a <- P.await
  s <- get
  P.yield (view v s, a)

-- | Yields a view into the stored value
retrieve' :: MonadState s m => Getter s b -> P.Pipe () b m r
retrieve' v = forever $ do
  P.await
  s <- get
  P.yield (view v s)

-- | Do something with the state everytime there is a yield.
onState :: (MonadState s m) => (s -> m ()) -> P.Pipe a a m r
onState f = PP.mapM $ \a -> do
    s <- get
    f s
    pure a
