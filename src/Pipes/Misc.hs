{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

-- | Miscellaneous utilities for pipes, required by glazier-tutorial
module Pipes.Misc where

import Control.Arrow
import qualified Control.Concurrent.STM as S
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Tuple
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Lift as PL
import qualified Pipes.PipeC as PPC
import qualified Pipes.Prelude as PP
import qualified Data.List.NonEmpty as NE
import Control.Monad.Except
import Control.Applicative
import Pipes.Internal (
    Proxy(..),
    closed
    )
-- | Like Pipes.Concurrent.fromInput, but stays in STM
fromInputSTM :: PC.Input a -> P.Producer' a S.STM ()
fromInputSTM as = void $ runMaybeT $ forever $ do
    a <- MaybeT $ lift $ PC.recv as
    lift $ P.yield a

-- | Like Pipes.Concurrent.toOutput, but stays in STM
toOutputSTM :: PC.Output a -> P.Consumer' a S.STM ()
toOutputSTM output = void $ runMaybeT $ forever $ do
    a <- lift P.await
    p <- lift $ lift $ PC.send output a
    guard p

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

-- | Given a size and a initial tail, create a pipe that
-- will buffer the output of a producer.
-- This pipe is stateful, and will only buffer until the immediate connecting
-- producer is finished.
-- @
-- forever $ do
--   a <- await
--   yield a >-> buffer 2 [] -- will only ever result a producer of single 'a : []'.
-- @
-- @
-- (forever $ do
--   a <- await
--   yield a
-- ) >-> buffer 2 [] -- will buffer properly and produce '[latest, old]'
-- @
buffer :: Monad m => Int -> [a] -> P.Pipe a [a] m r
buffer n as = do
  a <- P.await
  let as' = take n $ a : as
  case forceSpine as' of -- TODO: can we leave this lazy?
    () -> do
      P.yield as'
      buffer n as'
 where
  -- from https://ro-che.info/articles/2015-05-28-force-list
  forceSpine = foldr (const id) ()

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

-- | Run a pipe in a larger stream, using view function and modify function
-- of the larger stream.
locally ::
  Monad m =>
     (s -> a)
  -> (b -> s -> t)
  -> P.Pipe a b m r
  -> P.Pipe s t m r
locally viewf modifyf p =
  PP.map (\s -> (s, s))
  P.>-> PPC.getPipeC (first $ PPC.PipeC $ PP.map viewf P.>-> p)
  P.>-> PP.map (uncurry modifyf)
