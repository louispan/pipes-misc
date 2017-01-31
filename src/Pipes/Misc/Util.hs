module Pipes.Misc.Util where

import Control.Applicative
import Control.Arrow
import Control.Monad.Except
import qualified Data.List.NonEmpty as NE
import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as PP
import qualified Pipes.Shaft as PS

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
  P.>-> PS.runShaft (first $ PS.Shaft $ PP.map viewf P.>-> p)
  P.>-> PP.map (uncurry modifyf)
