module Pipes.Misc.Util where

import Control.Monad
import Control.Arrow
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import qualified Pipes.Shaft as PS
import Pipes.Internal (Proxy(..))

-- | Given a size and a initial tail, create a pipe that
-- will buffer the output of a producer.
-- This pipe is stateful, and will only buffer until the immediate connecting
-- producer is finished.
--
-- @
-- forever $ do
--   a <- await
--   yield a >-> buffer 2 [] -- will only ever result a producer of single 'a : []'.
-- @
--
-- @
-- (forever $ do
--   a <- await
--   yield a
-- ) >-> buffer 2 [] -- will buffer properly and produce '[latest, old]'
-- @
--
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
  P.>-> PS.shafted (first $ PS.Shaft $ PP.map viewf P.>-> p)
  P.>-> PP.map (uncurry modifyf)

-- | Given comparison function and an initial value.
-- yield the result of comparing the value await with the previously awaited value.
compare :: Monad m => (a -> a -> b) -> a -> P.Pipe a b m r
compare f i = do
    a <- P.await
    P.yield (f a i)
    go a
  where
    go a = do
        b <- P.await
        P.yield (f b a)
        go b

-- | Given comparison function
-- yield the result of comparing the value await with the first awaited value.
compare' :: Monad m => (a -> a -> b) -> P.Pipe a b m r
compare' f = do
    i <- P.await
    P.yield (f i i)
    go i
  where
    go i = forever $ do
        a <- P.await
        P.yield (f a i)

-- | constantly yields the given value
always :: Monad m => a -> P.Producer a m r
always = forever . P.yield

-- | Makes the Producer return/pure the last value yielded, or the input value if nothing
-- was yielded
lastOr :: Monad m => a -> P.Producer a m () -> P.Producer a m a
lastOr = go
  where
    go i p =
        case p of
            Request a' fa -> Request a' (go i . fa)
            Respond b fb' -> Respond b (go b . fb')
            M m -> M (m >>= pure . go i)
            Pure () -> Pure i
