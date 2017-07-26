{-# LANGUAGE RankNTypes #-}

-- | You can use the Arrow instance to get different types of ticker time. Eg:
--
-- @
-- diffTimeEvery :: MonadIO io => C.Clock -> Int -> P.Producer' C.TimeSpec io r
-- diffTimeEvery clock micros = always () P.>-> delay micros P.>-> ticker clock P.>-> diffTime
--
-- import Control.Arrow
-- import qualified Control.Category as Cat
-- import qualified Pipes.Shaft as PS
--
-- diffAndTickEvery :: MonadIO io => C.Clock -> Int -> P.Producer' (C.TimeSpec, C.TimeSpec) io r
-- diffAndTickEvery clock micros = always () P.>-> delay micros P.>-> ticker clock P.>->
--    PS.shafted (PS.Shaft diffTime &&& Cat.id)
--
-- @
--
module Pipes.Misc.Time where

import Control.Concurrent
import Control.Monad.IO.Class
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import qualified System.Clock as C
import qualified Data.Decimal as D
import qualified Pipes.Misc.Util as PM

-- | Add a delay after every await
delay :: MonadIO io => Int -> P.Pipe a a io r
delay i = PP.mapM $ \a -> do
    liftIO $ threadDelay i
    pure a

-- | After the first await, add a delay after every subsequent await.
delay' :: MonadIO io => Int -> P.Pipe a a io r
delay' i = do
    a <- P.await
    P.yield a
    delay i

-- | obtain the threadDelay given a fps
fps :: D.Decimal -> Int
fps x = truncate (D.roundTo 0 (D.Decimal 0 1000000 / x))

-- | Continuously yield the clock time
-- Use with delay to reduce the yield rate. Eg:
--
-- @
-- tickEvery :: MonadIO io => C.Clock -> Int -> P.Producer' C.TimeSpec io r
-- tickEvery clock micros = always () P.>-> delay micros P.>-> ticker clock
-- @
--
ticker :: MonadIO io => C.Clock -> P.Pipe () C.TimeSpec io r
ticker clock = P.for P.cat $ \() -> do
    t <- liftIO $ C.getTime clock
    P.yield t

-- | Converts a stream of times, into a stream of delta time. The first yield is zero.
diffTime :: Monad m => P.Pipe C.TimeSpec C.TimeSpec m r
diffTime = PM.compare C.diffTimeSpec (C.TimeSpec 0 0)

-- | Converts a stream of epoch times into a stream of epoch time, where zero is the first yielded time.
resetEpoch :: Monad m => P.Pipe C.TimeSpec C.TimeSpec m r
resetEpoch = PM.compare' C.diffTimeSpec
