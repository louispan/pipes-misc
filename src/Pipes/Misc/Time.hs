-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Pipes.Misc.Time where

import Control.Concurrent
-- import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import qualified System.Clock as C
import qualified Data.Decimal as D

-- | Add a delay after every yield
delay :: MonadIO io => Int -> P.Pipe a a io r
delay i = PP.mapM $ \a -> do
    liftIO $ threadDelay i
    pure a

-- | Add a delay after every yield, except for the first yield
delay' :: MonadIO io => Int -> P.Pipe a a io r
delay' i = do
    a <- P.await
    P.yield a
    delay i

-- | obtain the threadDelay given a fps
fpsDelay :: D.Decimal -> Int
fpsDelay fps = truncate (D.roundTo 0 (D.Decimal 0 1000000 / fps))

-- | Continuously yield the clock time
-- Use with delay to reduce the yield rate.
ticker :: MonadIO io => C.Clock -> P.Producer' C.TimeSpec io r
ticker clock = forever $ do
    t <- liftIO $ C.getTime clock
    P.yield t

-- | Converts a stream of times, into a stream of delta time. The first yield is zero.
diffTime :: Monad m => P.Pipe C.TimeSpec C.TimeSpec m r
diffTime = do
    t <- P.await
    P.yield (C.TimeSpec 0 0)
    go t
  where
    go t = do
        t' <- P.await
        P.yield $ C.diffTimeSpec t' t
        go t'
