module Pipes.Misc.Time where

import Control.Monad.IO.Class
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import Control.Concurrent

-- | Add a delay after every yield
-- To avoid delaying the first yield use:
--
-- @
-- Pipes.pull () >> delay d
-- @
--
delay :: MonadIO io => Int -> P.Pipe a a io ()
delay i = PP.mapM $ \a -> do
    liftIO $ threadDelay i
    pure a
