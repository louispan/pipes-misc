module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import qualified Pipes as P
import qualified Pipes.Prelude as PP
import qualified Pipes.Misc as PM

sig1 :: Monad m => P.Producer Int m ()
sig1 = do
  P.yield 1
  P.yield 2
  P.yield 3
  P.yield 4
  P.yield 5
  P.yield 6
  P.yield 7
  P.yield 8
  P.yield 9

sigConsumer :: Show a => P.Consumer a IO ()
sigConsumer = forever $ do
  a <- P.await
  lift $ print a

main :: IO ()
main = do
  putStrLn "\nOriginal"
  P.runEffect $ sig1 P.>-> sigConsumer

  putStrLn "\nBuffered"
  P.runEffect $ sig1 P.>-> PM.buffer 2 [] P.>-> sigConsumer

  putStrLn "\nLocally +10 to second"
  P.runEffect $ sig1
    P.>-> PP.map (\a -> (a, a))
    P.>-> PM.locally (view _2) (set _2) (PP.map (+ 10))
    P.>-> sigConsumer
