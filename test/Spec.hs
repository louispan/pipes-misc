module Main where

-- import Control.Concurrent
-- import Control.Concurrent.STM
import Control.Lens
-- import Control.Monad.Morph
import Data.Foldable
import qualified Pipes as P
-- import qualified Pipes.Concurrent as PC
import qualified Pipes.Misc as PM
import qualified Pipes.Prelude as PP
import Test.Hspec

data1 :: [Int]
data1 = [1, 2..9]

sig1 :: Monad m => P.Producer Int m ()
sig1 = traverse_ P.yield data1

main :: IO ()
main = do
    hspec $ do
        describe "Misc" $ do
            it "Buffered" $ do
                let xs = PP.toList (sig1 P.>-> PM.buffer 2 [])
                xs `shouldBe` (pure $ head data1) : ((\(a, b) -> [a, b]) <$> zip (tail data1) data1)

            it "Locally +10 to second" $ do
                let xs = PP.toList
                        (sig1
                        P.>-> PP.map (\a -> (a, a))
                        P.>-> PM.locally (view _2) (set _2) (PP.map (+ 10)))
                xs `shouldBe` zip data1 ((+ 10) <$> data1)

            -- -- hspec 2.5 doesn't work well with blocked threads, so comment this test out
            -- -- jcristovao/enclosed-exceptions#12
            -- it "Filtering STM Producer blocks" $ do
            --     sig <- PM.mkProducerSTM (PC.bounded 1) sig1
            --     PP.toListM (hoist atomically (sig P.>-> PP.filter even)) `shouldThrow` anyException
