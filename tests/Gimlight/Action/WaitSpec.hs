module Gimlight.Action.WaitSpec
    ( spec
    ) where

import           Control.Monad.State           (evalState)
import           Control.Monad.Writer          (writer)
import           Data.OpenUnion                (liftUnion)
import           Gimlight.Action.Wait          (waitAction)
import           Gimlight.ActionSpec           (okResult)
import           Gimlight.Actor                (player)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors)
import           Gimlight.Dungeon.Map.TileSpec (mockTileCollection)
import           Gimlight.IndexGenerator       (generator)
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "WaitAction" $
    it "returns a Ok result." $ result `shouldBe` expected
  where
    result = waitAction pos mockTileCollection cm
    expected = writer (okResult cm, [])
    cm = locateItemsActors [(pos, liftUnion p)] (emptyCellMap $ V2 1 1)
    p = evalState player generator
    pos = V2 0 0
