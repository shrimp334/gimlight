{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Action.DropSpec
    ( spec
    ) where

import           Control.Monad.State           (evalState, execStateT)
import           Control.Monad.Trans.Writer    (writer)
import           Data.Either.Combinators       (fromRight')
import           Data.OpenUnion                (liftUnion)
import           Gimlight.Action               (ActionResultWithLog)
import           Gimlight.Action.Drop          (dropAction)
import           Gimlight.ActionSpec           (failedResult, okResult)
import           Gimlight.Actor                (player)
import           Gimlight.ActorSpec            (addItems, removeItem)
import           Gimlight.Coord                (Coord)
import           Gimlight.Dungeon.Map.Cell     (CellMap, mapActorAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors,
                                                locateItemsActorsST)
import           Gimlight.Dungeon.Map.TileSpec (mockTileCollection)
import           Gimlight.IndexGenerator       (generator)
import           Gimlight.Item.Defined         (herb)
import           Gimlight.Item.SomeItem        (SomeItem)
import qualified Gimlight.Localization.Texts   as T
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, it, shouldBe)

spec :: Spec
spec = do
    testDropItemSuccessfully
    testItemAlreadyExists

testDropItemSuccessfully :: Spec
testDropItemSuccessfully =
    it "returns a Ok result if there is no item at the player's foot." $
    result testMap `shouldBe` expected
  where
    expected = writer (okResult cellMapAfterDropping, [T.youDropped T.herb])
    cellMapAfterDropping =
        fromRight' $
        flip execStateT testMap $ do
            mapActorAt mockTileCollection playerPos (removeItem 0)
            locateItemsActorsST
                [(playerPos, liftUnion (liftUnion herb :: SomeItem))]

testItemAlreadyExists :: Spec
testItemAlreadyExists =
    it "returns a Failed result if there is already an item at the player's foot." $
    result cm `shouldBe` expected
  where
    expected = writer (failedResult cm, [T.itemExists])
    cm =
        locateItemsActors
            [(playerPos, liftUnion (liftUnion herb :: SomeItem))]
            testMap

result :: CellMap -> ActionResultWithLog
result = dropAction 0 playerPos mockTileCollection

testMap :: CellMap
testMap = locateItemsActors [(playerPos, liftUnion p)] $ emptyCellMap $ V2 1 1
  where
    p = addItems [liftUnion herb] $ evalState player generator

playerPos :: Coord
playerPos = V2 0 0
