{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Gimlight.Action.PickUpSpec
    ( spec
    ) where

import           Control.Monad.State           (evalState, execStateT)
import           Control.Monad.Writer          (writer)
import           Data.Either.Combinators       (fromRight')
import           Data.OpenUnion                (Union, liftUnion)
import           Gimlight.Action               (ActionResultWithLog)
import           Gimlight.Action.PickUp        (pickUpAction)
import           Gimlight.ActionSpec           (failedResult, okResult)
import           Gimlight.Actor                (Actor)
import qualified Gimlight.Actor                as A
import           Gimlight.ActorSpec            (addItems)
import           Gimlight.Coord                (Coord)
import           Gimlight.Dungeon.Map.Cell     (CellMap, mapActorAt,
                                                removeItemAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors)
import           Gimlight.Dungeon.Map.TileSpec (mockTileCollection)
import           Gimlight.IndexGenerator       (generator)
import           Gimlight.Inventory            (maxSlot)
import           Gimlight.Item.Defined         (herb)
import           Gimlight.Item.SomeItem        (SomeItem)
import qualified Gimlight.Localization.Texts   as T
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Spec, it, shouldBe)

spec :: Spec
spec = do
    testPickUpSuccess
    testPickUpVoid
    testPickUpWhenInventoryIsFull

testPickUpSuccess :: Spec
testPickUpSuccess =
    it "returns a Ok result if there is an item at the actor's foot, and player's inventory is not full." $
    result cm `shouldBe` expected
  where
    expected = writer (okResult cellMapAfterPickingUp, [T.youGotItem T.herb])
    cellMapAfterPickingUp =
        fromRight' $
        flip execStateT cm $ do
            _ <- removeItemAt playerPos
            mapActorAt mockTileCollection playerPos (addItems [liftUnion herb])
    cm =
        cellMapWith
            [ (playerPos, liftUnion (liftUnion herb :: SomeItem))
            , (playerPos, liftUnion player)
            ]

testPickUpVoid :: Spec
testPickUpVoid =
    it "returns a Failed result if there is no item at the actor's foot." $
    result cm `shouldBe` expected
  where
    expected = writer (failedResult cm, [T.youGotNothing])
    cm = cellMapWith [(playerPos, liftUnion player)]

testPickUpWhenInventoryIsFull :: Spec
testPickUpWhenInventoryIsFull =
    it "returns a Failed result if the actor's inventory is full." $
    result cm `shouldBe` expected
  where
    expected = writer (failedResult cm, [T.bagIsFull])
    cm =
        cellMapWith
            [ (playerPos, liftUnion (liftUnion herb :: SomeItem))
            , (playerPos, liftUnion $ addItems items player)
            ]
    items = replicate maxSlot $ liftUnion herb

result :: CellMap -> ActionResultWithLog
result = pickUpAction playerPos mockTileCollection

cellMapWith :: [(Coord, Union '[ Actor, SomeItem])] -> CellMap
cellMapWith xs = locateItemsActors xs testMap

testMap :: CellMap
testMap = emptyCellMap $ V2 1 1

player :: Actor
player = evalState A.player generator

playerPos :: Coord
playerPos = V2 0 0
