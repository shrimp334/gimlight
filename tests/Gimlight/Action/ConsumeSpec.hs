{-# LANGUAGE DataKinds #-}

module Gimlight.Action.ConsumeSpec
    ( spec
    ) where

import           Control.Monad.State           (evalState, execStateT)
import           Control.Monad.Writer          (writer)
import           Data.Either.Combinators       (fromRight')
import           Data.Maybe                    (fromJust)
import           Data.OpenUnion                (Union, liftUnion, reUnion)
import           Gimlight.Action               (ActionResultWithLog)
import           Gimlight.Action.Consume       (consumeAction)
import           Gimlight.ActionSpec           (okResult, readingResult)
import           Gimlight.Actor                (Actor, equip)
import qualified Gimlight.Actor                as A
import           Gimlight.ActorSpec            (addItems, removeItem)
import           Gimlight.Coord                (Coord)
import           Gimlight.Dungeon.Map.Cell     (CellMap, mapActorAt)
import           Gimlight.Dungeon.Map.CellSpec (emptyCellMap, locateItemsActors)
import           Gimlight.Dungeon.Map.TileSpec (mockTileCollection)
import           Gimlight.IndexGenerator       (generator)
import           Gimlight.Item                 (Item)
import           Gimlight.Item.Armor           (Armor)
import           Gimlight.Item.Defined         (herb, sampleBook, sword,
                                                woodenArmor)
import           Gimlight.Item.Heal            (getHealAmount)
import           Gimlight.Item.SomeItem        (SomeItem, getName)
import           Gimlight.Item.Weapon          (Weapon)
import qualified Gimlight.Localization.Texts   as T
import           Linear                        (V2 (V2))
import           Test.Hspec                    (Expectation, Spec, it, shouldBe)

spec :: Spec
spec = do
    testStartReadingBook
    testConsumeHerb
    testEquipWeapon
    testEquipArmor

testStartReadingBook :: Spec
testStartReadingBook =
    it "returns a ReadingStarted result if an actor uses a book" $
    result cm `shouldBe` expected
  where
    expected = writer (readingResult T.sampleBookContent cm, [])
    cm = testMap $ liftUnion sampleBook

testConsumeHerb :: Spec
testConsumeHerb =
    it "returns a Ok result if an actor uses a herb" $
    result cm `shouldBe` expected
  where
    expected =
        writer (okResult cmAfter, [T.healed T.player $ getHealAmount herb])
    cmAfter = afterUsing (removeItem 0) cm
    cm = testMap $ liftUnion herb

testEquipWeapon :: Spec
testEquipWeapon =
    it "returns a Ok result if an actor equips a weapon" $
    testEquip $ liftUnion sword

testEquipArmor :: Spec
testEquipArmor =
    it "returns a Ok result if an actor equips a weapon" $
    testEquip $ liftUnion woodenArmor

testEquip :: Union '[ Item Weapon, Item Armor] -> Expectation
testEquip x = result cm `shouldBe` expected
  where
    expected = writer (okResult cmAfter, [T.equipped T.player (getName item)])
    cmAfter = afterUsing (removeItem 0 . fromJust . equip x) cm
    cm = testMap item
    item = reUnion x

afterUsing :: (Actor -> Actor) -> CellMap -> CellMap
afterUsing f cm =
    fromRight' $ flip execStateT cm $ mapActorAt mockTileCollection playerPos f

result :: CellMap -> ActionResultWithLog
result = consumeAction 0 playerPos mockTileCollection

testMap :: SomeItem -> CellMap
testMap x =
    locateItemsActors [(playerPos, liftUnion $ player x)] $
    emptyCellMap $ V2 1 1

player :: SomeItem -> Actor
player x = addItems [x] $ evalState A.player generator

playerPos :: Coord
playerPos = V2 0 0
