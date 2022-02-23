module Gimlight.Action.ConsumeSpec
    ( spec
    ) where

import           Control.Lens                ((%~), (&))
import           Control.Monad.State         (execStateT)
import           Control.Monad.Writer        (writer)
import           Data.Either.Combinators     (fromRight')
import           Data.Maybe                  (fromJust)
import           Data.OpenUnion              (liftUnion)
import           Gimlight.Action             (ActionResult (ActionResult, killed, newCellMap, status),
                                              ActionStatus (Ok, ReadingStarted))
import           Gimlight.Action.Consume     (consumeAction)
import           Gimlight.Actor              (equip, inventoryItems)
import           Gimlight.Dungeon.Map.Cell   (locateActorAt, removeActorAt)
import           Gimlight.Inventory          (removeNthItem)
import           Gimlight.Item               (getEffect)
import           Gimlight.Item.Defined       (herb, sampleBook, sword,
                                              woodenArmor)
import           Gimlight.Item.Heal          (getHealAmount)
import qualified Gimlight.Localization.Texts as T
import           Gimlight.SetUp.CellMap      (initCellMap, initTileCollection,
                                              orcWithArmorPosition,
                                              orcWithHerbPosition,
                                              orcWithSwordPosition,
                                              playerPosition)
import           Test.Hspec                  (Spec, it, shouldBe)

spec :: Spec
spec = do
    testStartReadingBook
    testConsumeHerb
    testEquipWeapon
    testEquipArmor

testStartReadingBook :: Spec
testStartReadingBook =
    it "returns a ReadingStarted result if an actor uses a book" $
    result `shouldBe` expected
  where
    result = consumeAction 0 playerPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = ReadingStarted $ getEffect sampleBook
            , newCellMap = initCellMap
            , killed = []
            }
    expectedLog = []

testConsumeHerb :: Spec
testConsumeHerb =
    it "returns a Ok result if an actor uses a herb" $
    result `shouldBe` expected
  where
    result = consumeAction 0 orcWithHerbPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterConsuming, killed = []}
    expectedLog = [T.healed T.orc $ getHealAmount herb]
    cellMapAfterConsuming =
        fromRight' $
        flip execStateT initCellMap $ do
            a <- removeActorAt orcWithHerbPosition
            locateActorAt
                initTileCollection
                (a & inventoryItems %~ (snd . removeNthItem 0))
                orcWithHerbPosition

testEquipWeapon :: Spec
testEquipWeapon =
    it "returns a Ok result if an actor equips a weapon" $
    result `shouldBe` expected
  where
    result = consumeAction 0 orcWithSwordPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterEquipping, killed = []}
    expectedLog = [T.equipped T.orc T.sword]
    cellMapAfterEquipping =
        fromRight' $
        flip execStateT initCellMap $ do
            a <- removeActorAt orcWithSwordPosition
            locateActorAt
                initTileCollection
                (fromJust (equip (liftUnion sword) a) &
                 inventoryItems %~ (snd . removeNthItem 0))
                orcWithSwordPosition

testEquipArmor :: Spec
testEquipArmor =
    it "returns a Ok result if an actor equips a weapon" $
    result `shouldBe` expected
  where
    result = consumeAction 0 orcWithArmorPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterEquipping, killed = []}
    expectedLog = [T.equipped T.orc T.woodenArmor]
    cellMapAfterEquipping =
        fromRight' $
        flip execStateT initCellMap $ do
            a <- removeActorAt orcWithArmorPosition
            locateActorAt
                initTileCollection
                (fromJust (equip (liftUnion woodenArmor) a) &
                 inventoryItems %~ (snd . removeNthItem 0))
                orcWithArmorPosition
