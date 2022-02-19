module Gimlight.Action.ConsumeSpec
    ( spec
    ) where

import           Control.Lens                ((%~), (&))
import           Control.Monad.State         (evalStateT, execStateT)
import           Control.Monad.Writer        (writer)
import           Data.Either.Combinators     (fromRight')
import           Gimlight.Action             (ActionResult (ActionResult, killed, newCellMap, status),
                                              ActionStatus (Ok, ReadingStarted))
import           Gimlight.Action.Consume     (consumeAction)
import           Gimlight.Actor              (getIdentifier, inventoryItems)
import           Gimlight.Actor.Identifier   (toName)
import           Gimlight.Dungeon.Map.Cell   (locateActorAt, removeActorAt)
import           Gimlight.Inventory          (removeNthItem)
import           Gimlight.Item               (Effect (Book, Heal), getEffect,
                                              herb, sampleBook)
import           Gimlight.Item.Heal          (getHealAmount)
import qualified Gimlight.Localization.Texts as T
import           Gimlight.SetUp.CellMap      (initCellMap, initTileCollection,
                                              orcWithHerbPosition,
                                              playerPosition)
import           Test.Hspec                  (Spec, it, shouldBe)

spec :: Spec
spec = do
    testStartReadingBook
    testConsumeHerb

testStartReadingBook :: Spec
testStartReadingBook =
    it "returns a ReadingStarted result if an actor uses a book" $
    result `shouldBe` expected
  where
    result = consumeAction 0 playerPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = ReadingStarted $ bookContent $ getEffect sampleBook
            , newCellMap = initCellMap
            , killed = []
            }
    expectedLog = []
    bookContent (Book c) = c
    bookContent _        = error "Not a book."

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
    expectedLog =
        [ T.healed (toName $ getIdentifier orcWithItem) $
          healAmount $ getEffect herb
        ]
    cellMapAfterConsuming =
        fromRight' $
        flip execStateT initCellMap $ do
            a <- removeActorAt orcWithHerbPosition
            locateActorAt
                initTileCollection
                (a & inventoryItems %~ (snd . removeNthItem 0))
                orcWithHerbPosition
    orcWithItem =
        fromRight' $
        flip evalStateT initCellMap $ removeActorAt orcWithHerbPosition
    healAmount (Heal h) = getHealAmount h
    healAmount _        = error "Not a healer."
