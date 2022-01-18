{-# LANGUAGE OverloadedStrings #-}

module Actor.Friendly.Electria
    ( electria
    ) where

import           Actor                   (Actor)
import           Actor.Friendly          (friendly)
import           Actor.Identifier        (Identifier (Electria))
import           Actor.Status            (status)
import           Actor.Status.Hp         (hp)
import           Control.Monad.State     (State)
import           Data.List.NonEmpty      (fromList)
import           GameStatus.Talking.Part (TalkingPart (QuestInquiry, Selection, UpdateQuest),
                                          questInquiryHandler, selectionHandler,
                                          updateQuestHandler)
import           IndexGenerator          (IndexGenerator)
import qualified Localization.Texts      as T
import           Quest                   (Inquiry (IsEnoughBatsKilled, IsKillBatsCompleted, IsKillBatsStarted),
                                          Updater (CompleteKillBats, StartKillBats))

electria :: State IndexGenerator Actor
electria =
    friendly
        Electria
        st
        talking
        "images/electria.png"
        "images/sample_standing_picture.png"
  where
    st = status (hp 1) 1 1

talking :: TalkingPart
talking = isQuestCompleted
  where
    isQuestCompleted =
        QuestInquiry $
        questInquiryHandler
            IsKillBatsCompleted
            (Just afterCompleted)
            (Just isQuestStarted)
    isQuestStarted =
        QuestInquiry $
        questInquiryHandler
            IsKillBatsStarted
            (Just isEnoughBatsKilled)
            (Just beforeStarted)
    afterCompleted =
        Selection $
        selectionHandler T.electriaAfterCompletion $ fromList [(T.yes, Nothing)]
    isEnoughBatsKilled =
        QuestInquiry $
        questInquiryHandler
            IsEnoughBatsKilled
            (Just youDidIt)
            (Just questIsOnGoing)
    youDidIt =
        UpdateQuest $
        updateQuestHandler CompleteKillBats $
        Just $
        Selection $
        selectionHandler T.electriaCompleted $ fromList [(T.yes, Nothing)]
    questIsOnGoing =
        Selection $
        selectionHandler T.electriaNotCompleted $ fromList [(T.yes, Nothing)]
    beforeStarted =
        Selection $
        selectionHandler T.electriaBeforeQuest $
        fromList [(T.yes, Just startQuest), (T.no, Just rejected)]
    startQuest =
        UpdateQuest $
        updateQuestHandler StartKillBats $
        Just $
        Selection $
        selectionHandler T.electriaAccept $ fromList [(T.yes, Nothing)]
    rejected =
        Selection $
        selectionHandler T.electriaReject $ fromList [(T.yes, Nothing)]
