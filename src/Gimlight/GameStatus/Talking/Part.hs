{-# LANGUAGE DeriveGeneric #-}

module Gimlight.GameStatus.Talking.Part
    ( TalkingPart(..)
    , SelectionHandler
    , QuestInquiryHandler
    , selectionHandler
    , questInquiryHandler
    , updateQuestHandler
    , getQuestion
    , getChoices
    , getSelectingIndex
    , proceedTalking
    , proceedNonVisiblePartsIfNecessary
    , selectPrevChoice
    , selectNextChoice
    ) where

import           Data.List.NonEmpty    (NonEmpty, toList)
import qualified Data.List.NonEmpty    as NonEmpty
import           GHC.Generics          (Generic)
import           Gimlight.Data.Maybe   (expectJust)
import           Gimlight.Localization (MultilingualText)
import           Gimlight.Quest        (Inquiry, QuestCollection, Updater)
import qualified Gimlight.Quest        as Quest

data UpdateQuestHandler =
    UpdateQuestHandler
        { updater :: Updater
        , after   :: Maybe TalkingPart
        }
    deriving (Show, Ord, Eq, Generic)

data QuestInquiryHandler =
    QuestInquiryHandler
        { inquiry   :: Inquiry
        , trueThen  :: Maybe TalkingPart
        , falseThen :: Maybe TalkingPart
        }
    deriving (Show, Ord, Eq, Generic)

data SelectionHandler =
    SelectionHandler
        { question        :: MultilingualText
        , choicesAndNexts :: NonEmpty (MultilingualText, Maybe TalkingPart)
        , selectingIndex  :: Int
        }
    deriving (Show, Ord, Eq, Generic)

data TalkingPart
    = Selection SelectionHandler
    | QuestInquiry QuestInquiryHandler
    | UpdateQuest UpdateQuestHandler
    deriving (Show, Ord, Eq, Generic)

selectionHandler ::
       MultilingualText
    -> NonEmpty (MultilingualText, Maybe TalkingPart)
    -> SelectionHandler
selectionHandler q c = SelectionHandler q c 0

questInquiryHandler ::
       Inquiry -> Maybe TalkingPart -> Maybe TalkingPart -> QuestInquiryHandler
questInquiryHandler = QuestInquiryHandler

updateQuestHandler :: Updater -> Maybe TalkingPart -> UpdateQuestHandler
updateQuestHandler = UpdateQuestHandler

getQuestion :: SelectionHandler -> MultilingualText
getQuestion = question

getChoices :: SelectionHandler -> [MultilingualText]
getChoices = toList . NonEmpty.map fst . choicesAndNexts

getSelectingIndex :: SelectionHandler -> Maybe Int
getSelectingIndex (SelectionHandler _ cs n) =
    if null cs
        then Nothing
        else Just n

proceedTalking ::
       QuestCollection -> TalkingPart -> (Maybe TalkingPart, QuestCollection)
proceedTalking q (Selection h) = select q h
proceedTalking q p             = proceedNonVisiblePartsIfNecessary q p

proceedNonVisiblePartsIfNecessary ::
       QuestCollection -> TalkingPart -> (Maybe TalkingPart, QuestCollection)
proceedNonVisiblePartsIfNecessary q (QuestInquiry (QuestInquiryHandler i t f))
    | Quest.inquiry i q =
        case t of
            Just x  -> proceedNonVisiblePartsIfNecessary q x
            Nothing -> (Nothing, q)
    | otherwise =
        case f of
            Just x  -> proceedNonVisiblePartsIfNecessary q x
            Nothing -> (Nothing, q)
proceedNonVisiblePartsIfNecessary q (UpdateQuest (UpdateQuestHandler u af)) =
    case af of
        Just x  -> proceedNonVisiblePartsIfNecessary updatedQuests x
        Nothing -> (Nothing, updatedQuests)
  where
    updatedQuests =
        expectJust "Failed to update the quest collection." (Quest.update u q)
proceedNonVisiblePartsIfNecessary q p = (Just p, q)

selectPrevChoice :: TalkingPart -> TalkingPart
selectPrevChoice (Selection (SelectionHandler q cns idx)) =
    Selection $ SelectionHandler q cns newIdx
  where
    newIdx = (idx - 1) `mod` length cns
selectPrevChoice _ = error "We are not selecting anything."

selectNextChoice :: TalkingPart -> TalkingPart
selectNextChoice (Selection (SelectionHandler q cns idx)) =
    Selection $ SelectionHandler q cns newIdx
  where
    newIdx = (idx + 1) `mod` length cns
selectNextChoice _ = error "We are not selecting anything."

select ::
       QuestCollection
    -> SelectionHandler
    -> (Maybe TalkingPart, QuestCollection)
select q (SelectionHandler _ xs n) =
    case next of
        Just (Selection x) -> (Just $ Selection x, q)
        Just x             -> proceedNonVisiblePartsIfNecessary q x
        Nothing            -> (Nothing, q)
  where
    next = snd $ xs NonEmpty.!! n
