{-# LANGUAGE DeriveGeneric #-}

module Gimlight.GameStatus.Talking
    ( TalkingHandler
    , talkingHandler
    , getTalkingPartner
    , getTalkingPart
    , getExploringHandler
    , proceedTalking
    , selectPrevChoice
    , selectNextChoice
    ) where

import           GHC.Generics                     (Generic)
import           Gimlight.Actor                   (Actor)
import           Gimlight.GameStatus.Exploring    (ExploringHandler, getQuests,
                                                   updateQuests)
import           Gimlight.GameStatus.Talking.Part (TalkingPart,
                                                   proceedNonVisiblePartsIfNecessary)
import qualified Gimlight.GameStatus.Talking.Part as Part

data TalkingHandler =
    TalkingHandler
        { talkingPartner :: Actor
        , part           :: TalkingPart
        , afterTalking   :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

talkingHandler :: Actor -> TalkingPart -> ExploringHandler -> TalkingHandler
talkingHandler a p h = TalkingHandler a updatedPart updatedHandler
  where
    updatedHandler = updateQuests updatedQuests h
    (updatedPart, updatedQuests) =
        case proceedNonVisiblePartsIfNecessary (getQuests h) p of
            (Just x, h') -> (x, h')
            (Nothing, _) -> error "No part to show."

getTalkingPartner :: TalkingHandler -> Actor
getTalkingPartner (TalkingHandler a _ _) = a

getTalkingPart :: TalkingHandler -> TalkingPart
getTalkingPart (TalkingHandler _ p _) = p

getExploringHandler :: TalkingHandler -> ExploringHandler
getExploringHandler (TalkingHandler _ _ h) = h

proceedTalking :: TalkingHandler -> Either ExploringHandler TalkingHandler
proceedTalking (TalkingHandler a p at) =
    case Part.proceedTalking (getQuests at) p of
        (Just next, updatedQuests) ->
            Right $ TalkingHandler a next (updateQuests updatedQuests at)
        (Nothing, updatedQuests) -> Left $ updateQuests updatedQuests at

selectPrevChoice :: TalkingHandler -> TalkingHandler
selectPrevChoice (TalkingHandler a p at) =
    TalkingHandler a (Part.selectPrevChoice p) at

selectNextChoice :: TalkingHandler -> TalkingHandler
selectNextChoice (TalkingHandler a p at) =
    TalkingHandler a (Part.selectNextChoice p) at
