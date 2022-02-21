{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Quest.KillBats
    ( KillBats
    , killBats
    , startQuest
    , completeQuest
    , isQuestStarted
    , isQuestCompleted
    , isEnoughBatsKilled
    , handleWithTurnResult
    ) where

import           GHC.Generics                (Generic)
import qualified Gimlight.Actor.Identifier   as A
import qualified Gimlight.Dungeon.Identifier as D

data KillBats
    = NotStarted
    | OnGoing
          { getRemaining :: Int
          }
    | Completed
    deriving (Show, Ord, Eq, Generic)

killBats :: KillBats
killBats = NotStarted

startQuest :: KillBats -> Maybe KillBats
startQuest NotStarted = Just $ OnGoing 0
startQuest _          = Nothing

completeQuest :: KillBats -> Maybe KillBats
completeQuest (OnGoing n)
    | n >= quota = Just Completed
    | otherwise = Nothing
completeQuest _ = Nothing

isQuestStarted :: KillBats -> Bool
isQuestStarted (OnGoing _) = True
isQuestStarted _           = False

isQuestCompleted :: KillBats -> Bool
isQuestCompleted Completed = True
isQuestCompleted _         = False

isEnoughBatsKilled :: KillBats -> Bool
isEnoughBatsKilled (OnGoing n) = n >= quota
isEnoughBatsKilled _           = undefined

handleWithTurnResult :: D.Identifier -> [A.Identifier] -> KillBats -> KillBats
handleWithTurnResult D.BatsCave killed (OnGoing n) = OnGoing $ n + length killed
handleWithTurnResult _ _ k                         = k

quota :: Int
quota = 3
