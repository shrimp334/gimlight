{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Gimlight.Quest
    ( QuestCollection
    , Inquiry(..)
    , Updater(..)
    , questCollection
    , handleWithTurnResult
    , inquiry
    , update
    ) where

import           Control.Lens                (makeLenses, (%~), (&), (.~), (^.))
import           GHC.Generics                (Generic)
import qualified Gimlight.Actor.Identifier   as A
import qualified Gimlight.Dungeon.Identifier as D
import           Gimlight.Quest.KillBats     (KillBats)
import qualified Gimlight.Quest.KillBats     as KillBats

data Inquiry
    = IsKillBatsStarted
    | IsEnoughBatsKilled
    | IsKillBatsCompleted
    deriving (Show, Ord, Eq, Generic)

data Updater
    = StartKillBats
    | CompleteKillBats
    deriving (Show, Ord, Eq, Generic)

newtype QuestCollection =
    QuestCollection
        { _killBats :: KillBats
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''QuestCollection

questCollection :: QuestCollection
questCollection = QuestCollection {_killBats = KillBats.killBats}

handleWithTurnResult ::
       D.Identifier -> [A.Identifier] -> QuestCollection -> QuestCollection
handleWithTurnResult currentDungeon killed qc =
    qc & killBats %~ KillBats.handleWithTurnResult currentDungeon killed

inquiry :: Inquiry -> QuestCollection -> Bool
inquiry IsKillBatsStarted qc   = qc ^. killBats & KillBats.isQuestStarted
inquiry IsEnoughBatsKilled qc  = qc ^. killBats & KillBats.isEnoughBatsKilled
inquiry IsKillBatsCompleted qc = qc ^. killBats & KillBats.isQuestCompleted

update :: Updater -> QuestCollection -> Maybe QuestCollection
update StartKillBats qc =
    (\x -> qc & killBats .~ x) <$> KillBats.startQuest (qc ^. killBats)
update CompleteKillBats qc =
    (\x -> qc & killBats .~ x) <$> KillBats.completeQuest (qc ^. killBats)
