{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Gimlight.GameStatus.Exploring
    ( ExploringHandler
    , exploringHandler
    , getTileCollection
    , ascendStairsAtPlayerPosition
    , descendStairsAtPlayerPosition
    , exitDungeon
    , doPlayerAction
    , processAfterPlayerTurn
    , updateQuests
    , getQuests
    , getPlayerActor
    , getPlayerPosition
    , getCurrentDungeon
    , getMessageLog
    ) where

import           Control.Lens                           (makeLenses, (%%~),
                                                         (%~), (&), (.~), (^.))
import           Control.Monad                          ((>=>))
import           Control.Monad.Trans.Writer             (runWriter)
import           GHC.Generics                           (Generic)
import           Gimlight.Action                        (Action, ActionStatus)
import           Gimlight.Actor                         (Actor, getIdentifier)
import           Gimlight.Coord                         (Coord)
import           Gimlight.Dungeon                       (Dungeon, cellMap)
import qualified Gimlight.Dungeon                       as D
import           Gimlight.Dungeon.Map.Cell              (playerActor,
                                                         updateExploredMap,
                                                         updatePlayerFov)
import           Gimlight.Dungeon.Map.Tile              (TileCollection)
import           Gimlight.GameStatus.Exploring.Dungeons (Dungeons)
import qualified Gimlight.GameStatus.Exploring.Dungeons as DS
import           Gimlight.Log                           (MessageLog)
import qualified Gimlight.Log                           as L
import           Gimlight.Quest                         (QuestCollection,
                                                         handleWithTurnResult)
import           Gimlight.TreeZipper                    (TreeZipper, getFocused,
                                                         modify)

data ExploringHandler =
    ExploringHandler
        { _dungeons       :: Dungeons
        , _messageLog     :: MessageLog
        , _quests         :: QuestCollection
        , _tileCollection :: TileCollection
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''ExploringHandler

exploringHandler ::
       TreeZipper Dungeon
    -> MessageLog
    -> QuestCollection
    -> TileCollection
    -> ExploringHandler
exploringHandler = ExploringHandler

getTileCollection :: ExploringHandler -> TileCollection
getTileCollection eh = eh ^. tileCollection

ascendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
ascendStairsAtPlayerPosition eh =
    eh & dungeons %%~ DS.ascendStairsAtPlayerPosition (eh ^. tileCollection)

descendStairsAtPlayerPosition :: ExploringHandler -> Maybe ExploringHandler
descendStairsAtPlayerPosition eh =
    eh & dungeons %%~ DS.descendStairsAtPlayerPosition (eh ^. tileCollection)

exitDungeon :: ExploringHandler -> Maybe ExploringHandler
exitDungeon eh =
    (\x -> eh & dungeons .~ x) <$>
    DS.exitDungeon (eh ^. tileCollection) (eh ^. dungeons)

doPlayerAction :: Action -> ExploringHandler -> (ActionStatus, ExploringHandler)
doPlayerAction action eh = (status, newHandler)
  where
    ((status, dungeonsAfterAction, killed), newLog) =
        runWriter $
        DS.doPlayerAction action (eh ^. tileCollection) (eh ^. dungeons)
    newHandler =
        eh & messageLog %~ L.addMessages newLog &
        dungeons .~ dungeonsAfterAction &
        quests %~
        handleWithTurnResult
            (D.getIdentifier (getFocused dungeonsAfterAction))
            (map getIdentifier killed)

processAfterPlayerTurn :: ExploringHandler -> Maybe ExploringHandler
processAfterPlayerTurn eh =
    (\x ->
         handlerAfterNpcTurns & dungeons %~ modify (const x) &
         quests %~ updateQuestsForResult (D.getIdentifier x)) <$>
    newCurrentDungeon
  where
    updateQuestsForResult d = handleWithTurnResult d $ map getIdentifier killed
    newCurrentDungeon =
        getFocused (handlerAfterNpcTurns ^. dungeons) &
        cellMap %%~
        (updatePlayerFov (eh ^. tileCollection) >=> (Just . updateExploredMap))
    (handlerAfterNpcTurns, killed) = handleNpcTurns eh

handleNpcTurns :: ExploringHandler -> (ExploringHandler, [Actor])
handleNpcTurns eh = (newHandler, killed)
  where
    newHandler =
        eh & dungeons .~ dungeonsAfterNpcTurns &
        messageLog %~ L.addMessages newLog
    ((dungeonsAfterNpcTurns, killed), newLog) =
        runWriter $ DS.handleNpcTurns (eh ^. tileCollection) (eh ^. dungeons)

updateQuests :: QuestCollection -> ExploringHandler -> ExploringHandler
updateQuests q e = e & quests .~ q

getQuests :: ExploringHandler -> QuestCollection
getQuests e = e ^. quests

getPlayerActor :: ExploringHandler -> Maybe Actor
getPlayerActor = fmap snd . playerActor . (^. cellMap) . getCurrentDungeon

getPlayerPosition :: ExploringHandler -> Maybe Coord
getPlayerPosition = fmap fst . playerActor . (^. cellMap) . getCurrentDungeon

getCurrentDungeon :: ExploringHandler -> Dungeon
getCurrentDungeon eh = getFocused $ eh ^. dungeons

getMessageLog :: ExploringHandler -> MessageLog
getMessageLog eh = eh ^. messageLog
