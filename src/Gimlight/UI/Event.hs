{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Event
    ( handleEvent
    ) where

import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import           Gimlight.GameConfig               (Language (English, Japanese),
                                                    setLocale)
import           Gimlight.GameModel                (GameModel (GameModel, config, status))
import           Gimlight.GameStatus               (GameStatus (Exploring, GameOver, ReadingBook, Scene, SelectingItem, SelectingLocale, Talking, Title),
                                                    newGameStatus)
import           Gimlight.GameStatus.Exploring     (ascendStairsAtPlayerPosition,
                                                    descendStairsAtPlayerPosition,
                                                    processAfterPlayerTurn)
import           Gimlight.GameStatus.ReadingBook   (finishReading)
import           Gimlight.GameStatus.Scene         (nextSceneOrFinish)
import           Gimlight.GameStatus.SelectingItem (Reason (Drop, Use),
                                                    getExploringHandler,
                                                    selectNextItem,
                                                    selectPrevItem)
import           Gimlight.GameStatus.Talking       (proceedTalking,
                                                    selectNextChoice,
                                                    selectPrevChoice)
import           Gimlight.Player                   (handlePlayerAfterSelecting,
                                                    handlePlayerMoving,
                                                    handlePlayerPickingUp,
                                                    handlePlayerSelectingItem)
import           Gimlight.UI.Types                 (AppEvent (AppInit, AppKeyboardInput, NewGameLoaded, ShowNextScene),
                                                    GameEventResponse,
                                                    GameWidgetEnv,
                                                    GameWidgetNode)
import           Linear.V2                         (V2 (V2))
import           Monomer                           (AnimationMsg (AnimationStart, AnimationStop),
                                                    EventResponse (Message, Model, Task),
                                                    WidgetKey (WidgetKey),
                                                    exitApplication)

handleEvent ::
       GameWidgetEnv
    -> GameWidgetNode
    -> GameModel
    -> AppEvent
    -> [GameEventResponse]
handleEvent _ _ gameStatus evt =
    case evt of
        AppInit            -> []
        AppKeyboardInput k -> handleKeyInput gameStatus k
        NewGameLoaded gm   -> [Model gm]
        ShowNextScene      -> nextStatus gameStatus
  where
    nextStatus g@GameModel {status = Scene sh} =
        case nextSceneOrFinish sh of
            Right r ->
                [ Model g {status = Scene r}
                , Message (WidgetKey "sceneFadeIn") AnimationStart
                ]
            Left l -> [Model g {status = Exploring l}]
    nextStatus _ = error "No handling scene."

handleKeyInput :: GameModel -> Text -> [GameEventResponse]
handleKeyInput e@GameModel {status = s} k =
    case s of
        Exploring _     -> handleKeyInputDuringExploring e k
        Talking _       -> handleKeyInputDuringTalking e k
        Scene _         -> handleKeyInputDuringScene e k
        SelectingItem _ -> handleKeyInputDuringSelectingItem e k
        ReadingBook _   -> handleKeyInputDuringReadingBook e k
        Title           -> handleKeyInputDuringTitle e k
        SelectingLocale -> handleKeyInputDuringSelectingLanguage e k
        GameOver        -> []

handleKeyInputDuringExploring :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringExploring e@GameModel {status = Exploring eh} k
    | k == "Right" = [Model $ e {status = handlePlayerMoving (V2 1 0) eh}]
    | k == "Left" = [Model $ e {status = handlePlayerMoving (V2 (-1) 0) eh}]
    | k == "Up" = [Model $ e {status = handlePlayerMoving (V2 0 (-1)) eh}]
    | k == "Down" = [Model $ e {status = handlePlayerMoving (V2 0 1) eh}]
    | k == "g" = [Model e {status = handlePlayerPickingUp eh}]
    | k == "u" = [Model e {status = handlePlayerSelectingItem Use eh}]
    | k == "d" = [Model e {status = handlePlayerSelectingItem Drop eh}]
    | k == "Shift-." =
        [ Model
              e
                  { status =
                        Exploring $
                        fromMaybe eh $ descendStairsAtPlayerPosition eh
                  }
        ]
    | k == "Shift-," =
        [ Model
              e
                  { status =
                        Exploring $
                        fromMaybe eh $ ascendStairsAtPlayerPosition eh
                  }
        ]
    | otherwise = []
handleKeyInputDuringExploring _ _ = error "We are not exploring."

handleKeyInputDuringTalking :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringTalking e@GameModel {status = Talking h} k
    | k == "Enter" = [Model $ e {status = nextStatus}]
    | k == "Up" = [Model $ e {status = Talking $ selectPrevChoice h}]
    | k == "Down" = [Model $ e {status = Talking $ selectNextChoice h}]
    | otherwise = []
  where
    nextStatus =
        case proceedTalking h of
            Right h' -> Talking h'
            Left eh  -> maybe GameOver Exploring (processAfterPlayerTurn eh)
handleKeyInputDuringTalking _ _ = error "We are not talking."

handleKeyInputDuringScene :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringScene GameModel {status = Scene _} k
    | k == "Enter" =
        [ Message (WidgetKey "sceneFadeIn") AnimationStop
        , Message (WidgetKey "sceneFadeOut") AnimationStart
        ]
    | otherwise = []
handleKeyInputDuringScene _ _ = error "We are not handling a scene."

handleKeyInputDuringSelectingItem :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringSelectingItem e@GameModel {status = SelectingItem sh} k
    | k == "Up" = [Model $ e {status = SelectingItem $ selectPrevItem sh}]
    | k == "Down" = [Model $ e {status = SelectingItem $ selectNextItem sh}]
    | k == "Enter" = [Model $ e {status = handlePlayerAfterSelecting sh}]
    | k == "Esc" = [Model $ e {status = Exploring $ getExploringHandler sh}]
    | otherwise = []
handleKeyInputDuringSelectingItem _ _ = error "We are not selecting an item"

handleKeyInputDuringReadingBook :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringReadingBook e@GameModel {status = ReadingBook h} k
    | k == "Enter" =
        [Model $ e {status = maybe GameOver Exploring $ finishReading h}]
    | otherwise = []
handleKeyInputDuringReadingBook _ _ = error "We are not reading a book."

handleKeyInputDuringTitle :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringTitle g k
    | k == "n" = [Task $ NewGameLoaded <$> startNewGame g]
    | k == "q" = [exitApplication]
    | otherwise = []
  where
    startNewGame GameModel {config = c} = do
        st <- newGameStatus
        return g {status = st, config = c}

handleKeyInputDuringSelectingLanguage ::
       GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringSelectingLanguage g@GameModel {config = c} k
    | k == "e" = [Model $ updateConfig English]
    | k == "j" = [Model $ updateConfig Japanese]
    | otherwise = []
  where
    updateConfig l = g {status = Title, config = setLocale l c}
