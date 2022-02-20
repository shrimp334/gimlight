module Gimlight.UI.Types
    ( AppEvent(..)
    , GameWidgetEnv
    , GameWidgetNode
    , GameEventResponse
    ) where

import           Data.Text          (Text)
import           Gimlight.GameModel (GameModel)
import           Monomer            (AppEventResponse, WidgetEnv, WidgetNode)

data AppEvent
    = AppInit
    | AppKeyboardInput Text
    | NewGameLoaded GameModel
    | ShowNextScene
    deriving (Eq)

type GameWidgetEnv = WidgetEnv GameModel AppEvent

type GameWidgetNode = WidgetNode GameModel AppEvent

type GameEventResponse = AppEventResponse GameModel AppEvent
