{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Gimlight.GameStatus.Scene
    ( SceneHandler
    , sceneHandler
    , emptyHandler
    , withoutSpeaker
    , text
    , getBackgroundImagePath
    , getCurrentScene
    , getExploringHandler
    , nextSceneOrFinish
    ) where

import           Control.Lens                  (makeLenses, view, (&), (.~),
                                                (^.))
import           Data.Binary                   (Binary)
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as N
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Gimlight.GameStatus.Exploring (ExploringHandler)
import           Gimlight.Localization         (MultilingualText)

newtype SceneElement =
    WithoutSpeaker MultilingualText
    deriving (Show, Ord, Eq, Generic)

instance Binary SceneElement

data SceneHandler =
    SceneHandler
        { _backgroundImage :: Text
        , _elements        :: NonEmpty SceneElement
        , _afterScene      :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''SceneHandler

instance Binary SceneHandler

emptyHandler :: ExploringHandler -> SceneHandler
emptyHandler =
    SceneHandler "images/game_opening.png" (withoutSpeaker mempty :| [])

withoutSpeaker :: MultilingualText -> SceneElement
withoutSpeaker = WithoutSpeaker

text :: SceneElement -> MultilingualText
text (WithoutSpeaker t) = t

getBackgroundImagePath :: SceneHandler -> Text
getBackgroundImagePath = view backgroundImage

getCurrentScene :: SceneHandler -> SceneElement
getCurrentScene = N.head . view elements

getExploringHandler :: SceneHandler -> ExploringHandler
getExploringHandler = view afterScene

sceneHandler ::
       Text -> NonEmpty SceneElement -> ExploringHandler -> SceneHandler
sceneHandler = SceneHandler

nextSceneOrFinish :: SceneHandler -> Either ExploringHandler SceneHandler
nextSceneOrFinish sh =
    case N.tail (sh ^. elements) of
        [] -> Left $ sh ^. afterScene
        xs -> Right $ sh & elements .~ N.fromList xs
