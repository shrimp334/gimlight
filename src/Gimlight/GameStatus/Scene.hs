{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Gimlight.GameStatus.Scene
    ( SceneHandler
    , sceneHandler
    , withoutSpeaker
    , text
    , getBackgroundImagePath
    , getCurrentScene
    , nextSceneOrFinish
    ) where

import           Control.Lens                  (makeLenses, view, (&), (.~),
                                                (^.))
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as N
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Gimlight.GameStatus.Exploring (ExploringHandler)
import           Gimlight.Localization         (MultilingualText)

newtype SceneElement =
    WithoutSpeaker MultilingualText
    deriving (Show, Ord, Eq, Generic)

data SceneHandler =
    SceneHandler
        { _backgroundImage :: Text
        , _elements        :: NonEmpty SceneElement
        , _afterScene      :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''SceneHandler

withoutSpeaker :: MultilingualText -> SceneElement
withoutSpeaker = WithoutSpeaker

text :: SceneElement -> MultilingualText
text (WithoutSpeaker t) = t

getBackgroundImagePath :: SceneHandler -> Text
getBackgroundImagePath = view backgroundImage

getCurrentScene :: SceneHandler -> SceneElement
getCurrentScene = N.head . view elements

sceneHandler ::
       Text -> NonEmpty SceneElement -> ExploringHandler -> SceneHandler
sceneHandler = SceneHandler

nextSceneOrFinish :: SceneHandler -> Either ExploringHandler SceneHandler
nextSceneOrFinish sh =
    case N.tail (sh ^. elements) of
        [] -> Left $ sh ^. afterScene
        xs -> Right $ sh & elements .~ N.fromList xs
