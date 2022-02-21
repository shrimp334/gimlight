{-# LANGUAGE DeriveGeneric #-}

module Gimlight.GameConfig
    ( GameConfig
    , Language(..)
    , initConfig
    , setLocale
    , getLocale
    ) where

import           GHC.Generics (Generic)

data Language
    = English
    | Japanese
    deriving (Eq, Show, Generic)

newtype GameConfig =
    GameConfig
        { language :: Maybe Language
        }
    deriving (Eq, Show, Generic)

initConfig :: GameConfig
initConfig = GameConfig {language = Nothing}

setLocale :: Language -> GameConfig -> GameConfig
setLocale l c = c {language = Just l}

getLocale :: GameConfig -> Maybe Language
getLocale GameConfig {language = l} = l
