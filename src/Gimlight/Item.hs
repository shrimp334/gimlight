{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Item
    ( Item
    , item
    , getName
    , getIconImagePath
    , getEffect
    , isUsableManyTimes
    ) where

import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Gimlight.Localization (MultilingualText)

data Item a =
    Item
        { name            :: MultilingualText
        , iconImagePath   :: Text
        , effect          :: a
        , usableManyTimes :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

item :: MultilingualText -> Text -> a -> Bool -> Item a
item n ip e u =
    Item {name = n, iconImagePath = ip, effect = e, usableManyTimes = u}

getName :: Item a -> MultilingualText
getName Item {name = n} = n

getIconImagePath :: Item a -> Text
getIconImagePath Item {iconImagePath = ip} = ip

getEffect :: Item a -> a
getEffect Item {effect = e} = e

isUsableManyTimes :: Item a -> Bool
isUsableManyTimes Item {usableManyTimes = u} = u
