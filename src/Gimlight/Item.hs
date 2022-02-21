{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Item
    ( Item
    , Effect(..)
    , herb
    , sampleBook
    , getName
    , getIconImagePath
    , getEffect
    , isUsableManyTimes
    ) where

import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Gimlight.Item.Book          (Book)
import           Gimlight.Item.Heal          (HealHandler, healHandler)
import           Gimlight.Localization       (MultilingualText)
import qualified Gimlight.Localization.Texts as T

data Effect
    = Heal HealHandler
    | Book Book
    deriving (Show, Ord, Eq, Generic)

data Item =
    Item
        { name            :: MultilingualText
        , iconImagePath   :: Text
        , effect          :: Effect
        , usableManyTimes :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

item :: MultilingualText -> Text -> Effect -> Bool -> Item
item n ip e u =
    Item {name = n, iconImagePath = ip, effect = e, usableManyTimes = u}

getName :: Item -> MultilingualText
getName Item {name = n} = n

getIconImagePath :: Item -> Text
getIconImagePath Item {iconImagePath = ip} = ip

getEffect :: Item -> Effect
getEffect Item {effect = e} = e

isUsableManyTimes :: Item -> Bool
isUsableManyTimes Item {usableManyTimes = u} = u

herb :: Item
herb = item T.herb "images/herb.png" (Heal $ healHandler 4) False

sampleBook :: Item
sampleBook = item T.sampleBook "images/book.png" (Book T.sampleBookContent) True
