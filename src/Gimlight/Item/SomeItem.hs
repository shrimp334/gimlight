{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gimlight.Item.SomeItem
    ( SomeItem
    , getName
    , getIconImagePath
    , isUsableManyTimes
    ) where

import           Data.OpenUnion        (Union, typesExhausted, (@>))
import           Data.Text             (Text)
import           Gimlight.Item         (Item)
import qualified Gimlight.Item         as I
import           Gimlight.Item.Armor   (Armor)
import           Gimlight.Item.Book    (Book)
import           Gimlight.Item.Heal    (Heal)
import           Gimlight.Item.Weapon  (Weapon)
import           Gimlight.Localization (MultilingualText)

type SomeItem = Union '[ Item Heal, Item Book, Item Weapon, Item Armor]

getName :: SomeItem -> MultilingualText
getName =
    (\(x :: Item Heal) -> I.getName x) @> (\(x :: Item Book) -> I.getName x) @>
    (\(x :: Item Weapon) -> I.getName x) @>
    (\(x :: Item Armor) -> I.getName x) @>
    typesExhausted

getIconImagePath :: SomeItem -> Text
getIconImagePath =
    (\(x :: Item Heal) -> I.getIconImagePath x) @>
    (\(x :: Item Book) -> I.getIconImagePath x) @>
    (\(x :: Item Weapon) -> I.getIconImagePath x) @>
    (\(x :: Item Armor) -> I.getIconImagePath x) @>
    typesExhausted

isUsableManyTimes :: SomeItem -> Bool
isUsableManyTimes =
    (\(x :: Item Heal) -> I.isUsableManyTimes x) @>
    (\(x :: Item Book) -> I.isUsableManyTimes x) @>
    (\(x :: Item Weapon) -> I.isUsableManyTimes x) @>
    (\(x :: Item Armor) -> I.isUsableManyTimes x) @>
    typesExhausted
