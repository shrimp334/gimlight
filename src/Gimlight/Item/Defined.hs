{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Item.Defined
    ( herb
    , sampleBook
    , sword
    , hammer
    , woodenArmor
    , goldenArmor
    ) where

import           Gimlight.Item               (Item, item)
import           Gimlight.Item.Armor         (Armor, armor)
import           Gimlight.Item.Book          (Book)
import           Gimlight.Item.Heal          (Heal, heal)
import           Gimlight.Item.Weapon        (Weapon, weapon)
import qualified Gimlight.Localization.Texts as T

herb :: Item Heal
herb = item T.herb "images/items/herb.png" (heal 4) False

sampleBook :: Item Book
sampleBook = item T.sampleBook "images/items/book.png" T.sampleBookContent True

-- Equipment is marked as not usable because it must be removed from the inventory once a player equips it.
sword :: Item Weapon
sword = item T.sword "images/items/sword.png" (weapon 4) False

hammer :: Item Weapon
hammer = item T.hammer "images/items/hammer.png" (weapon 8) False

woodenArmor :: Item Armor
woodenArmor = item T.woodenArmor "images/items/wood.png" (armor 4) False

goldenArmor :: Item Armor
goldenArmor = item T.goldenArmor "images/items/golden_armor.png" (armor 8) False
