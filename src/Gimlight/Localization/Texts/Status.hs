{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Status
    ( level
    , experience
    , attack
    , defence
    , weapon
    , armor
    , levelUp
    ) where

import           Gimlight.Localization (MultilingualText, multilingualText)
import           TextShow              (TextShow (showt))

level :: MultilingualText
level = multilingualText "Level" "レベル"

experience :: MultilingualText
experience = multilingualText "Experience" "経験値"

attack :: MultilingualText
attack = multilingualText "Attack" "攻撃"

defence :: MultilingualText
defence = multilingualText "Defence" "防御"

weapon :: MultilingualText
weapon = multilingualText "Weapon" "武具"

armor :: MultilingualText
armor = multilingualText "Armor" "防具"

levelUp :: MultilingualText -> Int -> MultilingualText
levelUp who n =
    who <>
    multilingualText " reached to level " "はレベル" <>
    multilingualText (showt n) (showt n) <> multilingualText "." "になった．"
