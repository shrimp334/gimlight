{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Items
    ( herb
    , sampleBook
    , sword
    , hammer
    , woodenArmor
    , goldenArmor
    ) where

import           Gimlight.Localization (MultilingualText, multilingualText)

herb :: MultilingualText
herb = multilingualText "Herb" "薬草"

sampleBook :: MultilingualText
sampleBook = multilingualText "Sample book" "テスト用本"

sword :: MultilingualText
sword = multilingualText "Sword" "剣"

hammer :: MultilingualText
hammer = multilingualText "Hammer" "ハンマー"

woodenArmor :: MultilingualText
woodenArmor = multilingualText "Wooden armor" "木の鎧"

goldenArmor :: MultilingualText
goldenArmor = multilingualText "Golden armor" "金の鎧"
