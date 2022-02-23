{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Actions
    ( youCannotMoveThere
    , youGotItem
    , youGotNothing
    , itemExists
    , bagIsFull
    , whatToUse
    , whatToDrop
    , youDropped
    , healed
    , damagedMessage
    , deathMessage
    , attackMessage
    , noDamageMessage
    , equipped
    ) where

import           Gimlight.Localization (MultilingualText, multilingualText)
import           TextShow              (TextShow (showt))

youCannotMoveThere :: MultilingualText
youCannotMoveThere = multilingualText "That way is blocked." "その方向には進めない．"

youGotItem :: MultilingualText -> MultilingualText
youGotItem item =
    multilingualText "You got " "" <> item <> multilingualText "." "を入手した．"

youGotNothing :: MultilingualText
youGotNothing = multilingualText "You got nothing." "あなたは無を入手した．"

bagIsFull :: MultilingualText
bagIsFull = multilingualText "Your bag is full." "バッグは一杯だ．"

healed :: MultilingualText -> Int -> MultilingualText
healed who amount =
    who <>
    multilingualText " healed " "は" <>
    amount'' <> multilingualText " point." "ポイント回復した．"
  where
    amount'' = multilingualText amount' amount'
    amount' = showt amount

whatToUse :: MultilingualText
whatToUse = multilingualText "What do you consume" "何を使う？"

whatToDrop :: MultilingualText
whatToDrop = multilingualText "What do you drop?" "何を置く？"

itemExists :: MultilingualText
itemExists =
    multilingualText "There is already an item at your foot." "足元には既にアイテムがある。"

youDropped :: MultilingualText -> MultilingualText
youDropped name =
    multilingualText "You dropped " "あなたは" <>
    name <> multilingualText "." "を床に置いた．"

damagedMessage ::
       Int -> MultilingualText -> MultilingualText -> MultilingualText
damagedMessage damage from to =
    attackMessage from to <>
    multilingualText
        (" for " <> showt damage <> " hit points.")
        ("して" <> showt damage <> "ポイントのダメージを与えた．")

deathMessage :: MultilingualText -> MultilingualText
deathMessage who = who <> multilingualText " is dead!" "は死んだ．"

attackMessage :: MultilingualText -> MultilingualText -> MultilingualText
attackMessage from to =
    mconcat
        [from, multilingualText " attacks " "は", to, multilingualText "" "に攻撃"]

noDamageMessage :: MultilingualText -> MultilingualText -> MultilingualText
noDamageMessage from to =
    attackMessage from to `mappend`
    multilingualText " but does not damage." "したがダメージを受けなかった．"

equipped :: MultilingualText -> MultilingualText -> MultilingualText
equipped who what =
    who <>
    multilingualText " equipped " "は" <> what <> multilingualText "." "を装備した。"
