{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Scene
    ( title
    , welcome
    ) where

import           Gimlight.Localization (MultilingualText, multilingualText)

title :: [MultilingualText]
title = fmap (uncurry multilingualText) texts
  where
    texts =
        [ ("Hi, do you see this?", "やあ。これが見えているかい？")
        , ("Alright.", "オーケー。")
        , ( "Well, should I start explaining this world, right?"
          , "そうだな、この世界について説明すればいいのかな。")
        , ( "This world is somewhat extraordinary. The hygiene condition is high, people can drink water safely, the literacy rate is high, potatoes are grown, there are people wearing sailor suits, and for some reason, there are medieval-like scenes. This world is, after all, the \" convenient \" world."
          , "この世界はかなり特殊だ。衛生状態は良いし、水も安全に飲める。識字率も高いし、じゃがいもも育っている。セーラー服を着ている人もいる。けどなぜか中世チックな場所もある。つまるところこの世界は「都合の良い」世界なのさ。")
        , ( "The residents of this world won't notice the facts, though."
          , "この世界の住民はその事実に気づかないだろうけどね。")
        , ( "Ah, by the way, adventures adventurers are treated as so-called handymen. It may bother you, but they may bring some exciting things to you."
          , "そうそう、この世界では冒険者たちはいわゆる便利屋といった感じで扱われている。面倒かもしれないけど、もしかしたら面白いことを持ち掛けられるかもしれないね。")
        , ( "Okay, it's time to jump into the world. I hope you enjoy your life there."
          , "よし、この世界に飛び込む時間だ。楽しんで。")
        ]

welcome :: MultilingualText
welcome =
    multilingualText "Welcome to the world of Gimlight!" "Gimlightの世界へようこそ！"
