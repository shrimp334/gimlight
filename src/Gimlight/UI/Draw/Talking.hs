{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.Talking
    ( drawTalking
    ) where

import           Control.Lens                     ((^.))
import           Gimlight.Actor                   (Actor, getIdentifier,
                                                   standingImagePath)
import           Gimlight.Actor.Identifier        (toName)
import           Gimlight.GameConfig              (GameConfig)
import           Gimlight.GameStatus.Talking      (TalkingHandler,
                                                   getExploringHandler,
                                                   getTalkingPart,
                                                   getTalkingPartner)
import           Gimlight.GameStatus.Talking.Part (SelectionHandler,
                                                   TalkingPart (Selection),
                                                   getChoices, getQuestion,
                                                   getSelectingIndex)
import           Gimlight.Localization            (getLocalizedText)
import           Gimlight.UI.Draw.Config          (windowWidth)
import           Gimlight.UI.Draw.Dialog          (dialog, heading, normalText)
import qualified Gimlight.UI.Draw.Dialog          as Dialog
import           Gimlight.UI.Draw.Exploring       (drawExploring)
import           Gimlight.UI.Draw.KeyEvent        (withKeyEvents)
import           Gimlight.UI.Draw.Shadow          (shadow)
import           Gimlight.UI.Types                (GameWidgetNode)
import           Monomer                          (CmbAlignCenter (alignCenter),
                                                   CmbAlignMiddle (alignMiddle),
                                                   CmbFitHeight (fitHeight),
                                                   CmbStyleBasic (styleBasic),
                                                   CmbWidth (width), filler,
                                                   hstack, image_, vstack,
                                                   zstack)

drawTalking :: TalkingHandler -> GameConfig -> GameWidgetNode
drawTalking th c =
    withKeyEvents $
    zstack
        [ drawExploring afterGameStatus c
        , shadow
        , talkingWindow c partner (getTalkingPart th)
        ]
  where
    partner = getTalkingPartner th
    afterGameStatus = getExploringHandler th

talkingWindow :: GameConfig -> Actor -> TalkingPart -> GameWidgetNode
talkingWindow c a (Selection h) = hstack [standingPicture, window]
  where
    standingPicture =
        image_ (a ^. standingImagePath) [alignCenter, alignMiddle, fitHeight]
    window = talkingContent c a h
talkingWindow _ _ _ = error "Unable to draw."

talkingContent :: GameConfig -> Actor -> SelectionHandler -> GameWidgetNode
talkingContent c a h =
    dialog $
    vstack (nameWidget c a : filler : question c h : filler : selections) `styleBasic`
    [width dialogWidth]
  where
    selections =
        case getSelectingIndex h of
            Just x ->
                [Dialog.selections x $ getLocalizedText c <$> getChoices h]
            Nothing -> []

nameWidget :: GameConfig -> Actor -> GameWidgetNode
nameWidget c a = heading (getLocalizedText c $ toName $ getIdentifier a)

question :: GameConfig -> SelectionHandler -> GameWidgetNode
question c h = normalText (getLocalizedText c $ getQuestion h)

dialogWidth :: Double
dialogWidth = fromIntegral windowWidth * 0.7
