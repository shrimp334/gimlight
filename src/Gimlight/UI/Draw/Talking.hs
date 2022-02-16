{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.Talking
    ( drawTalking
    ) where

import           Control.Lens                     (Ixed (ix), (%~), (&), (.~),
                                                   (^.))
import           Gimlight.Actor                   (Actor, getIdentifier,
                                                   standingImagePath)
import           Gimlight.Actor.Identifier        (toName)
import           Gimlight.GameConfig              (GameConfig)
import           Gimlight.GameStatus.Talking      (TalkingHandler,
                                                   getTalkingPart,
                                                   getTalkingPartner)
import           Gimlight.GameStatus.Talking.Part (SelectionHandler,
                                                   TalkingPart (Selection),
                                                   getChoices, getQuestion,
                                                   getSelectingIndex)
import           Gimlight.Localization            (getLocalizedText)
import           Gimlight.UI.Draw.Config          (windowHeight, windowWidth)
import           Gimlight.UI.Draw.Fonts           (bold)
import           Gimlight.UI.Draw.KeyEvent        (withKeyEvents)
import           Gimlight.UI.Types                (GameWidgetNode)
import           Monomer                          (CmbAlignCenter (alignCenter),
                                                   CmbAlignMiddle (alignMiddle),
                                                   CmbBgColor (bgColor),
                                                   CmbBorderB (borderB),
                                                   CmbFitHeight (fitHeight),
                                                   CmbMultiline (multiline),
                                                   CmbStyleBasic (styleBasic),
                                                   CmbTextColor (textColor),
                                                   CmbTextFont (textFont),
                                                   CmbTextSize (textSize),
                                                   CmbWidth (width),
                                                   Color (Color), StyleState,
                                                   black, box_, filler, hstack,
                                                   image_, label, label_,
                                                   paddingH, paddingV, vstack,
                                                   white, zstack)
import qualified Monomer.Lens                     as L

drawTalking :: TalkingHandler -> GameConfig -> GameWidgetNode
drawTalking th c =
    withKeyEvents $
    zstack
        [ filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
        , talkingWindow c partner (getTalkingPart th)
        ]
  where
    partner = getTalkingPartner th

talkingWindow :: GameConfig -> Actor -> TalkingPart -> GameWidgetNode
talkingWindow c a (Selection h) = hstack [standingPicture, window]
  where
    standingPicture =
        image_ (a ^. standingImagePath) [alignCenter, alignMiddle, fitHeight]
    window = talkingContent c a h `styleBasic` windowStyle
talkingWindow _ _ _ = error "Unable to draw."

talkingContent :: GameConfig -> Actor -> SelectionHandler -> GameWidgetNode
talkingContent c a h =
    box_ [alignCenter] $
    vstack $ nameWidget c a : filler : question c h : filler : selections c h

nameWidget :: GameConfig -> Actor -> GameWidgetNode
nameWidget c a =
    label (getLocalizedText c $ toName $ getIdentifier a) `styleBasic` nameStyle

question :: GameConfig -> SelectionHandler -> GameWidgetNode
question c h =
    label_ (getLocalizedText c $ getQuestion h) [multiline] `styleBasic`
    baseStyle

selections :: GameConfig -> SelectionHandler -> [GameWidgetNode]
selections c h =
    emphasizeSelection h $
    (`styleBasic` baseStyle) . label . getLocalizedText c <$> getChoices h

emphasizeSelection :: SelectionHandler -> [GameWidgetNode] -> [GameWidgetNode]
emphasizeSelection h labels =
    case getSelectingIndex h of
        Just x  -> labels & ix x %~ (`styleBasic` baseStyle <> [textFont bold])
        Nothing -> labels

windowStyle :: [StyleState]
windowStyle =
    [ width dialogWidth
    , paddingH $ fromIntegral windowWidth * 0.05
    , paddingV $ fromIntegral windowHeight * 0.1
    , bgColor (Color 0x0c 0x0c 0x0c 1)
    ]

nameStyle :: [StyleState]
nameStyle =
    [ textColor white
    , textSize 32
    , textFont bold
    , borderB 1 white
    , width dialogWidth
    , paddingV 4
    ]

baseStyle :: [StyleState]
baseStyle = [textColor white, textSize 24]

dialogWidth :: Double
dialogWidth = fromIntegral windowWidth * 0.7
