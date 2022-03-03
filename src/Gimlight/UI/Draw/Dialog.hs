module Gimlight.UI.Draw.Dialog
    ( dialog
    , heading
    , selections
    , normalText
    , boldText
    , dialogStyle
    ) where

import           Data.Text              (Text)
import           Gimlight.UI.Draw.Fonts (bold)
import           Gimlight.UI.Types      (GameWidgetNode)
import           Monomer                (CmbBgColor (bgColor),
                                         CmbBorderB (borderB),
                                         CmbMultiline (multiline),
                                         CmbPadding (padding),
                                         CmbStyleBasic (styleBasic),
                                         CmbTextColor (textColor),
                                         CmbTextFont (textFont),
                                         CmbTextSize (textSize),
                                         CmbTextSpaceV (textSpaceV),
                                         Color (Color), StyleState, box, label,
                                         label_, vstack, white)

dialog :: GameWidgetNode -> GameWidgetNode
dialog inner = box inner `styleBasic` dialogStyle

heading :: Text -> GameWidgetNode
heading text = label text `styleBasic` headingStyle

selections :: Int -> [Text] -> GameWidgetNode
selections selecting = vstack . fmap toLabel . zip [0 ..]
  where
    toLabel (idx, text)
        | idx == selecting = boldText text
        | otherwise = normalText text

normalText :: Text -> GameWidgetNode
normalText text = label_ text [multiline] `styleBasic` normalTextStyle

boldText :: Text -> GameWidgetNode
boldText text = label text `styleBasic` boldTextStyle

dialogStyle :: [StyleState]
dialogStyle = [bgColor $ Color 0x0c 0x0c 0x0c 1, padding $ normalFontSize * 2]

headingStyle :: [StyleState]
headingStyle =
    commonTextStyle <>
    [textSize headingFontSize, textFont bold, borderB 1 white, padding 4]

normalTextStyle :: [StyleState]
normalTextStyle = textSize normalFontSize : commonTextStyle

boldTextStyle :: [StyleState]
boldTextStyle = textFont bold : normalTextStyle

commonTextStyle :: [StyleState]
commonTextStyle = [textColor white, textSpaceV normalFontSize]

headingFontSize :: Double
headingFontSize = 32

normalFontSize :: Double
normalFontSize = 24
