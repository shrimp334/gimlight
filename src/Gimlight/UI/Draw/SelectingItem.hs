module Gimlight.UI.Draw.SelectingItem
    ( drawSelectingItem
    ) where

import           Data.Text                         (Text)
import           Gimlight.GameConfig               (GameConfig)
import           Gimlight.GameStatus.SelectingItem (Reason (Drop, Use),
                                                    SelectingItemHandler,
                                                    getExploringHandler,
                                                    getItems, getReason,
                                                    getSelectingIndex)
import           Gimlight.Item.SomeItem            (getIconImagePath, getName)
import           Gimlight.Localization             (MultilingualText,
                                                    getLocalizedText)
import qualified Gimlight.Localization.Texts       as T
import           Gimlight.UI.Draw.Config           (tileHeight, tileWidth,
                                                    windowHeight, windowWidth)
import           Gimlight.UI.Draw.Dialog           (boldTextStyle, heading,
                                                    normalTextStyle)
import qualified Gimlight.UI.Draw.Dialog           as Dialog
import           Gimlight.UI.Draw.Exploring        (drawExploring)
import           Gimlight.UI.Draw.KeyEvent         (withKeyEvents)
import           Gimlight.UI.Draw.Shadow           (shadow)
import           Gimlight.UI.Types                 (GameWidgetNode)
import           Monomer                           (CmbAlignCenter (alignCenter),
                                                    CmbAlignMiddle (alignMiddle),
                                                    CmbHeight (height),
                                                    CmbStyleBasic (styleBasic),
                                                    CmbWidth (width),
                                                    StyleState, box_, hstack,
                                                    image, label, spacer,
                                                    vstack, zstack)

drawSelectingItem :: SelectingItemHandler -> GameConfig -> GameWidgetNode
drawSelectingItem sh c =
    withKeyEvents $
    zstack
        [ drawExploring eh c
        , shadow
        , box_
              [alignMiddle, alignCenter]
              (vstack labels `styleBasic` dialogStyle)
        ]
  where
    eh = getExploringHandler sh
    labels = titleLabel sh c : itemLabels sh c

titleLabel :: SelectingItemHandler -> GameConfig -> GameWidgetNode
titleLabel sh c = heading $ getLocalizedText c $ titleLabelText sh

itemLabels :: SelectingItemHandler -> GameConfig -> [GameWidgetNode]
itemLabels sh c =
    case getSelectingIndex sh of
        Just x  -> [selectionsWithImages x $ itemPathsAndNames sh c]
        Nothing -> []

selectionsWithImages :: Int -> [(Text, Text)] -> GameWidgetNode
selectionsWithImages selecting = vstack . fmap toLabel . zip [0 ..]
  where
    toLabel (idx, (path, text)) = row (style idx) path text
    style idx
        | idx == selecting = boldTextStyle
        | otherwise = normalTextStyle

row :: [StyleState] -> Text -> Text -> GameWidgetNode
row style path text =
    hstack
        [ image path `styleBasic` [width $ fromIntegral tileWidth]
        , spacer
        , label text `styleBasic` style
        ] `styleBasic`
    [height $ fromIntegral tileHeight]

itemPathsAndNames :: SelectingItemHandler -> GameConfig -> [(Text, Text)]
itemPathsAndNames sh c = zip (itemPaths sh) (itemNames sh c)

itemPaths :: SelectingItemHandler -> [Text]
itemPaths = fmap getIconImagePath . getItems

itemNames :: SelectingItemHandler -> GameConfig -> [Text]
itemNames sh c = fmap (getLocalizedText c . getName) (getItems sh)

dialogStyle :: [StyleState]
dialogStyle =
    Dialog.dialogStyle <>
    [ width $ fromIntegral windowWidth * 0.8
    , height $ fromIntegral windowHeight * 0.8
    ]

titleLabelText :: SelectingItemHandler -> MultilingualText
titleLabelText sh =
    case getReason sh of
        Use  -> T.whatToUse
        Drop -> T.whatToDrop
