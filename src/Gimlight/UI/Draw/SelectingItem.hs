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
import           Gimlight.Item.SomeItem            (getName)
import           Gimlight.Localization             (MultilingualText,
                                                    getLocalizedText)
import qualified Gimlight.Localization.Texts       as T
import           Gimlight.UI.Draw.Config           (windowHeight, windowWidth)
import           Gimlight.UI.Draw.Dialog           (heading, selections)
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
                                                    StyleState, box_, vstack,
                                                    zstack)

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
        Just x  -> [selections x $ itemNames sh c]
        Nothing -> []

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
