{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.SelectingItem
    ( drawSelectingItem
    ) where

import           Dungeon.Item                        (getName)
import           GameModel.Config                    (Config)
import           GameModel.Status.SelectingItemToUse (SelectingItemToUseHandler,
                                                      getItems,
                                                      getSelectingIndex)
import           Localization                        (getLocalizedText)
import qualified Localization.Texts                  as T
import           Monomer                             (label, vstack)
import           TextShow                            (TextShow (showt))
import           UI.Draw.KeyEvent                    (withKeyEvents)
import           UI.Types                            (GameWidgetNode)

drawSelectingItem :: SelectingItemToUseHandler -> Config -> GameWidgetNode
drawSelectingItem sh c = withKeyEvents $ vstack labels
  where
    labels = label topLabel : map label addAsterlist
    addAsterlist =
        zipWith
            (\idx x ->
                 if Just idx == getSelectingIndex sh
                     then "* " <> showt idx <> " " <> x
                     else showt idx <> " " <> x)
            [0 ..] $
        map (getLocalizedText c) itemNames
    itemNames = map getName $ getItems sh
    topLabel = getLocalizedText c T.whichItemToUse
