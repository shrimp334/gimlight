module Gimlight.UI.Draw.Shadow
    ( shadow
    ) where

import           Control.Lens      ((&), (.~))
import           Gimlight.UI.Types (GameWidgetNode)
import           Monomer           (CmbBgColor (bgColor),
                                    CmbStyleBasic (styleBasic), black, filler)
import qualified Monomer.Lens      as L

shadow :: GameWidgetNode
shadow = filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
