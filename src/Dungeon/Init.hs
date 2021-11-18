module Dungeon.Init
    ( initDungeon
    ) where

import           Actor                     (player)
import           Data.Maybe                (fromMaybe)
import           Dungeon                   (Dungeon, pushActor, updateMap)
import           Dungeon.Predefined.Beaeve (beaeve)
import           Linear.V2                 (V2 (V2))

initDungeon :: Dungeon
initDungeon =
    fromMaybe
        (error "Failed to initialize the first map.")
        (updateMap $ pushActor (player $ V2 5 5) beaeve)
