module Gimlight.Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Control.Monad.State              (State, StateT)
import           Data.Tree                        (Tree)
import           Gimlight.Coord                   (Coord)
import           Gimlight.Dungeon                 (Dungeon)
import           Gimlight.Dungeon.Generate        (generateMultipleFloorsDungeon)
import           Gimlight.Dungeon.Generate.Config (config)
import           Gimlight.Dungeon.Identifier      (Identifier (BatsCave))
import           Gimlight.Dungeon.Map.Tile        (TileCollection)
import           Gimlight.IndexGenerator          (IndexGenerator)
import           Linear.V2                        (V2 (V2))
import           System.Random                    (StdGen)

batsDungeon ::
       TileCollection
    -> StateT IndexGenerator (State StdGen) (Tree Dungeon, Coord)
batsDungeon ts = generateMultipleFloorsDungeon ts cfg BatsCave
  where
    cfg = config 3 10 (V2 5 8) (V2 50 50) "tiles/cave.json"
