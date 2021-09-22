{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon.Tile
    ( Tile(..)
    , floorTile
    , wallTile
    , walkable
    , transparent
    , darkAttr
    , lightAttr
    ) where

import           Brick.AttrMap   (AttrName)
import           Control.Lens.TH (makeLenses)

data Tile = Tile
          { _walkable    :: Bool
          , _transparent :: Bool
          , _darkAttr    :: AttrName
          , _lightAttr   :: AttrName
          } deriving (Show)
makeLenses ''Tile

wallTile :: Tile
wallTile = Tile { _walkable = False
            , _transparent = False
            , _darkAttr = "darkWallAttr"
            , _lightAttr = "lightWallAttr"
            }

floorTile :: Tile
floorTile = Tile { _walkable = True
             , _transparent = True
             , _darkAttr = "darkFloorAttr"
             , _lightAttr = "lightFloorAttr"
             }
