{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Dungeon.Map.TileSpec
    ( mockTileCollection
    , walkable
    , unwalkable
    ) where

import           Codec.Picture             (PixelRGBA8 (PixelRGBA8),
                                            generateImage)
import           Data.Map                  (fromList)
import           Gimlight.Dungeon.Map.Tile (TileCollection, TileId, tile)
import           Gimlight.UI.Draw.Config   (tileHeight, tileWidth)

mockTileCollection :: TileCollection
mockTileCollection =
    fromList
        [ (walkable, tile True True emptyImage)
        , (unwalkable, tile False True emptyImage)
        ]
  where
    emptyImage = generateImage (\_ _ -> PixelRGBA8 0 0 0 0) tileWidth tileHeight

walkable :: TileId
walkable = (dummyTileFile, 0)

unwalkable :: TileId
unwalkable = (dummyTileFile, 1)

dummyTileFile :: FilePath
dummyTileFile = "dummy.json"
