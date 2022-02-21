{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Dungeon.Map.Tile
    ( Tile
    , TileCollection
    , TileId
    , TileIndex
    , tile
    , isWalkable
    , isTransparent
    , getImage
    ) where

import           Codec.Picture (Image (imageData, imageHeight, imageWidth),
                                PixelRGBA8)
import           Data.Map      (Map)
import           GHC.Generics  (Generic)

data Tile =
    Tile
        { walkable    :: Bool
        , transparent :: Bool
        , image       :: Image PixelRGBA8
        }
    deriving (Eq, Generic)

instance Show Tile where
    show t =
        "Tile {walkable = " ++
        show (walkable t) ++
        ", transparent = " ++ show (transparent t) ++ ", ...}"

instance Ord Tile where
    a <= b =
        walkable a <= walkable a &&
        transparent a <= transparent b &&
        imageWidth (image a) <= imageWidth (image b) &&
        imageHeight (image a) <= imageHeight (image b) &&
        imageData (image a) <= imageData (image b)

type TileCollection = Map TileId Tile

type TileId = (FilePath, Int)

type TileIndex = Int

tile :: Bool -> Bool -> Image PixelRGBA8 -> Tile
tile = Tile

getImage :: Tile -> Image PixelRGBA8
getImage = image

isWalkable :: Tile -> Bool
isWalkable = walkable

isTransparent :: Tile -> Bool
isTransparent = transparent
