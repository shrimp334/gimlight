{-# LANGUAGE OverloadedStrings #-}

module Gimlight.SetUp.CellMap
    ( initCellMap
    , initTileCollection
    , playerPosition
    , orcWithoutItemsPosition
    , orcWithFullItemsPosition
    , strongestOrcPosition
    , intermediateOrcPosition
    , weakestOrcPosition
    , orcWithHerbPosition
    , orcWithSwordPosition
    , orcWithArmorPosition
    ) where

import           Codec.Picture             (PixelRGBA8 (PixelRGBA8),
                                            generateImage)
import           Control.Lens              ((%~))
import           Control.Monad.State       (State, evalState, execStateT)
import           Data.Array                (listArray, (//))
import           Data.Map                  (fromList)
import           Data.Maybe                (fromJust)
import           Data.OpenUnion            (liftUnion)
import           Gimlight.Actor            (Actor, inventoryItems, monster,
                                            player)
import           Gimlight.Actor.Identifier (Identifier (Orc))
import           Gimlight.Actor.Monsters   (orc)
import           Gimlight.Actor.Status     (status)
import           Gimlight.Actor.Status.Hp  (hp)
import           Gimlight.Coord            (Coord)
import           Gimlight.Data.Either      (expectRight)
import           Gimlight.Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer),
                                            cellMap, locateActorAt,
                                            locateItemAt)
import           Gimlight.Dungeon.Map.Tile (TileCollection, tile)
import           Gimlight.IndexGenerator   (IndexGenerator, generator)
import           Gimlight.Inventory        (addItem)
import           Gimlight.Item.Defined     (herb, sampleBook, sword,
                                            woodenArmor)
import           Gimlight.UI.Draw.Config   (tileHeight, tileWidth)
import           Linear.V2                 (V2 (V2))

initCellMap :: CellMap
initCellMap =
    expectRight "Failed to set up the test environment." $
    flip execStateT emptyMap $ do
        mapM_
            (locateItemAt initTileCollection (liftUnion herb))
            [playerPosition, orcWithFullItemsPosition]
        mapM_
            (uncurry (locateActorAt initTileCollection))
            [ (p, playerPosition)
            , (orcWithoutItems, orcWithoutItemsPosition)
            , (orcWithFullItems, orcWithFullItemsPosition)
            , (s, strongestOrcPosition)
            , (i, intermediateOrcPosition)
            , (w, weakestOrcPosition)
            , (orcWithHerb, orcWithHerbPosition)
            , (orcWithSword, orcWithSwordPosition)
            , (orcWithArmor, orcWithArmorPosition)
            ]
  where
    emptyMap =
        cellMap $
        listArray (V2 0 0, V2 (mapWidth - 1) (mapHeight - 1)) (repeat emptyTile) //
        [(V2 0 1, unwalkable)]
    (p, w, i, s, orcWithoutItems, orcWithFullItems, orcWithHerb, orcWithSword, orcWithArmor) =
        flip evalState generator $ (,,,,,,,,) <$>
        ((inventoryItems %~ fromJust . addItem (liftUnion sampleBook)) <$>
         player) <*>
        weakestOrc <*>
        intermediateOrc <*>
        strongestOrc <*>
        orc <*>
        ((!! 5) .
         iterate (inventoryItems %~ fromJust . addItem (liftUnion herb)) <$>
         orc) <*>
        ((inventoryItems %~ fromJust . addItem (liftUnion herb)) <$> orc) <*>
        ((inventoryItems %~ fromJust . addItem (liftUnion sword)) <$> orc) <*>
        ((inventoryItems %~ fromJust . addItem (liftUnion woodenArmor)) <$> orc)
    emptyTile = TileIdLayer Nothing Nothing
    unwalkable = TileIdLayer (Just (dummyTileFile, 1)) Nothing
    mapWidth = 3
    mapHeight = 4

initTileCollection :: TileCollection
initTileCollection =
    fromList
        [ ((dummyTileFile, 0), tile True True emptyImage)
        , ((dummyTileFile, 1), tile False True emptyImage)
        ]
  where
    emptyImage = generateImage (\_ _ -> PixelRGBA8 0 0 0 0) tileWidth tileHeight

strongestOrc :: State IndexGenerator Actor
strongestOrc = monster Orc (status (hp 100) 100 100) ""

intermediateOrc :: State IndexGenerator Actor
intermediateOrc = monster Orc (status (hp 100) 50 50) ""

weakestOrc :: State IndexGenerator Actor
weakestOrc = monster Orc (status (hp 1) 0 0) ""

playerPosition :: Coord
playerPosition = V2 0 0

orcWithoutItemsPosition :: Coord
orcWithoutItemsPosition = V2 1 0

orcWithFullItemsPosition :: Coord
orcWithFullItemsPosition = V2 2 0

strongestOrcPosition :: Coord
strongestOrcPosition = V2 1 2

intermediateOrcPosition :: Coord
intermediateOrcPosition = V2 0 3

weakestOrcPosition :: Coord
weakestOrcPosition = V2 1 3

orcWithHerbPosition :: Coord
orcWithHerbPosition = V2 2 1

orcWithSwordPosition :: Coord
orcWithSwordPosition = V2 2 2

orcWithArmorPosition :: Coord
orcWithArmorPosition = V2 2 3

dummyTileFile :: FilePath
dummyTileFile = "dummy.json"
