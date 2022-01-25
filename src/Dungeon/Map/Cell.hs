{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Dungeon.Map.Cell
    ( CellMap(..)
    , rawCellMap
    , TileIdentifierLayer(..)
    , Error(..)
    , upper
    , lower
    , cellMap
    , allWallTiles
    , changeTileAt
    , updateExploredMap
    , updatePlayerFov
    , playerFov
    , playerActor
    , walkableFloors
    , transparentMap
    , exploredMap
    , widthAndHeight
    , isPositionInMap
    , locateActorAt
    , locateItemAt
    , removeActorAt
    , removeItemAt
    , removeActorIf
    , positionsAndActors
    , positionsAndItems
    , tileIdentifierLayerAt
    ) where

import           Actor               (Actor, isPlayer)
import           Control.Lens        (Ixed (ix), makeLenses, preview, view,
                                      (%%~), (%~), (&), (.~), (?~), (^.), (^?))
import           Control.Monad.State (StateT (StateT), gets)
import           Coord               (Coord)
import           Data.Array          (Array, assocs, bounds, listArray, (!),
                                      (//))
import           Data.Binary         (Binary)
import           Data.Foldable       (find)
import qualified Data.Map            as M
import           Data.Maybe          (isJust, isNothing, mapMaybe)
import           Dungeon.Map.Tile    (TileCollection, TileIdentifier, floorTile,
                                      wallTile)
import qualified Dungeon.Map.Tile    as Tile
import           Fov                 (calculateFov)
import           GHC.Generics        (Generic)
import           Item                (Item)
import           Linear.V2           (V2 (V2))

data Error
    = OutOfRange
    | ActorNotFound
    | ActorAlreadyExists Actor
    | ItemNotFound
    | ItemAlreadyExists Item
    | TileIsNotWalkable
    deriving (Show)

data TileIdentifierLayer =
    TileIdentifierLayer
        { _upper :: Maybe TileIdentifier
        , _lower :: Maybe TileIdentifier
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''TileIdentifierLayer

instance Binary TileIdentifierLayer

data Cell =
    Cell
        { _tileIdentifierLayer :: TileIdentifierLayer
        , _actor               :: Maybe Actor
        , _item                :: Maybe Item
        , _explored            :: Bool
        , _visibleFromPlayer   :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Cell

instance Binary Cell

isWalkable :: TileCollection -> Cell -> Bool
isWalkable tc c =
    all ($ c)
        [ (/= Just False) . fmap (Tile.isWalkable . (tc M.!)) .
          view (tileIdentifierLayer . upper)
        , isNothing . view actor
        ]

isTransparent :: TileCollection -> Cell -> Bool
isTransparent tc =
    (/= Just False) . fmap (Tile.isTransparent . (tc M.!)) .
    view (tileIdentifierLayer . upper)

isTileWalkable :: TileCollection -> Cell -> Bool
isTileWalkable tc c =
    fmap (Tile.isWalkable . (tc M.!)) (c ^. tileIdentifierLayer . upper) /=
    Just False

locateActor :: TileCollection -> Actor -> Cell -> Either Error Cell
locateActor tc a c
    | not $ isTileWalkable tc c = Left TileIsNotWalkable
    | otherwise =
        case c ^. actor of
            Just x  -> Left $ ActorAlreadyExists x
            Nothing -> Right $ c & actor ?~ a

locateItem :: TileCollection -> Item -> Cell -> Either Error Cell
locateItem tc i c
    | not $ isTileWalkable tc c = Left TileIsNotWalkable
    | otherwise =
        case c ^. item of
            Just x  -> Left $ ItemAlreadyExists x
            Nothing -> Right $ c & item ?~ i

removeActor :: Cell -> (Either Error Actor, Cell)
removeActor c =
    case c ^. actor of
        Just a  -> (Right a, c & actor .~ Nothing)
        Nothing -> (Left ActorNotFound, c)

removeItem :: Cell -> (Either Error Item, Cell)
removeItem c =
    case c ^. item of
        Just i  -> (Right i, c & item .~ Nothing)
        Nothing -> (Left ItemNotFound, c)

newtype CellMap =
    CellMap
        { _rawCellMap :: Array (V2 Int) Cell
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary CellMap

makeLenses ''CellMap

cellMap :: Array (V2 Int) TileIdentifierLayer -> CellMap
cellMap = CellMap . fmap (\x -> Cell x Nothing Nothing False False)

allWallTiles :: V2 Int -> CellMap
allWallTiles wh = CellMap $ listArray (V2 0 0, wh - V2 1 1) $ repeat cell
  where
    cell =
        Cell
            (TileIdentifierLayer (Just wallTile) (Just floorTile))
            Nothing
            Nothing
            False
            False

widthAndHeight :: CellMap -> V2 Int
widthAndHeight = (+ V2 1 1) . snd . bounds . view rawCellMap

isPositionInMap :: Coord -> CellMap -> Bool
isPositionInMap (V2 x y) cm = x >= 0 && x < w && y >= 0 && y < h
  where
    V2 w h = widthAndHeight cm

changeTileAt ::
       (TileIdentifierLayer -> TileIdentifierLayer)
    -> Coord
    -> CellMap
    -> Maybe CellMap
changeTileAt f c m
    | isJust $ tileIdentifierLayerAt c m =
        Just $ m & rawCellMap %~ (// [(c, newTile)])
    | otherwise = Nothing
  where
    newTile = (m ^. rawCellMap) ! c & tileIdentifierLayer %~ f

walkableFloors :: TileCollection -> CellMap -> Array (V2 Int) Bool
walkableFloors tc = fmap (isWalkable tc) . view rawCellMap

exploredMap :: CellMap -> Array (V2 Int) Bool
exploredMap = fmap (view explored) . view rawCellMap

playerFov :: CellMap -> Array (V2 Int) Bool
playerFov = fmap (view visibleFromPlayer) . view rawCellMap

playerActor :: CellMap -> Maybe (Coord, Actor)
playerActor = find (isPlayer . snd) . positionsAndActors

transparentMap :: TileCollection -> CellMap -> Array (V2 Int) Bool
transparentMap tc = fmap (isTransparent tc) . view rawCellMap

updateExploredMap :: CellMap -> CellMap
updateExploredMap cm =
    cm & rawCellMap %~
    (// [ (pos, (cm ^. rawCellMap) ! pos & explored .~ True)
        | pos <- visibleList
        ])
  where
    visibleList = map fst $ filter snd $ assocs $ playerFov cm

updatePlayerFov :: TileCollection -> CellMap -> Maybe CellMap
updatePlayerFov tc cm =
    fmap
        (\xs ->
             cm & rawCellMap %~
             (// [ ( pos
                   , (cm ^. rawCellMap) ! pos & visibleFromPlayer .~ isVisible)
                 | (pos, isVisible) <- xs
                 ]))
        visibilityList
  where
    visibilityList = fmap assocs fov
    fov = (`calculateFov` transparentMap tc cm) <$> playerPosition
    playerPosition = fst <$> playerActor cm

positionsAndActors :: CellMap -> [(Coord, Actor)]
positionsAndActors = mapMaybe mapStep . assocs . view rawCellMap
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. actor

positionsAndItems :: CellMap -> [(Coord, Item)]
positionsAndItems = mapMaybe mapStep . assocs . view rawCellMap
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. item

locateActorAt ::
       TileCollection -> Actor -> Coord -> StateT CellMap (Either Error) ()
locateActorAt tc a c =
    StateT $ \cm -> fmap ((), ) $ cm & rawCellMap . ix c %%~ locateActor tc a

locateItemAt ::
       TileCollection -> Item -> Coord -> StateT CellMap (Either Error) ()
locateItemAt tc i c =
    StateT $ \cm -> fmap ((), ) $ cm & rawCellMap . ix c %%~ locateItem tc i

removeActorAt :: Coord -> StateT CellMap (Either Error) Actor
removeActorAt c =
    StateT $ \cm ->
        case cm ^? rawCellMap . ix c of
            Just x ->
                case removeActor x of
                    (Right removed, newCell) ->
                        Right (removed, cm & rawCellMap %~ (// [(c, newCell)]))
                    (Left e, _) -> Left e
            Nothing -> Left ActorNotFound

removeItemAt :: Coord -> StateT CellMap (Either Error) Item
removeItemAt c =
    StateT $ \cm ->
        case cm ^? rawCellMap . ix c of
            Just x ->
                case removeItem x of
                    (Right removed, newCell) ->
                        Right (removed, cm & rawCellMap %~ (// [(c, newCell)]))
                    (Left e, _) -> Left e
            Nothing -> Left ItemNotFound

removeActorIf :: (Actor -> Bool) -> StateT CellMap (Either Error) Actor
removeActorIf f = do
    position <- gets $ fmap fst . find (f . snd) . positionsAndActors
    case position of
        Just x  -> removeActorAt x
        Nothing -> StateT $ const $ Left ActorNotFound

tileIdentifierLayerAt :: Coord -> CellMap -> Maybe TileIdentifierLayer
tileIdentifierLayerAt c = preview (rawCellMap . ix c . tileIdentifierLayer)
