{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Gimlight.Dungeon.Map.Cell
    ( CellMap
    , TileIdLayer(..)
    , Error(..)
    , tileIdLayer
    , upper
    , lower
    , cellMap
    , upperAt
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
    , mapActorAt
    , positionsAndActors
    , positionsAndItems
    , tileIdLayerAt
    ) where

import           Control.Lens              (Ixed (ix), Traversal', makeLenses,
                                            preview, view, (%%~), (&), (.~),
                                            (<&>), (?~), (^.), (^?))
import           Control.Monad.State       (MonadTrans (lift), StateT (StateT),
                                            gets)
import           Data.Array                (Array, assocs, bounds, (!), (//))
import           Data.Bifunctor            (Bifunctor (second))
import           Data.Either.Combinators   (maybeToRight)
import           Data.Foldable             (find)
import qualified Data.Map                  as M
import           Data.Maybe                (isNothing, mapMaybe)
import           GHC.Generics              (Generic)
import           Gimlight.Actor            (Actor, isPlayer)
import           Gimlight.Coord            (Coord)
import           Gimlight.Dungeon.Map.Tile (TileCollection, TileId)
import qualified Gimlight.Dungeon.Map.Tile as Tile
import           Gimlight.Fov              (calculateFov)
import           Gimlight.Item.SomeItem    (SomeItem)
import           Linear.V2                 (V2 (V2))

data Error
    = OutOfRange
    | ActorNotFound
    | ActorAlreadyExists Actor
    | ItemNotFound
    | ItemAlreadyExists SomeItem
    | TileIsNotWalkable
    deriving (Show)

data TileIdLayer =
    TileIdLayer
        { _upper :: Maybe TileId
        , _lower :: Maybe TileId
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''TileIdLayer

data Cell =
    Cell
        { _tileIdLayer       :: TileIdLayer
        , _actor             :: Maybe Actor
        , _item              :: Maybe SomeItem
        , _explored          :: Bool
        , _visibleFromPlayer :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Cell

isWalkable :: TileCollection -> Cell -> Bool
isWalkable tc c =
    all ($ c)
        [ (/= Just False) . fmap (Tile.isWalkable . (tc M.!)) .
          view (tileIdLayer . upper)
        , isNothing . view actor
        ]

isTransparent :: TileCollection -> Cell -> Bool
isTransparent tc =
    (/= Just False) . fmap (Tile.isTransparent . (tc M.!)) .
    view (tileIdLayer . upper)

isTileWalkable :: TileCollection -> Cell -> Bool
isTileWalkable tc c =
    fmap (Tile.isWalkable . (tc M.!)) (c ^. tileIdLayer . upper) /= Just False

locateActor :: TileCollection -> Actor -> Cell -> Either Error Cell
locateActor tc a c
    | not $ isTileWalkable tc c = Left TileIsNotWalkable
    | otherwise =
        case c ^. actor of
            Just x  -> Left $ ActorAlreadyExists x
            Nothing -> Right $ c & actor ?~ a

locateItem :: TileCollection -> SomeItem -> Cell -> Either Error Cell
locateItem tc i c
    | not $ isTileWalkable tc c = Left TileIsNotWalkable
    | otherwise =
        case c ^. item of
            Just x  -> Left $ ItemAlreadyExists x
            Nothing -> Right $ c & item ?~ i

removeActor :: Cell -> Either Error (Actor, Cell)
removeActor c =
    fmap (, c & actor .~ Nothing) . maybeToRight ActorNotFound $ c ^. actor

removeItem :: Cell -> Either Error (SomeItem, Cell)
removeItem c =
    fmap (, c & item .~ Nothing) . maybeToRight ItemNotFound $ c ^. item

type CellMap = Array (V2 Int) Cell

cellMap :: Array (V2 Int) TileIdLayer -> CellMap
cellMap = fmap (\x -> Cell x Nothing Nothing False False)

upperAt :: V2 Int -> Traversal' CellMap (Maybe TileId)
upperAt x = ix x . tileIdLayer . upper

widthAndHeight :: CellMap -> V2 Int
widthAndHeight = (+ V2 1 1) . snd . bounds

isPositionInMap :: Coord -> CellMap -> Bool
isPositionInMap (V2 x y) cm = x >= 0 && x < w && y >= 0 && y < h
  where
    V2 w h = widthAndHeight cm

walkableFloors :: TileCollection -> CellMap -> Array (V2 Int) Bool
walkableFloors tc = fmap (isWalkable tc)

exploredMap :: CellMap -> Array (V2 Int) Bool
exploredMap = fmap (view explored)

playerFov :: CellMap -> Array (V2 Int) Bool
playerFov = fmap (view visibleFromPlayer)

playerActor :: CellMap -> Maybe (Coord, Actor)
playerActor = find (isPlayer . snd) . positionsAndActors

transparentMap :: TileCollection -> CellMap -> Array (V2 Int) Bool
transparentMap tc = fmap (isTransparent tc)

updateExploredMap :: CellMap -> CellMap
updateExploredMap cm =
    cm // [(pos, cm ! pos & explored .~ True) | pos <- visibleList]
  where
    visibleList = map fst $ filter snd $ assocs $ playerFov cm

updatePlayerFov :: TileCollection -> CellMap -> Maybe CellMap
updatePlayerFov tc cm = fmap markAsVisible visibilityList
  where
    markAsVisible xs =
        cm //
        [ (pos, cm ! pos & visibleFromPlayer .~ isVisible)
        | (pos, isVisible) <- xs
        ]
    visibilityList = fmap assocs fov
    fov = (`calculateFov` transparentMap tc cm) <$> playerPosition
    playerPosition = fst <$> playerActor cm

positionsAndActors :: CellMap -> [(Coord, Actor)]
positionsAndActors = mapMaybe mapStep . assocs
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. actor

positionsAndItems :: CellMap -> [(Coord, SomeItem)]
positionsAndItems = mapMaybe mapStep . assocs
  where
    mapStep (coord, cell) = (coord, ) <$> cell ^. item

locateActorAt ::
       TileCollection -> Actor -> Coord -> StateT CellMap (Either Error) ()
locateActorAt tc a c =
    StateT $ \cm -> fmap ((), ) $ cm & ix c %%~ locateActor tc a

locateItemAt ::
       TileCollection -> SomeItem -> Coord -> StateT CellMap (Either Error) ()
locateItemAt tc i c =
    StateT $ \cm -> fmap ((), ) $ cm & ix c %%~ locateItem tc i

removeActorAt :: Coord -> StateT CellMap (Either Error) Actor
removeActorAt c =
    StateT $ \cm ->
        maybeToRight OutOfRange (cm ^? ix c) >>= removeActor <&>
        second (\cell -> cm // [(c, cell)])

removeItemAt :: Coord -> StateT CellMap (Either Error) SomeItem
removeItemAt c =
    StateT $ \cm ->
        maybeToRight OutOfRange (cm ^? ix c) >>= removeItem <&>
        second (\cell -> cm // [(c, cell)])

removeActorIf :: (Actor -> Bool) -> StateT CellMap (Either Error) Actor
removeActorIf f =
    gets (fmap fst . find (f . snd) . positionsAndActors) >>= lift .
    maybeToRight ActorNotFound >>=
    removeActorAt

mapActorAt ::
       TileCollection
    -> Coord
    -> (Actor -> Actor)
    -> StateT CellMap (Either Error) ()
mapActorAt tc p f = do
    a <- removeActorAt p
    locateActorAt tc (f a) p

tileIdLayerAt :: Coord -> CellMap -> Maybe TileIdLayer
tileIdLayerAt c = preview (ix c . tileIdLayer)
