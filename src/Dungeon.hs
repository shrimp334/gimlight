-- I refer to "Dungeon" in this source code as the mixed things of map and
-- actors because I could not come up with a much more proper word.  So,
-- in this code, "Dungeon" means not only dungeon but also towns, etc.
--
-- TODO: Change the word to more precise one.
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Dungeon
    ( Dungeon
    , dungeon
    , getIdentifier
    , getActors
    , changeTile
    , popActorAt
    , popActorIf
    , pushActor
    , walkableFloor
    , getPlayerActor
    , mapWidthAndHeight
    , playerPosition
    , stairsPositionCandidates
    , updateMap
    , calculateFovAt
    , isTown
    , isPositionInDungeon
    , npcs
    , positionOnParentMap
    , tileMap
    , visible
    , explored
    , items
    , popItemAt
    , descendingStairs
    , addAscendingAndDescendingStiars
    , addDescendingStairs
    , ascendingStairs
    , pushItem
    ) where

import           Actor                (Actor, isPlayer)
import qualified Actor                as A
import           Control.Lens         (makeLenses, (%~), (&), (.~), (^.))
import           Coord                (Coord)
import           Data.Array.Base      (assocs)
import           Data.Binary          (Binary)
import           Data.Foldable        (find)
import           Data.List            (findIndex)
import           Dungeon.Identifier   (Identifier)
import qualified Dungeon.Identifier   as Identifier
import           Dungeon.Map.Bool     (BoolMap)
import           Dungeon.Map.Explored (ExploredMap, initExploredMap,
                                       updateExploredMap)
import           Dungeon.Map.Fov      (Fov, calculateFov, initFov)
import           Dungeon.Map.Tile     (TileCollection, TileId, TileMap,
                                       changeTileAt, walkableMap,
                                       widthAndHeight)
import qualified Dungeon.Map.Tile     as TileMap
import           Dungeon.Stairs       (StairsPair (StairsPair, downStairs, upStairs))
import           GHC.Generics         (Generic)
import           Item                 (Item)
import qualified Item                 as I
import           Linear.V2            (V2 (..))

data Dungeon =
    Dungeon
        { _tileMap             :: TileMap
        , _visible             :: Fov
        , _explored            :: ExploredMap
        , _actors              :: [Actor]
        , _items               :: [Item]
        , _positionOnParentMap :: Maybe Coord
          -- Do not integrate `_ascendingStairs` with
          -- `_positionOnParentMap` For example, towns have a `Just`
          -- `_positionOnParentMap` but they do not have ascending stairs
          -- to the global map.
        , _ascendingStairs     :: Maybe StairsPair
        , _descendingStairs    :: [StairsPair]
        , _identifier          :: Identifier
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Dungeon

instance Binary Dungeon

dungeon :: TileMap -> [Actor] -> [Item] -> Identifier -> Dungeon
dungeon t e i ident =
    Dungeon
        { _tileMap = t
        , _visible = initFov (widthAndHeight t)
        , _explored = initExploredMap (widthAndHeight t)
        , _actors = e
        , _items = i
        , _positionOnParentMap = Nothing
        , _ascendingStairs = Nothing
        , _descendingStairs = []
        , _identifier = ident
        }

getIdentifier :: Dungeon -> Identifier
getIdentifier d = d ^. identifier

changeTile :: Coord -> TileId -> Dungeon -> Maybe Dungeon
changeTile c t d = (\x -> d & tileMap .~ x) <$> changeTileAt c t (d ^. tileMap)

addAscendingAndDescendingStiars ::
       StairsPair -> (Dungeon, Dungeon) -> (Dungeon, Dungeon)
addAscendingAndDescendingStiars sp@(StairsPair upper _) (parent@Dungeon {_descendingStairs = ss}, child@Dungeon { _ascendingStairs = Nothing
                                                                                                                , _positionOnParentMap = Nothing
                                                                                                                }) =
    ( parent {_descendingStairs = sp : ss}
    , child {_ascendingStairs = Just sp, _positionOnParentMap = Just upper})
addAscendingAndDescendingStiars _ _ =
    error "The child's position and the ascending stairs are already set."

addDescendingStairs :: StairsPair -> (Dungeon, Dungeon) -> (Dungeon, Dungeon)
addDescendingStairs sp@(StairsPair upper _) (parent@Dungeon {_descendingStairs = ss}, child@Dungeon {_positionOnParentMap = Nothing}) =
    ( parent {_descendingStairs = sp : ss}
    , child {_positionOnParentMap = Just upper})
addDescendingStairs _ _ =
    error "The child's position in the parent map is already set."

updateMap :: TileCollection -> Dungeon -> Maybe Dungeon
updateMap ts = updateFov ts . updateExplored

updateExplored :: Dungeon -> Dungeon
updateExplored d =
    d & explored .~ updateExploredMap (d ^. visible) (d ^. explored)

updateFov :: TileCollection -> Dungeon -> Maybe Dungeon
updateFov ts d =
    (\pos -> d & visible .~ calculateFovAt pos ts d) <$> playerPosition d

calculateFovAt :: Coord -> TileCollection -> Dungeon -> BoolMap
calculateFovAt c ts d = calculateFov c (transparentMap ts d)

playerPosition :: Dungeon -> Maybe Coord
playerPosition d = (^. A.position) <$> getPlayerActor d

getPlayerActor :: Dungeon -> Maybe Actor
getPlayerActor d = find isPlayer $ d ^. actors

getActors :: Dungeon -> [Actor]
getActors d = d ^. actors

pushActor :: Actor -> Dungeon -> Dungeon
pushActor e d = d & actors %~ (e :)

pushItem :: Item -> Dungeon -> Dungeon
pushItem i d = d & items %~ (i :)

popActorAt :: Coord -> Dungeon -> (Maybe Actor, Dungeon)
popActorAt c = popActorIf (\x -> x ^. A.position == c)

popActorIf :: (Actor -> Bool) -> Dungeon -> (Maybe Actor, Dungeon)
popActorIf f d =
    let xs = d ^. actors
     in case findIndex f xs of
            Just x ->
                let actor = xs !! x
                    newEntities = take x xs ++ drop (x + 1) xs
                 in (Just actor, d & actors .~ newEntities)
            Nothing -> (Nothing, d)

popItemAt :: Coord -> Dungeon -> (Maybe Item, Dungeon)
popItemAt c = popItemIf (\x -> I.getPosition x == c)

popItemIf :: (Item -> Bool) -> Dungeon -> (Maybe Item, Dungeon)
popItemIf f d =
    let xs = d ^. items
     in case findIndex f xs of
            Just x ->
                let item = xs !! x
                    newItems = take x xs ++ drop (x + 1) xs
                 in (Just item, d & items .~ newItems)
            Nothing -> (Nothing, d)

stairsPositionCandidates :: TileCollection -> Dungeon -> [Coord]
stairsPositionCandidates ts d =
    filter (not . isStairsOnPosition) $ walkableCoords d
  where
    walkableCoords = map fst . filter snd . assocs . walkableFloor ts
    isStairsOnPosition c = isUpStairsPosition c || isDownStairsPosition c
    isUpStairsPosition c = (downStairs <$> d ^. ascendingStairs) == Just c
    isDownStairsPosition c = c `elem` map upStairs (d ^. descendingStairs)

walkableFloor :: TileCollection -> Dungeon -> BoolMap
walkableFloor ts d = walkableMap ts (d ^. tileMap)

transparentMap :: TileCollection -> Dungeon -> BoolMap
transparentMap ts d = TileMap.transparentMap ts (d ^. tileMap)

npcs :: Dungeon -> [Actor]
npcs d = filter (not . isPlayer) $ d ^. actors

mapWidthAndHeight :: Dungeon -> V2 Int
mapWidthAndHeight d = TileMap.widthAndHeight (d ^. tileMap)

isTown :: Dungeon -> Bool
isTown d = Identifier.isTown $ d ^. identifier

isPositionInDungeon :: Coord -> Dungeon -> Bool
isPositionInDungeon c d = x >= 0 && x < width && y >= 0 && y < height
  where
    V2 width height = mapWidthAndHeight d
    V2 x y = c
