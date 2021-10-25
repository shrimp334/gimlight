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
    , completeThisTurn
    , popPlayer
    , popActorAt
    , monsters
    , pushActor
    , walkableFloor
    , getPlayerActor
    , enemyCoords
    , mapWidthAndHeight
    , playerPosition
    , initialPlayerPositionCandidates
    , updateMap
    , isGlobalMap
    , isTown
    , actorAt
    , isPositionInDungeon
    , npcs
    , positionOnParentMap
    , DungeonKind(..)
    , actors
    , tileMap
    , visible
    , explored
    , items
    , popItemAt
    , pushItem
    , descendingStairs
    , addAscendingAndDescendingStiars
    , addDescendingStairs
    , ascendingStairs
    ) where

import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Lens.Getter            (use)
import           Control.Monad.Trans.State      (State, runState, state)
import           Control.Monad.Trans.State.Lazy (get)
import           Coord                          (Coord)
import           Data.Array.Base                (IArray (bounds), assocs)
import           Data.Binary                    (Binary)
import           Data.Foldable                  (find)
import           Data.List                      (findIndex)
import           Data.Maybe                     (isJust)
import           Dungeon.Actor                  (Actor, isMonster, isPlayer)
import qualified Dungeon.Actor                  as A
import           Dungeon.Item                   (Item)
import qualified Dungeon.Item                   as I
import           Dungeon.Map.Bool               (BoolMap)
import           Dungeon.Map.Explored           (ExploredMap, initExploredMap,
                                                 updateExploredMap)
import           Dungeon.Map.Fov                (Fov, calculateFov, initFov)
import           Dungeon.Map.Tile               (TileMap, transparent, walkable)
import           Dungeon.Stairs                 (StairsPair (StairsPair))
import qualified Dungeon.Turn                   as DT
import           GHC.Generics                   (Generic)
import           Linear.V2                      (V2 (..))

data DungeonKind = Town | DungeonType | GlobalMap deriving (Show, Ord, Eq, Generic)
instance Binary DungeonKind

data Dungeon = Dungeon
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
          , _dungeonKind         :: DungeonKind
          } deriving (Show, Ord, Eq, Generic)
makeLenses ''Dungeon
instance Binary Dungeon

dungeon :: TileMap -> [Actor] -> [Item] -> DungeonKind -> Dungeon
dungeon t e i d = Dungeon { _tileMap = t
                          , _visible = initFov widthAndHeight
                          , _explored = initExploredMap widthAndHeight
                          , _actors = e
                          , _items = i
                          , _positionOnParentMap = Nothing
                          , _ascendingStairs = Nothing
                          , _descendingStairs = []
                          , _dungeonKind = d
                          }
    where widthAndHeight = snd (bounds t) + V2 1 1

addAscendingAndDescendingStiars :: StairsPair -> (Dungeon, Dungeon) -> (Dungeon, Dungeon)
addAscendingAndDescendingStiars
    sp@(StairsPair upper _)
    ( parent@Dungeon { _descendingStairs = ss }
    , child@Dungeon { _ascendingStairs = Nothing, _positionOnParentMap = Nothing }) =
    (parent { _descendingStairs = sp: ss }, child { _ascendingStairs = Just sp, _positionOnParentMap = Just upper })
addAscendingAndDescendingStiars _ _ = error "The child's position and the ascending stairs are already set."

addDescendingStairs :: StairsPair -> (Dungeon, Dungeon) -> (Dungeon, Dungeon)
addDescendingStairs sp@(StairsPair upper _) (parent@Dungeon { _descendingStairs = ss }, child@Dungeon { _positionOnParentMap = Nothing } ) =
    (parent { _descendingStairs = sp:ss }, child { _positionOnParentMap = Just upper })
addDescendingStairs _ _ = error "The child's position in the parent map is already set."

completeThisTurn :: State Dungeon DT.Status
completeThisTurn = do
        updateMap
        d <- get
        return $ if isPlayerAlive d then DT.Success else DT.PlayerKilled

updateMap :: State Dungeon ()
updateMap = do
        updateExplored
        updateFov

updateExplored :: State Dungeon ()
updateExplored = do
        v <- use visible
        e <- use explored

        explored .= updateExploredMap e v

updateFov :: State Dungeon ()
updateFov = do
    d <- get

    let t = transparentMap d
        p = getPlayerActor d

    case p of
        Just p' -> visible .= calculateFov (p' ^. A.position) t
        Nothing -> return ()

playerPosition :: Dungeon -> Maybe Coord
playerPosition d = (^. A.position) <$> getPlayerActor d

getPlayerActor :: Dungeon -> Maybe Actor
getPlayerActor d = find isPlayer $ d ^. actors

actorAt :: Coord -> Dungeon -> Maybe Actor
actorAt c d = find (\x -> x ^. A.position == c) $ d ^. actors

pushActor :: Actor -> State Dungeon ()
pushActor e = state $ \d -> ((), d & actors %~ (e :))

popPlayer :: State Dungeon Actor
popPlayer = state $ \d -> case runState (popActorIf isPlayer) d of
                  (Just x, d') -> (x, d')
                  (Nothing, _) -> error "No player actor."

popActorAt :: Coord -> State Dungeon (Maybe Actor)
popActorAt c = popActorIf (\x -> x ^. A.position == c)

popActorIf :: (Actor -> Bool) -> State Dungeon (Maybe Actor)
popActorIf f = state $ \d ->
    let xs = d ^. actors
    in case findIndex f xs of
        Just x -> let actor = xs !! x
                      newEntities = take x xs ++ drop (x + 1) xs
                  in (Just actor, d & actors .~ newEntities)
        Nothing -> (Nothing, d)

pushItem :: Item -> State Dungeon ()
pushItem i = state $ \d -> ((), d & items %~ (i :))

popItemAt :: Coord -> State Dungeon (Maybe Item)
popItemAt c = popItemIf (\x -> x ^. I.position == c)

popItemIf :: (Item -> Bool) -> State Dungeon (Maybe Item)
popItemIf f = state $ \d ->
    let xs = d ^. items
    in case findIndex f xs of
        Just x -> let item = xs !! x
                      newItems = take x xs ++ drop (x + 1) xs
                  in (Just item, d & items .~ newItems)
        Nothing -> (Nothing, d)

initialPlayerPositionCandidates :: Dungeon -> [Coord]
initialPlayerPositionCandidates d = filter (\x -> x `notElem` map (^. A.position) (d ^. actors)) $
    map fst $ filter snd $ assocs $ walkableFloor d

walkableFloor :: Dungeon -> BoolMap
walkableFloor d = fmap (^. walkable) (d ^. tileMap)

transparentMap :: Dungeon -> BoolMap
transparentMap d = fmap (^. transparent) (d ^. tileMap)

enemyCoords :: Dungeon -> [Coord]
enemyCoords d = map (^. A.position) $ filter (not . isPlayer) $ d ^. actors

isPlayerAlive :: Dungeon -> Bool
isPlayerAlive d = isJust $ getPlayerActor d

npcs :: Dungeon -> [Actor]
npcs d = filter (not . isPlayer) $ d ^. actors

monsters :: Dungeon -> [Actor]
monsters d = filter isMonster $ d ^. actors

mapWidthAndHeight :: Dungeon -> V2 Int
mapWidthAndHeight d = snd (bounds $ d ^. tileMap) + V2 1 1

isGlobalMap :: Dungeon -> Bool
isGlobalMap d = (d ^. dungeonKind) == GlobalMap

isTown :: Dungeon -> Bool
isTown d = (d ^. dungeonKind) ==  Town

isPositionInDungeon :: Coord -> Dungeon -> Bool
isPositionInDungeon c d = x >= 0 && x < width && y >= 0 && y < height
    where V2 width height = mapWidthAndHeight d
          V2 x y = c
