{-# LANGUAGE TemplateHaskell #-}

module Dungeon
    ( initDungeon
    , Dungeon
    , completeThisTurn
    , entities
    , visible
    , explored
    , tileMap
    , popPlayer
    , popActorAt
    , enemies
    , pushEntity
    , walkableFloor
    , getPlayerEntity
    , enemyCoords
    , aliveEnemies
    ) where

import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Lens.Getter            (use)
import           Control.Monad                  (join)
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State      (State, evalState, runState,
                                                 state)
import           Control.Monad.Trans.State.Lazy (execState, get, modify)
import           Coord                          (Coord (..))
import           Data.Array                     (Array)
import           Data.Array.Base                (array, bounds, elems, (!),
                                                 (//))
import           Data.Foldable                  (find)
import           Data.List                      (findIndex)
import           Data.Maybe                     (isJust, isNothing)
import           Dungeon.Generate               (generateDungeon)
import           Dungeon.Room                   (Room (..), x1, x2, y1, y2)
import           Dungeon.Size                   (height, maxRooms, roomMaxSize,
                                                 roomMinSize, width)
import qualified Dungeon.Turn                   as DT
import           Entity                         (Entity (..), isAlive, name,
                                                 position)
import qualified Entity                         as E
import           Graphics.Vty.Attributes.Color  (Color, white, yellow)
import           Linear.V2                      (V2 (..), _x, _y)
import           Log                            (Message, message)
import qualified Map                            as M
import           Map.Bool                       (BoolMap, emptyBoolMap)
import           Map.Explored                   (ExploredMap, updateExploredMap)
import           Map.Fov                        (Fov, calculateFov)
import           Map.Tile                       (Tile, TileMap, darkAttr,
                                                 lightAttr, transparent,
                                                 walkable)
import           System.Random.Stateful         (StdGen, newStdGen, random,
                                                 randomR)

data Dungeon = Dungeon
          { _tileMap  :: TileMap
          , _visible  :: Fov
          , _explored :: ExploredMap
          , _entities :: [Entity]
          } deriving (Show)
makeLenses ''Dungeon

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
            p = getPlayerEntity d

        visible .= calculateFov (p ^. position) t

getPlayerEntity :: Dungeon -> Entity
getPlayerEntity d =
        case find (^. E.isPlayer) $ d ^. entities of
            Just p  -> p
            Nothing -> error "No player entity."

pushEntity :: Entity -> State Dungeon ()
pushEntity e = state $ \d@Dungeon{ _entities = entities } -> ((), d { _entities = e:entities })

popPlayer :: State Dungeon Entity
popPlayer = state $ \d -> case runState (popActorIf (^. E.isPlayer)) d of
                  (Just x, d') -> (x, d')
                  (Nothing, _) -> error "No player entity."

popActorAt :: Coord -> State Dungeon (Maybe Entity)
popActorAt c = popActorIf (\x -> x ^. position == c)

popActorIf :: (Entity -> Bool) -> State Dungeon (Maybe Entity)
popActorIf f = state $ \d@Dungeon{ _entities = entities } ->
    case findIndex f entities of
        Just x -> let entity = entities !! x
                      newEntities = take x entities ++ drop (x + 1) entities
                  in (Just entity, d{ _entities = newEntities})
        Nothing -> (Nothing, d)

walkableFloor :: Dungeon -> BoolMap
walkableFloor d = M.generate (\c -> ((d ^. tileMap) ! c) ^. walkable)

transparentMap :: Dungeon -> BoolMap
transparentMap d = fmap (^. transparent) (d ^. tileMap)

enemyCoords :: Dungeon -> [Coord]
enemyCoords d = map (^. position) $ filter (not . (^. E.isPlayer)) $ d ^. entities

isPlayerAlive :: Dungeon -> Bool
isPlayerAlive d = getPlayerEntity d ^. isAlive

aliveEnemies :: Dungeon -> [Entity]
aliveEnemies d = filter (^. isAlive) $ enemies d

enemies :: Dungeon -> [Entity]
enemies d = filter (not . (^. E.isPlayer)) $ d ^. entities

initDungeon :: IO Dungeon
initDungeon = do
        gen <- newStdGen
        let (dungeon, enemies, playerPos, _) = generateDungeon gen maxRooms roomMinSize roomMaxSize (V2 width height)
        let player = E.player playerPos
        let g = Dungeon { _tileMap = dungeon
                        , _visible = emptyBoolMap
                        , _explored = emptyBoolMap
                        , _entities = player:enemies
                        }
        return $ execState updateMap g
