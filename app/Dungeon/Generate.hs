module Dungeon.Generate
    ( generateDungeon
    ) where

import           Control.Lens            ((^.))
import           Coord                   (Coord)
import           Data.Array              ((//))
import           Dungeon.Entity          (Entity)
import           Dungeon.Entity.Monsters (orc, troll)
import           Dungeon.Map.Tile        (TileMap, allWallTiles, floorTile)
import           Dungeon.Room            (Room (..), center,
                                          roomFromTwoPositionInclusive,
                                          roomFromWidthHeight, roomOverlaps)
import           Dungeon.Size            (height, width)
import           Dungeon.Types           (position)
import           Linear.V2               (V2 (..), _x, _y)
import           System.Random           (Random (randomR), StdGen, random)

generateDungeon :: StdGen -> Int -> Int -> Int -> V2 Int -> (TileMap, [Entity], V2 Int, StdGen)
generateDungeon = generateDungeonAccum [] [] allWallTiles (V2 0 0)

generateDungeonAccum :: [Entity] -> [Room] -> TileMap -> Coord -> StdGen -> Int -> Int -> Int -> V2 Int -> (TileMap, [Entity], V2 Int, StdGen)
generateDungeonAccum enemiesAcc _ d pos g 0 _ _ _ = (d, enemiesAcc, pos, g)
generateDungeonAccum enemiesAcc acc dungeon playerPos g maxRooms roomMinSize roomMaxSize mapSize
    = generateDungeonAccum newEnemiesAcc newAcc newDungeon newPlayerPos g''''' (maxRooms - 1) roomMinSize roomMaxSize mapSize
    where (roomWidth, g') = randomR (roomMinSize, roomMaxSize) g
          (roomHeight, g'') = randomR (roomMinSize, roomMaxSize) g'
          (x, g''') = randomR (0, width - roomWidth - 1) g''
          (y, g'''') = randomR (0, height - roomHeight - 1) g'''
          room = roomFromWidthHeight (V2 x y) (V2 roomWidth roomHeight)
          usable = not $ any (roomOverlaps room) acc
          (enemies, g''''') = placeEnemies g'''' room maxMonstersPerRoom
          (newEnemiesAcc, newAcc, newDungeon, newPlayerPos) = if usable
                                                   then if null acc
                                                            then (enemies ++ enemiesAcc, room:acc, createRoom room dungeon, center room)
                                                            else (enemies ++ enemiesAcc, room:acc, tunnelBetween (center room) (center $ head acc) $ createRoom room dungeon, center room)
                                                   else (enemiesAcc, acc, dungeon, playerPos)

createRoom :: Room -> TileMap -> TileMap
createRoom room r
    = r // [((x, y), floorTile) | x <- [x1 room .. x2 room - 1], y <- [y1 room .. y2 room - 1]]

tunnelBetween :: Coord -> Coord -> TileMap -> TileMap
tunnelBetween start end d = createRoom path1 $ createRoom path2 d
    where path1 = roomFromTwoPositionInclusive start corner
          path2 = roomFromTwoPositionInclusive corner end
          corner = V2 (start ^. _x) (end ^. _y)

maxMonstersPerRoom :: Int
maxMonstersPerRoom = 1

placeEnemies :: StdGen -> Room -> Int -> ([Entity], StdGen)
placeEnemies = placeEnemiesAccum []

placeEnemiesAccum :: [Entity] -> StdGen -> Room -> Int -> ([Entity], StdGen)
placeEnemiesAccum e g _ 0 = (e, g)
placeEnemiesAccum e g r n =
        placeEnemiesAccum newEnemies g''' r (n - 1)
        where (x, g') = randomR (x1 r, x2 r - 1) g
              (y, g'') = randomR (y1 r, y2 r - 1) g'
              (enemy, g''') = newMonster g'' (V2 x y)
              newEnemies = if V2 x y `notElem` map (^. position) e
                            then enemy:e
                            else e

newMonster :: StdGen -> Coord -> (Entity, StdGen)
newMonster g c =
        let (r, g') = random g :: (Float, StdGen)
        in if r < 0.8
            then (orc c, g')
            else (troll c, g')
