{-# LANGUAGE GADTs #-}

module Gimlight.Dungeon.Generate
    ( generateMultipleFloorsDungeon
    ) where

import           Control.Lens                     (Ixed (ix), _2, _Just, view,
                                                   (&), (.~), (?~), (^.), (^?))
import           Control.Monad.Morph              (MFunctor (hoist), generalize)
import           Control.Monad.State              (MonadTrans (lift), State,
                                                   StateT, execStateT)
import           Data.Array                       (listArray, (!))
import           Data.Bits                        (Bits (bit, complement, (.&.), (.|.)))
import           Data.Either                      (fromRight)
import           Data.Foldable                    (foldlM)
import           Data.List                        (elemIndex)
import           Data.Maybe                       (isNothing)
import           Data.OpenUnion                   (liftUnion)
import           Data.Tree                        (Tree (Node, rootLabel, subForest))
import           Gimlight.Actor                   (Actor)
import           Gimlight.Actor.Monsters          (orc, troll)
import           Gimlight.Coord                   (Coord)
import           Gimlight.Data.List               (filterAll)
import           Gimlight.Data.Maybe              (expectJust)
import           Gimlight.Dungeon                 (Dungeon,
                                                   addAscendingAndDescendingStiars,
                                                   cellMap, dungeon,
                                                   stairsPositionCandidates)
import           Gimlight.Dungeon.Generate.Config (Config, getMapSize,
                                                   getMaxRooms, getNumOfFloors,
                                                   getRoomMaxSize,
                                                   getRoomMinSize,
                                                   getTileFilePath)
import           Gimlight.Dungeon.Generate.Room   (Room (..), center,
                                                   roomFromTwoPositionInclusive,
                                                   roomFromWidthHeight,
                                                   roomOverlaps)
import           Gimlight.Dungeon.Identifier      (Identifier)
import           Gimlight.Dungeon.Map.Cell        (CellMap,
                                                   TileIdLayer (TileIdLayer),
                                                   locateActorAt, locateItemAt,
                                                   tileIdLayer, upper, upperAt,
                                                   widthAndHeight)
import qualified Gimlight.Dungeon.Map.Cell        as C
import           Gimlight.Dungeon.Map.Tile        (TileCollection, TileId,
                                                   TileIndex)
import           Gimlight.Dungeon.Stairs          (StairsPair (StairsPair))
import           Gimlight.IndexGenerator          (IndexGenerator)
import           Gimlight.Item.Defined            (herb, sampleBook, sword,
                                                   woodenArmor)
import           Gimlight.System.Random           (choiceST, randomRST,
                                                   randomST)
import           Gimlight.TreeZipper              (TreeZipper, appendNode,
                                                   getFocused, goDownBy,
                                                   goToRootAndGetTree, modify,
                                                   treeZipper)
import           Linear.V2                        (V2 (..), _x, _y)
import           System.Random                    (StdGen)

generateMultipleFloorsDungeon ::
       TileCollection
    -> Config
    -> Identifier
    -> StateT IndexGenerator (State StdGen) (Tree Dungeon, Coord)
generateMultipleFloorsDungeon ts cfg ident = do
    (firstFloor, ascendingStairsInFirstFloor) <- generateDungeon ts cfg ident
    let treeWithFirstFloor = Node {rootLabel = firstFloor, subForest = []}
        zipperWithFirstFloor = treeZipper treeWithFirstFloor
    dungeonZipper <-
        foldlM
            (\dacc _ -> generateDungeonAndAppend dacc ts cfg ident)
            zipperWithFirstFloor
            [1 .. getNumOfFloors cfg - 1]
    return (goToRootAndGetTree dungeonZipper, ascendingStairsInFirstFloor)

generateDungeonAndAppend ::
       TreeZipper Dungeon
    -> TileCollection
    -> Config
    -> Identifier
    -> StateT IndexGenerator (State StdGen) (TreeZipper Dungeon)
generateDungeonAndAppend zipper ts cfg ident = do
    (generatedDungeon, lowerStairsPosition) <- generateDungeon ts cfg ident
    upperStairsPosition <- lift $ newStairsPosition ts $ getFocused zipper
    let (newUpperDungeon, newLowerDungeon) =
            addAscendingAndDescendingStiars
                (StairsPair upperStairsPosition lowerStairsPosition)
                (getFocused zipper, generatedDungeon)
        upperWithStairs =
            newUpperDungeon & cellMap . upperAt upperStairsPosition ?~
            downStairsId cfg
        newZipper =
            appendNode newLowerDungeon $ modify (const upperWithStairs) zipper
        zipperFocusingNext =
            expectJust "unreachable." (goDownBy (== newLowerDungeon) newZipper)
    return zipperFocusingNext

newStairsPosition :: TileCollection -> Dungeon -> State StdGen Coord
newStairsPosition ts d = do
    index <- randomRST (0, length candidates - 1)
    return $ candidates !! index
  where
    candidates = stairsPositionCandidates ts d

generateDungeon ::
       TileCollection
    -> Config
    -> Identifier
    -> StateT IndexGenerator (State StdGen) (Dungeon, Coord)
generateDungeon tc cfg ident = do
    (tiles, enterPosition) <-
        generateDungeonAccum [] tc initialMap (V2 0 0) cfg (getMaxRooms cfg)
    let d =
            dungeon
                (addWallsAndEdges cfg tiles & upperAt enterPosition ?~
                 (getTileFilePath cfg, upStairsIndex))
                ident
    return (d, enterPosition)
  where
    initialMap = C.cellMap $ listArray mapRange $ repeat $ initialTile cfg
    mapRange = (V2 0 0, getMapSize cfg - V2 1 1)

generateDungeonAccum ::
       [Room]
    -> TileCollection
    -> CellMap
    -> Coord
    -> Config
    -> Int
    -> StateT IndexGenerator (State StdGen) (CellMap, V2 Int)
generateDungeonAccum _ _ tileMap playerPos _ 0 = return (tileMap, playerPos)
generateDungeonAccum acc tc tileMap playerPos cfg rooms = do
    roomWidth <- lift $ randomRST (getRoomMinSize cfg, getRoomMaxSize cfg)
    roomHeight <- lift $ randomRST (getRoomMinSize cfg, getRoomMaxSize cfg)
    x <- lift $ randomRST (0, width - roomWidth - 1)
    y <- lift $ randomRST (0, height - roomHeight - 1)
    let room = roomFromWidthHeight (V2 x y) (V2 roomWidth roomHeight)
        appendRoom =
            if null acc
                then createRoom room tileMap
                else tunnelBetween (center room) (center $ head acc) $
                     createRoom room tileMap
    mapWithNewEnemies <- placeEnemies tc appendRoom room maxMonstersPerRoom
    mapWithItems <- lift $ placeItems mapWithNewEnemies tc room maxItemsPerRoom
    let usable = not $ any (roomOverlaps room) acc
        (newMap, newAcc, newPlayerPos) =
            if usable
                then (mapWithItems, room : acc, center room)
                else (tileMap, acc, playerPos)
    generateDungeonAccum newAcc tc newMap newPlayerPos cfg (rooms - 1)
  where
    V2 width height = widthAndHeight tileMap

addWallsAndEdges :: Config -> CellMap -> CellMap
addWallsAndEdges cfg = addEdgeTiles cfg . addWallTiles cfg

addEdgeTiles :: Config -> CellMap -> CellMap
addEdgeTiles cfg cm = foldl updateTileId cm ceilTiles
  where
    updateTileId cm' pos =
        cm' & ix pos . tileIdLayer . upper . _Just . _2 .~
        blobToIdAndLeftRotation (calculateBlob pos)
    blobToIdAndLeftRotation b =
        case fmap (`elemIndex` blobBase) (candidates b) of
            [Just x, _, _, _] -> x
            [_, Just x, _, _] -> head flags .|. flags !! 1 .|. x
            [_, _, Just x, _] -> flags !! 1 .|. flags !! 2 .|. x
            [_, _, _, Just x] -> flags !! 2 .|. head flags .|. x
            _                 -> error "Unmatched."
    candidates b = take 4 $ iterate ((`mod` 255) . (* 4)) b
    flags = cycle $ fmap bit [29, 30, 31]
    calculateBlob c =
        foldl
            (\acc (b, offset) ->
                 if isEmpty (c + offset) || isWall (c + offset) ||
                    isStairs (c + offset)
                     then acc .&. complement b :: Int
                     else acc)
            255
            blobOrder
    blobOrder =
        [ (bit 7 .|. bit 0 .|. bit 1, V2 0 (-1))
        , (bit 1, V2 1 (-1))
        , (bit 1 .|. bit 2 .|. bit 3, V2 1 0)
        , (bit 3, V2 1 1)
        , (bit 3 .|. bit 4 .|. bit 5, V2 0 1)
        , (bit 5, V2 (-1) 1)
        , (bit 5 .|. bit 6 .|. bit 7, V2 (-1) 0)
        , (bit 7, V2 (-1) (-1))
        ]
    -- Refer to http://www.cr31.co.uk/stagecast/wang/blob.html for the blob
    -- tile.
    blobBase = [0, 1, 5, 7, 17, 21, 23, 29, 31, 85, 87, 95, 119, 127, 255]
    ceilTiles = filter isCeil allCoordsInMap
    isCeil = (Just (Just (getTileFilePath cfg, ceilTileIndex)) ==) . upperTileAt
    isEmpty = (== Just Nothing) . upperTileAt
    isWall = tileIdSatisfies isWallId
    isStairs = tileIdSatisfies isStairsId
    tileIdSatisfies cond = maybe False (maybe False (cond . snd)) . upperTileAt
    allCoordsInMap = [V2 x y | x <- [0 .. width - 1], y <- [0 .. height - 1]]
    upperTileAt c = cm ^? ix c . tileIdLayer . upper
    V2 width height = widthAndHeight cm

addWallTiles :: Config -> CellMap -> CellMap
addWallTiles cfg cm = foldl changeTile cm wallCoords
  where
    changeTile cm' pos = cm' & ix pos . tileIdLayer . upper .~ changeTo pos
    changeTo c
        | isLeftEmpty c && isRightEmpty c =
            Just (getTileFilePath cfg, edgeWallIndex)
        | isLeftEmpty c = Just (getTileFilePath cfg, leftWallIndex)
        | isRightEmpty c = Just (getTileFilePath cfg, rightWallIndex)
        | otherwise = Just (getTileFilePath cfg, centerWallIndex)
    isRightEmpty c@(V2 x _) = x + 1 < width && isEmptyTile (c + V2 1 0)
    isLeftEmpty c@(V2 x _) = x - 1 >= 0 && isEmptyTile (c + V2 (-1) 0)
    wallCoords =
        subtract (V2 0 1) <$> filterAll [isAboveTileCeil, isEmptyTile] coords
    isAboveTileCeil =
        (Just ceilTileIndex ==) . fmap snd . upperTileAt . subtract (V2 0 1)
    isEmptyTile = isNothing . upperTileAt
    upperTileAt = (fmap (view (tileIdLayer . upper)) cm !)
    coords = [V2 x y | x <- [0 .. width - 1], y <- [1 .. height - 1]] -- Exclude the first line otherwise an index out of bounds error will happen.
    V2 width height = widthAndHeight cm

createRoom :: Room -> CellMap -> CellMap
createRoom room = flip (foldl removeTileAt) coords
  where
    removeTileAt cm x = cm & ix x . tileIdLayer . upper .~ Nothing
    coords =
        [V2 x y | x <- [x1 room .. x2 room - 1], y <- [y1 room .. y2 room - 1]]

tunnelBetween :: Coord -> Coord -> CellMap -> CellMap
tunnelBetween start end d = createRoom path1 $ createRoom path2 d
  where
    path1 = roomFromTwoPositionInclusive start corner
    path2 = roomFromTwoPositionInclusive corner end
    corner = V2 (start ^. _x) (end ^. _y)

placeEnemies ::
       TileCollection
    -> CellMap
    -> Room
    -> Int
    -> StateT IndexGenerator (State StdGen) CellMap
placeEnemies tc before r n = foldlM foldStep before [1 .. n]
  where
    foldStep cm _ = do
        x <- lift $ randomRST (x1 r, x2 r - 1)
        y <- lift $ randomRST (y1 r, y2 r - 1)
        enemy <- newMonster
        return $ fromRight cm $ flip execStateT cm $
            locateActorAt tc enemy (V2 x y)

placeItems :: CellMap -> TileCollection -> Room -> Int -> State StdGen CellMap
placeItems before tc r n = foldlM foldStep before [1 .. n]
  where
    foldStep cm _ = do
        x <- randomRST (x1 r, x2 r - 1)
        y <- randomRST (y1 r, y2 r - 1)
        newItem <- choiceST items
        return $ fromRight cm $ flip execStateT cm $
            locateItemAt tc newItem (V2 x y)
    items =
        [ liftUnion herb
        , liftUnion sampleBook
        , liftUnion sword
        , liftUnion woodenArmor
        ]

newMonster :: StateT IndexGenerator (State StdGen) Actor
newMonster = do
    r <- lift randomST :: StateT IndexGenerator (State StdGen) Float
    hoist generalize $
        if r < 0.8
            then orc
            else troll

initialTile :: Config -> TileIdLayer
initialTile cfg = TileIdLayer u l
  where
    u = Just (getTileFilePath cfg, ceilTileIndex)
    l = Just (getTileFilePath cfg, floorTileIndex)

isStairsId :: TileIndex -> Bool
isStairsId = (`elem` [downStairsIndex, upStairsIndex])

isWallId :: TileIndex -> Bool
isWallId =
    (`elem` [leftWallIndex, centerWallIndex, rightWallIndex, edgeWallIndex])

floorTileIndex :: TileIndex
floorTileIndex = 0

ceilTileIndex :: TileIndex
ceilTileIndex = 14

leftWallIndex :: TileIndex
leftWallIndex = 15

centerWallIndex :: TileIndex
centerWallIndex = 16

rightWallIndex :: TileIndex
rightWallIndex = 17

edgeWallIndex :: TileIndex
edgeWallIndex = 18

downStairsId :: Config -> TileId
downStairsId cfg = (getTileFilePath cfg, downStairsIndex)

downStairsIndex :: TileIndex
downStairsIndex = 22

upStairsIndex :: TileIndex
upStairsIndex = 23

maxMonstersPerRoom :: Int
maxMonstersPerRoom = 1

maxItemsPerRoom :: Int
maxItemsPerRoom = 2
