module Gimlight.Dungeon.Generate.Config
    ( Config
    , getNumOfFloors
    , getMaxRooms
    , getRoomMinSize
    , getRoomMaxSize
    , getMapSize
    , getTileFilePath
    , config
    , numOfFloorsMustBePositive
    , maxRoomMustBePositive
    , roomMinSizeMustBePositive
    , roomMinIsLargerThanRoomMax
    , roomMaxSizeIsLargerThanRoomWidth
    , roomMaxSizeIsLargerThanRoomHeight
    ) where

import           Control.Lens (_1, _2, view)
import           Linear.V2    (V2 (V2))

data Config =
    Config
        { numOfFloors   :: Int
        , maxRooms      :: Int
        , roomSizeRange :: V2 Int
        , mapSize       :: V2 Int
        , tileFilePath  :: FilePath
        }
    deriving (Show)

getNumOfFloors :: Config -> Int
getNumOfFloors = numOfFloors

getMaxRooms :: Config -> Int
getMaxRooms = maxRooms

getRoomMinSize :: Config -> Int
getRoomMinSize = view _1 . roomSizeRange

getRoomMaxSize :: Config -> Int
getRoomMaxSize = view _2 . roomSizeRange

getMapSize :: Config -> V2 Int
getMapSize = mapSize

getTileFilePath :: Config -> FilePath
getTileFilePath = tileFilePath

config :: Int -> Int -> V2 Int -> V2 Int -> FilePath -> Config
config nf mr rs@(V2 rmin rmax) ms@(V2 width height) path
    | nf <= 0 = error numOfFloorsMustBePositive
    | mr <= 0 = error maxRoomMustBePositive
    | rmin <= 0 = error roomMinSizeMustBePositive
    | rmin > rmax = error $ roomMinIsLargerThanRoomMax rmin rmax -- No need to check if `rmax <= 0` as this ensures that `0 < rmin <= rmax`.
    | rmax > width = error $ roomMaxSizeIsLargerThanRoomWidth rmax width
    | rmax > height = error $ roomMaxSizeIsLargerThanRoomHeight rmax height -- No need to check if `width` or `height` are negative as `(width or height) >= rmax >= rmin > 0`
    | otherwise = Config nf mr rs ms path

numOfFloorsMustBePositive :: String
numOfFloorsMustBePositive = "The number of floors must be positive."

maxRoomMustBePositive :: String
maxRoomMustBePositive = "The maximum number of rooms must be positive."

roomMinSizeMustBePositive :: String
roomMinSizeMustBePositive = "The minimum room size must be positive."

roomMinIsLargerThanRoomMax :: Int -> Int -> String
roomMinIsLargerThanRoomMax rmin rmax =
    "The room minimum size " ++
    show rmin ++
    " is larger than or equal to the room maximum size " ++ show rmax ++ "."

roomMaxSizeIsLargerThanRoomWidth :: Int -> Int -> String
roomMaxSizeIsLargerThanRoomWidth rmax width =
    "The room maximum size " ++
    show rmax ++
    " is larger than or equal to the map width " ++ show width ++ "."

roomMaxSizeIsLargerThanRoomHeight :: Int -> Int -> String
roomMaxSizeIsLargerThanRoomHeight rmax height =
    "The room maximum size " ++
    show rmax ++
    " is larger than or equal to the map height " ++ show height ++ "."
