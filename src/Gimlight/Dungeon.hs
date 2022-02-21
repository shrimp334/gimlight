-- I refer to "Dungeon" in this source code as the mixed things of map and
-- actors because I could not come up with a much more proper word.  So,
-- in this code, "Dungeon" means not only dungeon but also towns, etc.
--
-- TODO: Change the word to more precise one.
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Gimlight.Dungeon
    ( Dungeon
    , dungeon
    , getIdentifier
    , stairsPositionCandidates
    , positionOnParentMap
    , cellMap
    , descendingStairs
    , addAscendingAndDescendingStiars
    , addDescendingStairs
    , ascendingStairs
    ) where

import           Control.Lens                (makeLenses, (^.))
import           Data.Array.Base             (assocs)
import           GHC.Generics                (Generic)
import           Gimlight.Coord              (Coord)
import           Gimlight.Dungeon.Identifier (Identifier)
import           Gimlight.Dungeon.Map.Cell   (CellMap, walkableFloors)
import           Gimlight.Dungeon.Map.Tile   (TileCollection)
import           Gimlight.Dungeon.Stairs     (StairsPair (StairsPair, downStairs, upStairs))

-- We do not store the positions of stairs in `CellMap` because it is
-- difficult to guarantee that there is only one upstairs on the map.
data Dungeon =
    Dungeon
        { _cellMap             :: CellMap
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

dungeon :: CellMap -> Identifier -> Dungeon
dungeon c ident =
    Dungeon
        { _cellMap = c
        , _positionOnParentMap = Nothing
        , _ascendingStairs = Nothing
        , _descendingStairs = []
        , _identifier = ident
        }

getIdentifier :: Dungeon -> Identifier
getIdentifier d = d ^. identifier

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

stairsPositionCandidates :: TileCollection -> Dungeon -> [Coord]
stairsPositionCandidates ts d =
    filter (not . isStairsOnPosition) $ walkableCoords (d ^. cellMap)
  where
    walkableCoords = map fst . filter snd . assocs . walkableFloors ts
    isStairsOnPosition c = isUpStairsPosition c || isDownStairsPosition c
    isUpStairsPosition c = (downStairs <$> d ^. ascendingStairs) == Just c
    isDownStairsPosition c = c `elem` map upStairs (d ^. descendingStairs)
