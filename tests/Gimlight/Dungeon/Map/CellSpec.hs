{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Gimlight.Dungeon.Map.CellSpec
    ( emptyTile
    , emptyCellMap
    , locateItemsActors
    , locateItemsActorsST
    ) where

import           Control.Monad.State           (MonadState (get, put), StateT,
                                                execStateT)
import           Data.Array                    (listArray)
import           Data.Either.Combinators       (fromRight')
import           Data.OpenUnion                (Union, typesExhausted, (@>))
import           Gimlight.Actor                (Actor)
import           Gimlight.Coord                (Coord)
import           Gimlight.Dungeon.Map.Cell     (CellMap, Error,
                                                TileIdLayer (TileIdLayer),
                                                cellMap, locateActorAt,
                                                locateItemAt)
import           Gimlight.Dungeon.Map.TileSpec (mockTileCollection)
import           Gimlight.Item.SomeItem        (SomeItem)
import           Linear                        (V2 (V2))

emptyTile :: TileIdLayer
emptyTile = TileIdLayer Nothing Nothing

emptyCellMap :: V2 Int -> CellMap
emptyCellMap widthHeight =
    cellMap $ listArray (V2 0 0, widthHeight - V2 1 1) $ repeat emptyTile

locateItemsActors :: [(Coord, Union '[ Actor, SomeItem])] -> CellMap -> CellMap
locateItemsActors xs cm = foldl helper cm xs
  where
    helper ncm (pos, x) =
        fromRight' $
        flip execStateT ncm $
        (itemFunc pos @> actorFunc pos @> typesExhausted) x
    actorFunc = apply locateActorAt
    itemFunc = apply locateItemAt
    apply f pos x = f mockTileCollection x pos

-- To conform to the types of `locateItemAt` and `locateActorAt`
locateItemsActorsST ::
       [(Coord, Union '[ Actor, SomeItem])] -> StateT CellMap (Either Error) ()
locateItemsActorsST xs = do
    cm <- get
    let cm' = locateItemsActors xs cm
    put cm'
