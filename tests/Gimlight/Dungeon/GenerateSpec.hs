module Gimlight.Dungeon.GenerateSpec
    ( spec
    ) where

import           Control.Lens                         (view, (^.))
import           Control.Monad.State                  (State, StateT, evalState,
                                                       evalStateT)
import           Data.Tree                            (Tree (Node))
import           Gimlight.Coord                       (Coord)
import           Gimlight.Dungeon                     (Dungeon)
import qualified Gimlight.Dungeon                     as D
import           Gimlight.Dungeon.Generate            (generateMultipleFloorsDungeon)
import           Gimlight.Dungeon.Generate.Config     (Config, config,
                                                       getTileFilePath)
import           Gimlight.Dungeon.Identifier          (Identifier (Beaeve))
import           Gimlight.Dungeon.Map.Cell            (CellMap, actorExists,
                                                       lower, tileIdLayer,
                                                       upper)
import           Gimlight.Dungeon.Map.JSONReader      (readMapFile)
import           Gimlight.Dungeon.Map.Tile            (TileCollection)
import           Gimlight.Dungeon.Map.Tile.JSONReader (readTileFileRecursive)
import           Gimlight.IndexGenerator              (IndexGenerator,
                                                       generator)
import           Gimlight.SetUp.TileFile              (tileFileForGeneration)
import           Linear.V2                            (V2 (V2))
import           System.Random                        (StdGen, mkStdGen)
import           Test.Hspec                           (Spec, describe, it,
                                                       runIO, shouldBe,
                                                       shouldSatisfy)
import           Test.Hspec.QuickCheck                (prop)

spec :: Spec
spec = do
    tc <- runIO $ readTileFileRecursive "tests/tiles/valid/"
    expected <- runIO $ readMapFile "tests/maps/generate/seed_0.json"
    let result = generateSingleMap tc cfg 0
    describe "generateMultipleFloorsDungeon" $ do
        it "fills the lower layer with the floor tile." $
            tilesOf lower result `shouldSatisfy`
            all (== Just (getTileFilePath cfg, 0))
        it "generates the upper layer." $
            tilesOf upper result `shouldBe` tilesOf upper expected
        testNoActorExistsOnUpStairs tc
  where
    tilesOf layer = fmap (view (tileIdLayer . layer))
    cfg = config 1 10 (V2 3 3) (V2 10 10) tileFileForGeneration

testNoActorExistsOnUpStairs :: TileCollection -> Spec
testNoActorExistsOnUpStairs tc =
    prop "does not locate an actor at the upstairs." $ \g ->
        let (d, c) = toDungeon g
         in not $ actorExists c (d ^. D.cellMap)
  where
    toDungeon g =
        let (Node d _, c) = tree (mkStdGen g)
         in (d, c)
    tree g =
        extractDungeonTreeAndAscendingStairsPosition g $
        generateMultipleFloorsDungeon tc cfg Beaeve
    cfg = config 1 10 (V2 3 3) (V2 10 10) tileFileForGeneration

generateSingleMap :: TileCollection -> Config -> Int -> CellMap
generateSingleMap tc cfg g =
    let Node d _ = generateMap tc cfg g
     in d

generateMap :: TileCollection -> Config -> Int -> Tree CellMap
generateMap tc cfg g = fmap (^. D.cellMap) tree
  where
    tree =
        extractDungeonTree (mkStdGen g) $
        generateMultipleFloorsDungeon tc cfg Beaeve

extractDungeonTree ::
       StdGen
    -> StateT IndexGenerator (State StdGen) (Tree Dungeon, Coord)
    -> Tree Dungeon
extractDungeonTree g = fst . extractDungeonTreeAndAscendingStairsPosition g

extractDungeonTreeAndAscendingStairsPosition ::
       StdGen
    -> StateT IndexGenerator (State StdGen) (Tree Dungeon, Coord)
    -> (Tree Dungeon, Coord)
extractDungeonTreeAndAscendingStairsPosition g =
    flip evalState g . flip evalStateT generator
