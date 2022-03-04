module Gimlight.Dungeon.GenerateSpec
    ( spec
    ) where

import           Control.Lens                         (Ixed (ix), view, (^.),
                                                       (^?))
import           Control.Monad.State                  (State, StateT, evalState,
                                                       evalStateT)
import           Data.Tree                            (Tree (Node))
import           Gimlight.Coord                       (Coord)
import           Gimlight.Dungeon                     (Dungeon)
import qualified Gimlight.Dungeon                     as D
import           Gimlight.Dungeon.Generate            (generateMultipleFloorsDungeon,
                                                       upStairsIndex)
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
    let result = generateSingleMap 0 tc
    describe "generateMultipleFloorsDungeon" $ do
        it "fills the lower layer with the floor tile." $ tilesOf lower result `shouldSatisfy`
            all (== Just (getTileFilePath cfg, 0))
        it "generates the upper layer." $ tilesOf upper result `shouldBe`
            tilesOf upper expected
        testNoActorExistsOnUpStairs tc
        testUpstairsIsOnCorrectPosition tc
  where
    tilesOf layer = fmap (view (tileIdLayer . layer))

testUpstairsIsOnCorrectPosition :: TileCollection -> Spec
testUpstairsIsOnCorrectPosition tc =
    prop "upstairs appears on the correct position." $ \g ->
        let (d, c) = generateDungeonAndUpStairsPosition g tc
         in d ^? D.cellMap . ix c . tileIdLayer . upper `shouldBe`
            Just (Just (tileFileForGeneration, upStairsIndex))

testNoActorExistsOnUpStairs :: TileCollection -> Spec
testNoActorExistsOnUpStairs tc =
    prop "does not locate an actor at the upstairs." $ \g ->
        let (d, c) = generateDungeonAndUpStairsPosition g tc
         in not $ actorExists c (d ^. D.cellMap)

generateSingleMap :: Int -> TileCollection -> CellMap
generateSingleMap g =
    view D.cellMap . fst . generateDungeonAndUpStairsPosition g

generateDungeonAndUpStairsPosition :: Int -> TileCollection -> (Dungeon, Coord)
generateDungeonAndUpStairsPosition g tc = (d, c)
  where
    (Node d _, c) = generateDungeonTreeAndUpstairsPosition g tc

generateDungeonTreeAndUpstairsPosition ::
       Int -> TileCollection -> (Tree Dungeon, Coord)
generateDungeonTreeAndUpstairsPosition g tc =
    extractDungeonTreeAndAscendingStairsPosition (mkStdGen g) $
    generateMultipleFloorsDungeon tc cfg Beaeve

extractDungeonTreeAndAscendingStairsPosition ::
       StdGen
    -> StateT IndexGenerator (State StdGen) (Tree Dungeon, Coord)
    -> (Tree Dungeon, Coord)
extractDungeonTreeAndAscendingStairsPosition g =
    flip evalState g . flip evalStateT generator

cfg :: Config
cfg = config 1 10 (V2 3 3) (V2 10 10) tileFileForGeneration
