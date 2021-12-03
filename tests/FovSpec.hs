module FovSpec
    ( spec
    ) where

import           Data.Array (array)
import           Fov        (calculateFov)
import           Linear.V2  (V2 (V2))
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testFovCalculationOfWidthHeight 3 3
    testFovCalculationOfWidthHeight 2 4

testFovCalculationOfWidthHeight :: Int -> Int -> Spec
testFovCalculationOfWidthHeight mapWidth mapHeight =
    describe "calculateFov" $
    it ("calculates FoV of " <>
        show mapWidth <>
        "x" <>
        show mapHeight <>
        " from the previous FoV generated by `darkFov` successfully") $
    calculatedFov `shouldBe` expectedFov
  where
    expectedFov =
        array
            (V2 0 0, V2 (mapWidth - 1) (mapHeight - 1))
            [ (V2 x y, True)
            | x <- [0 .. mapWidth - 1]
            , y <- [0 .. mapHeight - 1]
            ]
    calculatedFov = calculateFov playerPosition transparentMap
    transparentMap =
        array
            (V2 0 0, V2 (mapWidth - 1) (mapHeight - 1))
            [ (V2 x y, True)
            | x <- [0 .. mapWidth - 1]
            , y <- [0 .. mapHeight - 1]
            ]
    playerPosition = V2 1 1
