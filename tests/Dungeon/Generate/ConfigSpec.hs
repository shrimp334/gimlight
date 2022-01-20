module Dungeon.Generate.ConfigSpec
    ( spec
    ) where

import           Control.Exception       (evaluate)
import           Dungeon.Generate.Config (config, maxRoomMustBePositive,
                                          numOfFloorsMustBePositive,
                                          roomMaxSizeIsLargerThanRoomHeight,
                                          roomMaxSizeIsLargerThanRoomWidth,
                                          roomMinIsLargerThanRoomMax,
                                          roomMinSizeMustBePositive)
import           Generator               (generateNonPositive,
                                          generatePositiveBigSmallNumbers)
import           Linear.V2               (V2 (V2))
import           Test.Hspec              (Spec, describe, errorCall, it,
                                          shouldThrow)
import           Test.QuickCheck         (forAll)

spec :: Spec
spec =
    describe "config" $ do
        testPanicIfNumOfFloorsIsNotPositive
        testPanicIfMaxRoomsIsNotPositive
        testPanicIfRoomMinSizeIsNotPositive
        testPanicIfRoomMinSizeIsLargerThanRoomMaxSize
        testPanicIfRoomMinSizeIsLargerThanMapWidth
        testPanicIfRoomMinSizeIsLargerThanMapHeight

testPanicIfNumOfFloorsIsNotPositive :: Spec
testPanicIfNumOfFloorsIsNotPositive =
    it "panics if the given number of floors is not positive." $
    forAll generateNonPositive $ \n ->
        evaluate (config n 1 1 1 (V2 100 100)) `shouldThrow`
        errorCall numOfFloorsMustBePositive

testPanicIfMaxRoomsIsNotPositive :: Spec
testPanicIfMaxRoomsIsNotPositive =
    it "panics if the given number of maximum rooms is not positive." $
    forAll generateNonPositive $ \n ->
        evaluate (config 1 n 1 1 (V2 100 100)) `shouldThrow`
        errorCall maxRoomMustBePositive

testPanicIfRoomMinSizeIsNotPositive :: Spec
testPanicIfRoomMinSizeIsNotPositive =
    it "panics if the given number of minimum room size is not positive." $
    forAll generateNonPositive $ \n ->
        evaluate (config 1 1 n 1 (V2 100 100)) `shouldThrow`
        errorCall roomMinSizeMustBePositive

testPanicIfRoomMinSizeIsLargerThanRoomMaxSize :: Spec
testPanicIfRoomMinSizeIsLargerThanRoomMaxSize =
    it "panics if the given room minimum size is larger than the room maximum size" $
    forAll generatePositiveBigSmallNumbers $ \(rmin, rmax) ->
        evaluate (config 1 1 rmin rmax (V2 100 100)) `shouldThrow`
        errorCall (roomMinIsLargerThanRoomMax rmin rmax)

testPanicIfRoomMinSizeIsLargerThanMapWidth :: Spec
testPanicIfRoomMinSizeIsLargerThanMapWidth =
    it "panics if the given room minimum size is larger than the room width" $
    evaluate (config 1 1 1 rmax (V2 width 6)) `shouldThrow`
    errorCall (roomMaxSizeIsLargerThanRoomWidth rmax width)
  where
    rmax = 5
    width = 4

testPanicIfRoomMinSizeIsLargerThanMapHeight :: Spec
testPanicIfRoomMinSizeIsLargerThanMapHeight =
    it "panics if the given room minimum size is larger than the room height" $
    evaluate (config 1 1 1 rmax (V2 6 height)) `shouldThrow`
    errorCall (roomMaxSizeIsLargerThanRoomHeight rmax height)
  where
    rmax = 5
    height = 4
