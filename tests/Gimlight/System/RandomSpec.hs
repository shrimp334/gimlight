module Gimlight.System.RandomSpec
    ( spec
    ) where

import           Gimlight.System.Random (choice)
import           System.Random          (mkStdGen)
import           Test.Hspec             (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "choice" $
    it "chooses a random element from the given list." $ do
        choiceSpec 0 "Ramen"
        choiceSpec 1 "Tsukemen"
        choiceSpec 2 "Ikemen"
        choiceSpec 8 "Boku"
  where
    choiceSpec n expected =
        fst (choice ["Ramen", "Tsukemen", "Boku", "Ikemen"] (mkStdGen n)) `shouldBe`
        expected
