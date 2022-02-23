module Main
    ( main
    ) where

import qualified Gimlight.Action.ConsumeSpec
import qualified Gimlight.Action.DropSpec
import qualified Gimlight.Action.MeleeSpec
import qualified Gimlight.Action.MoveSpec
import qualified Gimlight.Action.PickUpSpec
import qualified Gimlight.Action.WaitSpec
import qualified Gimlight.ActorSpec
import qualified Gimlight.Data.ArraySpec
import qualified Gimlight.Data.EitherSpec
import qualified Gimlight.Data.ListSpec
import qualified Gimlight.Data.MaybeSpec
import qualified Gimlight.Data.StringSpec
import qualified Gimlight.Dungeon.Generate.ConfigSpec
import qualified Gimlight.Dungeon.GenerateSpec
import qualified Gimlight.Dungeon.Map.JSONReaderSpec
import qualified Gimlight.Dungeon.Map.Tile.JSONReaderSpec
import qualified Gimlight.FovSpec
import qualified Gimlight.System.RandomSpec
import           Test.Hspec                               (Spec, describe,
                                                           hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Gimlight.Action.Consume" Gimlight.Action.ConsumeSpec.spec
    describe "Gimlight.Action.Drop" Gimlight.Action.DropSpec.spec
    describe "Gimlight.Action.Melee" Gimlight.Action.MeleeSpec.spec
    describe "Gimlight.Action.Move" Gimlight.Action.MoveSpec.spec
    describe "Gimlight.Action.PickUp" Gimlight.Action.PickUpSpec.spec
    describe "Gimlight.Action.Wait" Gimlight.Action.WaitSpec.spec
    describe "Gimlight.Actor" Gimlight.ActorSpec.spec
    describe "Gimlight.Data.Array" Gimlight.Data.ArraySpec.spec
    describe "Gimlight.Data.Either" Gimlight.Data.EitherSpec.spec
    describe "Gimlight.Data.List" Gimlight.Data.ListSpec.spec
    describe "Gimlight.Data.Maybe" Gimlight.Data.MaybeSpec.spec
    describe "Gimlight.Data.String" Gimlight.Data.StringSpec.spec
    describe "Gimlight.Dungeon.Generate" Gimlight.Dungeon.GenerateSpec.spec
    describe
        "Gimlight.Dungeon.Generate.Config"
        Gimlight.Dungeon.Generate.ConfigSpec.spec
    describe
        "Gimlight.Dungeon.Map.JSONReader"
        Gimlight.Dungeon.Map.JSONReaderSpec.spec
    describe
        "Gimlight.Dungeon.Map.Tile.JSONReader"
        Gimlight.Dungeon.Map.Tile.JSONReaderSpec.spec
    describe "Gimlight.Fov" Gimlight.FovSpec.spec
    describe "Gimlight.System.Random" Gimlight.System.RandomSpec.spec
