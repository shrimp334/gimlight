{-# LANGUAGE ScopedTypeVariables #-}

module Gimlight.ActorSpec
    ( spec
    ) where

import           Control.Monad.State         (evalState)
import           Data.Maybe                  (fromJust)
import           Data.OpenUnion              (liftUnion)
import           Gimlight.Actor              (Actor, equip, getArmor, getItems,
                                              getWeapon, player)
import qualified Gimlight.Actor              as A
import           Gimlight.IndexGenerator     (generator)
import           Gimlight.Item               (getName)
import qualified Gimlight.Item.Armor         as Armor
import           Gimlight.Item.Defined       (goldenArmor, hammer, sword,
                                              woodenArmor)
import qualified Gimlight.Item.Weapon        as W
import qualified Gimlight.Localization.Texts as T
import           Test.Hspec                  (Spec, context, describe, it,
                                              shouldBe)

spec :: Spec
spec =
    describe "equip" $ do
        testEquipWeapon
        testChangeWeapon
        testEquipArmor
        testChangeArmor

testEquipWeapon :: Spec
testEquipWeapon =
    context "When the actor does not equip anything." $ do
        it "equips a sword." $
            fmap getName (getWeapon after) `shouldBe` Just T.sword
        it "increases the power." $
            A.getPower after `shouldBe` A.getPower before + W.getPower sword
        it "removes the sword from the inventory" $ getItems after `shouldBe` []
  where
    after = fromJust $ equip (liftUnion sword) before
    before = base

testChangeWeapon :: Spec
testChangeWeapon =
    context "When the actor already equips a weapon." $ do
        it "equips a new weapon." $
            fmap getName (getWeapon after) `shouldBe` Just T.hammer
        it "changes the power." $
            A.getPower after `shouldBe` A.getPower before - W.getPower sword +
            W.getPower hammer
        it "backs previously equipped weapon to the inventory." $
            getItems after `shouldBe` [liftUnion sword]
  where
    after = fromJust $ equip (liftUnion hammer) before
    before = fromJust $ equip (liftUnion sword) base

testEquipArmor :: Spec
testEquipArmor =
    context "When the actor does not equip an armor." $ do
        it "equips an armor." $
            fmap getName (getArmor after) `shouldBe` Just T.woodenArmor
        it "increases the defence." $
            A.getDefence after `shouldBe` A.getDefence before +
            Armor.getDefence woodenArmor
        it "removes the armor from the inventory" $ getItems after `shouldBe` []
  where
    after = fromJust $ equip (liftUnion woodenArmor) before
    before = base

testChangeArmor :: Spec
testChangeArmor =
    context "When the actor already equips an armor." $ do
        it "equips a new armor" $
            fmap getName (getArmor after) `shouldBe` Just T.goldenArmor
        it "changes the defence." $
            A.getDefence after `shouldBe` A.getDefence before -
            Armor.getDefence woodenArmor +
            Armor.getDefence goldenArmor
        it "backs previously equipped armor to the inventory." $
            getItems after `shouldBe` [liftUnion woodenArmor]
  where
    after = fromJust $ equip (liftUnion goldenArmor) before
    before = fromJust $ equip (liftUnion woodenArmor) base

base :: Actor
base = evalState player generator
