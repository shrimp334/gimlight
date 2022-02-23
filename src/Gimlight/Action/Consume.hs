{-# LANGUAGE DataKinds #-}

module Gimlight.Action.Consume
    ( consumeAction
    ) where

import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad.State         (StateT (runStateT), execStateT)
import           Control.Monad.Writer        (tell)
import           Data.OpenUnion              (Union, liftUnion, reUnion,
                                              typesExhausted, (@>))
import           Gimlight.Action             (Action,
                                              ActionResult (ActionResult),
                                              ActionResultWithLog,
                                              ActionStatus (Failed, Ok, ReadingStarted))
import           Gimlight.Actor              (Actor, equip, getIdentifier,
                                              healHp, inventoryItems)
import           Gimlight.Actor.Identifier   (toName)
import           Gimlight.Data.Either        (expectRight)
import           Gimlight.Dungeon.Map.Cell   (CellMap, locateActorAt,
                                              removeActorAt)
import           Gimlight.Inventory          (removeNthItem)
import           Gimlight.Item               (Item, getEffect)
import           Gimlight.Item.Armor         (Armor)
import           Gimlight.Item.Book          (Book)
import           Gimlight.Item.Heal          (Heal, getHealAmount)
import           Gimlight.Item.SomeItem      (SomeItem, getName,
                                              isUsableManyTimes)
import           Gimlight.Item.Weapon        (Weapon)
import qualified Gimlight.Localization.Texts as T

consumeAction :: Int -> Action
consumeAction n position tc cm =
    case flip runStateT cm $ removeActorAt position of
        Right (a, ncm) -> consumeActionForActor a ncm
        _              -> error "No such actor."
  where
    consumeActionForActor actorWithItem ncm =
        case removeNthItem n (actorWithItem ^. inventoryItems) of
            (Just x, i) ->
                useItem
                    x
                    (actorWithItem & inventoryItems .~ i)
                    actorWithItem
                    ncm
            (Nothing, _) -> do
                tell [T.whatToUse]
                return $ ActionResult Failed cm []
    useItem x actorWithoutItem actorWithItem ncm =
        let actor =
                if isUsableManyTimes x
                    then actorWithItem
                    else actorWithoutItem
         in doItemEffect actor ncm x
    doItemEffect :: Actor -> CellMap -> SomeItem -> ActionResultWithLog
    doItemEffect a ncm =
        useHeal a ncm @> useBook a ncm @> useWeapon a ncm @> useArmor a ncm @>
        typesExhausted
    useHeal :: Actor -> CellMap -> Item Heal -> ActionResultWithLog
    useHeal a ncm h = do
        tell [T.healed (toName $ getIdentifier a) (getHealAmount h)]
        return $ ActionResult Ok cmAfterHealing []
      where
        amount = getHealAmount h
        cmAfterHealing =
            case flip execStateT ncm $
                 locateActorAt tc (healHp amount a) position of
                Right x -> x
                Left e  -> error $ "Failed to locate an actor." <> show e
    useBook :: Actor -> CellMap -> Item Book -> ActionResultWithLog
    useBook a ncm b =
        return $ ActionResult (ReadingStarted $ getEffect b) cmAfterReading []
      where
        cmAfterReading =
            case flip execStateT ncm $ locateActorAt tc a position of
                Right x -> x
                Left e  -> error $ "Failed to locate an actor." <> show e
    useWeapon :: Actor -> CellMap -> Item Weapon -> ActionResultWithLog
    useWeapon a ncm w = useEquipment a ncm (liftUnion w)
    useArmor :: Actor -> CellMap -> Item Armor -> ActionResultWithLog
    useArmor a ncm armor = useEquipment a ncm (liftUnion armor)
    useEquipment ::
           Actor
        -> CellMap
        -> Union '[ Item Weapon, Item Armor]
        -> ActionResultWithLog
    useEquipment a ncm w =
        case equip w a of
            Just x -> do
                let cmAfterEquipping =
                        expectRight "Failed to locate an actor." $
                        flip execStateT ncm $ locateActorAt tc x position
                tell equipLog
                return $ ActionResult Ok cmAfterEquipping []
            Nothing -> do
                tell [T.bagIsFull]
                return $ ActionResult Failed ncm []
      where
        equipLog = [T.equipped (toName $ getIdentifier a) (getName $ reUnion w)]
