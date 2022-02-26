{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gimlight.Actor
    ( Actor
    , player
    , getIndex
    , getIdentifier
    , getLevel
    , getCurrentExperiencePoint
    , getExperiencePointForNextLevel
    , getHp
    , getMaxHp
    , getPower
    , getDefence
    , getTalkingPart
    , monster
    , isPlayer
    , isMonster
    , isFriendlyNpc
    , ActorKind(FriendlyNpc)
    , attackFromTo
    , actor
    , pathToDestination
    , standingImagePath
    , walkingImagePath
    , healHp
    , inventoryItems
    , getItems
    , target
    , equip
    , getWeapon
    , getArmor
    ) where

import           Control.Applicative              ((<|>))
import           Control.Lens                     (makeLenses, (%~), (&), (.~),
                                                   (?~), (^.))
import           Control.Monad.State              (State)
import           Control.Monad.Writer             (MonadWriter (writer), Writer)
import           Data.OpenUnion                   (Union, liftUnion, restrict)
import           Data.Text                        (Text)
import           GHC.Generics                     (Generic)
import           Gimlight.Actor.Identifier        (Identifier, toName)
import qualified Gimlight.Actor.Identifier        as Identifier
import           Gimlight.Actor.Status            (Status)
import qualified Gimlight.Actor.Status            as S
import           Gimlight.Actor.Status.Hp         (hp)
import           Gimlight.Coord                   (Coord)
import           Gimlight.GameStatus.Talking.Part (TalkingPart)
import           Gimlight.IndexGenerator          (Index, IndexGenerator,
                                                   generate)
import           Gimlight.Inventory               (Inventory, addItem,
                                                   inventory, maxSlot)
import qualified Gimlight.Inventory               as I
import           Gimlight.Item                    (Item)
import           Gimlight.Item.Armor              (Armor)
import qualified Gimlight.Item.Armor              as A
import           Gimlight.Item.SomeItem           (SomeItem)
import           Gimlight.Item.Weapon             (Weapon)
import qualified Gimlight.Item.Weapon             as W
import           Gimlight.Log                     (MessageLog)

data ActorKind
    = Player
    | FriendlyNpc
    | Monster
    deriving (Show, Ord, Eq, Generic)

data Actor =
    Actor
        { _index             :: Index
        , _identifier        :: Identifier
        , _status            :: Status
        , _pathToDestination :: [Coord]
        , _actorKind         :: ActorKind
        , _talk              :: Maybe TalkingPart
        , _walkingImagePath  :: Text
        , _standingImagePath :: Text
        , _inventoryItems    :: Inventory
        , _target            :: Maybe Index
        , _weapon            :: Maybe (Item Weapon)
        , _armor             :: Maybe (Item Armor)
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Actor

actor ::
       Identifier
    -> Status
    -> ActorKind
    -> Maybe TalkingPart
    -> Text
    -> Text
    -> State IndexGenerator Actor
actor id' st ak talkMessage' walkingImagePath' standingImagePath' = do
    idx <- generate
    return
        Actor
            { _index = idx
            , _identifier = id'
            , _status = st
            , _pathToDestination = []
            , _talk = talkMessage'
            , _walkingImagePath = walkingImagePath'
            , _standingImagePath = standingImagePath'
            , _actorKind = ak
            , _inventoryItems = inventory maxSlot
            , _target = Nothing
            , _weapon = Nothing
            , _armor = Nothing
            }

monster :: Identifier -> Status -> Text -> State IndexGenerator Actor
monster name' st walking =
    actor name' st Monster Nothing walking "images/sample_standing_picture.png"

player :: State IndexGenerator Actor
player =
    actor
        Identifier.Player
        st
        Player
        Nothing
        "images/player.png"
        "images/sample_standing_picture.png"
  where
    st = S.status (hp 30) 5 2

isPlayer :: Actor -> Bool
isPlayer e = e ^. actorKind == Player

isMonster :: Actor -> Bool
isMonster e = e ^. actorKind == Monster

isFriendlyNpc :: Actor -> Bool
isFriendlyNpc e = e ^. actorKind == FriendlyNpc

getIdentifier :: Actor -> Identifier
getIdentifier a = a ^. identifier

getHp :: Actor -> Int
getHp e = S.getHp $ e ^. status

getMaxHp :: Actor -> Int
getMaxHp e = S.getMaxHp $ e ^. status

getTalkingPart :: Actor -> Maybe TalkingPart
getTalkingPart a = a ^. talk

attackFromTo :: Actor -> Actor -> (Writer MessageLog) (Actor, Maybe Actor)
attackFromTo attacker defender = writer ((newAttacker, newDefender), msg)
  where
    (newAttackerStatus, newDefenderStatus, msgFunc) =
        S.attackFromTo (attacker ^. status) (defender ^. status)
    newAttacker = attacker & status .~ newAttackerStatus
    newDefender = (\x -> defender & status .~ x) <$> newDefenderStatus
    msg =
        msgFunc
            (toName $ getIdentifier attacker)
            (toName $ getIdentifier defender)

healHp :: Int -> Actor -> Actor
healHp amount a = a & status %~ S.healHp amount

getIndex :: Actor -> Index
getIndex a = a ^. index

getLevel :: Actor -> Int
getLevel a = S.getLevel $ a ^. status

getCurrentExperiencePoint :: Actor -> Int
getCurrentExperiencePoint a = S.getCurrentExperiencePoint $ a ^. status

getExperiencePointForNextLevel :: Actor -> Int
getExperiencePointForNextLevel a =
    S.getExperiencePointForNextLevel $ a ^. status

getPower :: Actor -> Int
getPower a = S.getPower (a ^. status) + maybe 0 W.getPower (a ^. weapon)

getDefence :: Actor -> Int
getDefence a = S.getDefence (a ^. status) + maybe 0 A.getDefence (a ^. armor)

getItems :: Actor -> [SomeItem]
getItems a = I.getItems $ a ^. inventoryItems

getWeapon :: Actor -> Maybe (Item Weapon)
getWeapon a = a ^. weapon

getArmor :: Actor -> Maybe (Item Armor)
getArmor a = a ^. armor

equip :: Union '[ Item Weapon, Item Armor] -> Actor -> Maybe Actor
equip equipment a = tryEquipWeapon <|> tryEquipArmor
  where
    tryEquipWeapon =
        case (restrict equipment, inventoryWith weapon) of
            (Right x, Just inv) ->
                Just $ a & weapon ?~ x & inventoryItems .~ inv
            _ -> Nothing
    tryEquipArmor =
        case (restrict equipment, inventoryWith armor) of
            (Right x, Just inv) -> Just $ a & armor ?~ x & inventoryItems .~ inv
            _                   -> Nothing
    inventoryWith lens =
        case a ^. lens of
            Just x  -> addItem (liftUnion x) (a ^. inventoryItems)
            Nothing -> Just $ a ^. inventoryItems
