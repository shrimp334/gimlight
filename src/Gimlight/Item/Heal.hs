{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Item.Heal
    ( Heal
    , heal
    , getHealAmount
    ) where

import           GHC.Generics  (Generic)
import           Gimlight.Item (Item, getEffect)

newtype Heal =
    Heal
        { amount :: Int
        }
    deriving (Show, Ord, Eq, Generic)

heal :: Int -> Heal
heal = Heal

getHealAmount :: Item Heal -> Int
getHealAmount = amount . getEffect
