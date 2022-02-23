module Gimlight.Item.Weapon
    ( Weapon
    , weapon
    , getPower
    ) where

import           Gimlight.Item (Item, getEffect)

newtype Weapon =
    Weapon
        { power :: Int
        }
    deriving (Show, Ord, Eq)

weapon :: Int -> Weapon
weapon = Weapon

getPower :: Item Weapon -> Int
getPower = power . getEffect
