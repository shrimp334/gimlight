{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Item.Heal
    ( HealHandler
    , healHandler
    , getHealAmount
    ) where

import           GHC.Generics (Generic)

newtype HealHandler =
    HealHandler
        { amount :: Int
        }
    deriving (Show, Ord, Eq, Generic)

healHandler :: Int -> HealHandler
healHandler = HealHandler

getHealAmount :: HealHandler -> Int
getHealAmount (HealHandler a) = a
