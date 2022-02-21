{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Actor.Status.Hp
    ( Hp
    , hp
    , getHp
    , getMaxHp
    , healHp
    , receiveDamage
    ) where

import           GHC.Generics        (Generic)
import           Gimlight.Data.Maybe (expectJust)

data Hp =
    Hp
        { currentHp :: Int
        , maxHp     :: Int
        }
    deriving (Show, Ord, Eq, Generic)

hp :: Int -> Hp
hp h = expectJust "The initial HP value must be positive." (hpOrFail h)

getHp :: Hp -> Int
getHp = currentHp

getMaxHp :: Hp -> Int
getMaxHp = maxHp

healHp :: Int -> Hp -> Hp
healHp amount Hp {currentHp = c, maxHp = m} =
    Hp {currentHp = min m $ c + amount, maxHp = m}

receiveDamage :: Int -> Hp -> Maybe Hp
receiveDamage damage Hp {currentHp = c, maxHp = m} =
    if newHp > 0
        then Just $ Hp {currentHp = newHp, maxHp = m}
        else Nothing
  where
    newHp = c - damage

hpOrFail :: Int -> Maybe Hp
hpOrFail h
    | h > 0 = Just $ Hp h h
    | otherwise = Nothing
