{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Dungeon.Identifier
    ( Identifier(..)
    , isTown
    ) where

import           GHC.Generics (Generic)

data Identifier
    = Beaeve
    | BatsCave
    | GlobalMap
    deriving (Show, Ord, Eq, Generic)

isTown :: Identifier -> Bool
isTown Beaeve = True
isTown _      = False
