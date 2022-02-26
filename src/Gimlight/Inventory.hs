{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Gimlight.Inventory
    ( Inventory
    , inventory
    , addItem
    , getItems
    , removeNthItem
    , maxSlot
    ) where

import           Control.Lens           (makeLenses, (%~), (&), (.~), (^.))
import           GHC.Generics           (Generic)
import           Gimlight.Item.SomeItem (SomeItem)

data Inventory =
    Inventory
        { _items    :: [SomeItem]
        , _maxItems :: Int
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Inventory

inventory :: Int -> Inventory
inventory n = Inventory {_items = [], _maxItems = n}

addItem :: SomeItem -> Inventory -> Maybe Inventory
addItem item inv =
    if length (inv ^. items) == inv ^. maxItems
        then Nothing
        else Just $ inv & items %~ (:) item

getItems :: Inventory -> [SomeItem]
getItems inv = inv ^. items

removeNthItem :: Int -> Inventory -> (Maybe SomeItem, Inventory)
removeNthItem n e =
    if n < e ^. maxItems
        then (Just removedItem, e & items .~ newItems)
        else (Nothing, e)
  where
    newItems = take n (e ^. items) ++ drop (n + 1) (e ^. items)
    removedItem = (e ^. items) !! n

maxSlot :: Int
maxSlot = 5
