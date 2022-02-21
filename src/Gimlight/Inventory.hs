{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Gimlight.Inventory
    ( Inventory
    , inventory
    , addItem
    , getItems
    , removeNthItem
    ) where

import           Control.Lens  (makeLenses, (%~), (&), (.~), (^.))
import           GHC.Generics  (Generic)
import           Gimlight.Item (Item)

data Inventory =
    Inventory
        { _items    :: [Item]
        , _maxItems :: Int
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Inventory

inventory :: Int -> Inventory
inventory n = Inventory {_items = [], _maxItems = n}

addItem :: Item -> Inventory -> Maybe Inventory
addItem item inv =
    if length (inv ^. items) == inv ^. maxItems
        then Nothing
        else Just $ inv & items %~ (:) item

getItems :: Inventory -> [Item]
getItems inv = inv ^. items

removeNthItem :: Int -> Inventory -> (Maybe Item, Inventory)
removeNthItem n e =
    if n < e ^. maxItems
        then (Just removedItem, e & items .~ newItems)
        else (Nothing, e)
  where
    newItems = take n (e ^. items) ++ drop (n + 1) (e ^. items)
    removedItem = (e ^. items) !! n
