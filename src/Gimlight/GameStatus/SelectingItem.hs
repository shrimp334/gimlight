{-# LANGUAGE DeriveGeneric #-}

module Gimlight.GameStatus.SelectingItem
    ( SelectingItemHandler
    , Reason(..)
    , selectingItemHandler
    , getExploringHandler
    , getReason
    , getItems
    , getSelectingIndex
    , selectPrevItem
    , selectNextItem
    ) where

import           GHC.Generics                  (Generic)
import           Gimlight.GameStatus.Exploring (ExploringHandler)
import           Gimlight.Item                 (Item)

data Reason
    = Drop
    | Use
    deriving (Show, Ord, Eq, Generic)

data SelectingItemHandler =
    SelectingItemHandler
        { items     :: [Item]
        , selecting :: Int
        , reason    :: Reason
        , handler   :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

selectingItemHandler ::
       [Item] -> Reason -> ExploringHandler -> SelectingItemHandler
selectingItemHandler is = SelectingItemHandler is 0

getItems :: SelectingItemHandler -> [Item]
getItems SelectingItemHandler {items = is} = is

getReason :: SelectingItemHandler -> Reason
getReason = reason

getSelectingIndex :: SelectingItemHandler -> Maybe Int
getSelectingIndex SelectingItemHandler {items = is, selecting = n} =
    if null is
        then Nothing
        else Just n

getExploringHandler :: SelectingItemHandler -> ExploringHandler
getExploringHandler = handler

selectPrevItem :: SelectingItemHandler -> SelectingItemHandler
selectPrevItem sh@SelectingItemHandler {items = is, selecting = n}
    | null is = sh
    | otherwise = sh {selecting = (n - 1) `mod` length is}

selectNextItem :: SelectingItemHandler -> SelectingItemHandler
selectNextItem sh@SelectingItemHandler {items = is, selecting = n}
    | null is = sh
    | otherwise = sh {selecting = (n + 1) `mod` length is}
