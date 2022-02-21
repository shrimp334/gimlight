{-# LANGUAGE DeriveGeneric #-}

module Gimlight.GameStatus.ReadingBook
    ( ReadingBookHandler
    , readingBookHandler
    , getContent
    , finishReading
    ) where

import           GHC.Generics                  (Generic)
import           Gimlight.GameStatus.Exploring (ExploringHandler,
                                                processAfterPlayerTurn)
import           Gimlight.Localization         (MultilingualText)

data ReadingBookHandler =
    ReadingBookHandler
        { content      :: MultilingualText
        , afterReading :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

readingBookHandler :: MultilingualText -> ExploringHandler -> ReadingBookHandler
readingBookHandler = ReadingBookHandler

getContent :: ReadingBookHandler -> MultilingualText
getContent (ReadingBookHandler c _) = c

finishReading :: ReadingBookHandler -> Maybe ExploringHandler
finishReading (ReadingBookHandler _ h) = processAfterPlayerTurn h
