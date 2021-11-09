{-# LANGUAGE DeriveGeneric #-}

module GameStatus.ReadingBook
    ( ReadingBookHandler
    , readingBookHandler
    , getContent
    , finishReading
    ) where

import           Data.Binary          (Binary)
import           GHC.Generics         (Generic)
import           GameStatus.Exploring (ExploringHandler, processAfterPlayerTurn)
import           Localization         (MultilingualText)

data ReadingBookHandler =
    ReadingBookHandler
        { content      :: MultilingualText
        , afterReading :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary ReadingBookHandler

readingBookHandler :: MultilingualText -> ExploringHandler -> ReadingBookHandler
readingBookHandler = ReadingBookHandler

getContent :: ReadingBookHandler -> MultilingualText
getContent (ReadingBookHandler c _) = c

finishReading :: ReadingBookHandler -> Maybe ExploringHandler
finishReading (ReadingBookHandler _ h) = processAfterPlayerTurn h
