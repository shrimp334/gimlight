{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Gimlight.GameStatus
    ( GameStatus(..)
    , newGameStatus
    ) where

import           Control.Lens                          ()
import           Control.Monad.State                   (StateT (runStateT),
                                                        evalState, evalStateT)
import           Data.List.NonEmpty                    (fromList)
import           Data.Tree                             (Tree (Node, rootLabel, subForest))
import           GHC.Generics                          (Generic)
import           Gimlight.Data.Maybe                   (expectJust)
import           Gimlight.Dungeon                      (addAscendingAndDescendingStiars,
                                                        addDescendingStairs)
import           Gimlight.Dungeon.Init                 (initDungeon)
import           Gimlight.Dungeon.Map.Tile.JSONReader  (readTileFileRecursive)
import           Gimlight.Dungeon.Predefined.BatsCave  (batsDungeon)
import           Gimlight.Dungeon.Predefined.GlobalMap (globalMap)
import           Gimlight.Dungeon.Stairs               (StairsPair (StairsPair))
import           Gimlight.GameStatus.Exploring         (ExploringHandler,
                                                        exploringHandler)
import           Gimlight.GameStatus.ReadingBook       (ReadingBookHandler)
import           Gimlight.GameStatus.Scene             (SceneHandler,
                                                        sceneHandler,
                                                        withoutSpeaker)
import           Gimlight.GameStatus.SelectingItem     (SelectingItemHandler)
import           Gimlight.GameStatus.Talking           (TalkingHandler)
import           Gimlight.IndexGenerator               (generator)
import qualified Gimlight.Localization.Texts.Scene     as T (title1, title2,
                                                             welcome)
import qualified Gimlight.Log                          as L
import           Gimlight.Quest                        (questCollection)
import           Gimlight.TreeZipper                   (appendTree, goDownBy,
                                                        treeZipper)
import           Linear.V2                             (V2 (V2))
import           System.Random                         (getStdGen)

data GameStatus
    = Exploring ExploringHandler
    | Talking TalkingHandler
    | Scene SceneHandler
    | SelectingItem SelectingItemHandler
    | ReadingBook ReadingBookHandler
    | Title
    | GameOver
    | SelectingLocale
    deriving (Show, Ord, Eq, Generic)

newGameStatus :: IO GameStatus
newGameStatus = do
    g <- getStdGen
    tc <- readTileFileRecursive "tiles/"
    gm <- globalMap
    (beaeve, ig) <- runStateT (initDungeon tc) generator
    let (bats, stairsPosition) =
            flip evalState g $ evalStateT (batsDungeon tc) ig
        (gmWithBatsStairs, batsRootMapWithParentMap) =
            addAscendingAndDescendingStiars
                (StairsPair (V2 9 6) stairsPosition)
                (gm, rootLabel bats)
        batsTreeWithParentMap = bats {rootLabel = batsRootMapWithParentMap}
        (initGm, beaeveWithParentMap) =
            addDescendingStairs
                (StairsPair (V2 3 16) (V2 7 0))
                (gmWithBatsStairs, beaeve)
        dungeonTree =
            Node
                { rootLabel = initGm
                , subForest =
                      [Node {rootLabel = beaeveWithParentMap, subForest = []}]
                }
        zipper = appendTree batsTreeWithParentMap $ treeZipper dungeonTree
        initZipper =
            expectJust "Unreachable." (goDownBy (== beaeveWithParentMap) zipper)
        initExploring =
            exploringHandler
                initZipper
                (foldr (L.addMessage . L.message) L.emptyLog [T.welcome])
                questCollection
                tc
    return . Scene $
        sceneHandler
            "images/game_opening.png"
            (fromList [withoutSpeaker T.title1, withoutSpeaker T.title2])
            initExploring
