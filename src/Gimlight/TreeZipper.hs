{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Gimlight.TreeZipper
    ( TreeZipper
    , treeZipper
    , goToRootAndGetTree
    , getFocused
    , modify
    , goToRoot
    , goUp
    , goDownBy
    , appendNode
    , appendTree
    ) where

import           Data.Foldable (find)
import           Data.Tree     (Tree (Node, rootLabel, subForest))
import           GHC.Generics  (Generic)

data TreeCrumb a =
    TreeCrumb a [Tree a]
    deriving (Show, Ord, Eq, Generic)

type TreeZipper a = (Tree a, [TreeCrumb a])

treeZipper :: Tree a -> TreeZipper a
treeZipper t = (t, [])

goToRootAndGetTree :: TreeZipper a -> Tree a
goToRootAndGetTree z = fst $ goToRoot z

getFocused :: TreeZipper a -> a
getFocused (t, _) = rootLabel t

modify :: (a -> a) -> TreeZipper a -> TreeZipper a
modify f (n@Node {rootLabel = r}, bs) = (n {rootLabel = f r}, bs)

goToRoot :: TreeZipper a -> TreeZipper a
goToRoot z = maybe z goToRoot (goUp z)

goUp :: TreeZipper a -> Maybe (TreeZipper a)
goUp (Node {rootLabel = r, subForest = fs}, TreeCrumb newRootLabel newSubForest:bs) =
    Just
        ( Node {rootLabel = newRootLabel, subForest = Node r fs : newSubForest}
        , bs)
goUp (_, []) = Nothing

goDownBy :: (a -> Bool) -> TreeZipper a -> Maybe (TreeZipper a)
goDownBy f (Node {rootLabel = r, subForest = ts}, bs) =
    (, TreeCrumb r ts : bs) <$> find (f . rootLabel) ts

appendNode :: a -> TreeZipper a -> TreeZipper a
appendNode n (z@Node {subForest = ts}, bs) = (z {subForest = newNode : ts}, bs)
  where
    newNode = Node {rootLabel = n, subForest = []}

appendTree :: Tree a -> TreeZipper a -> TreeZipper a
appendTree tree (z@Node {subForest = ts}, bs) = (z {subForest = tree : ts}, bs)
