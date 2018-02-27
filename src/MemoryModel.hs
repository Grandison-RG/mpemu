{-# LANGUAGE GADTs #-}

module MemoryModel
        ( Node (..),
          ParentNode (..),
          State (..))
        where

import Data.List ( sort )

data Node a = Node a deriving (Show, Eq, Ord)

instance Functor Node where
  fmap f (Node x) = Node (f x)

data ParentNode a = ParentNode a deriving (Show, Eq, Ord)

instance Functor ParentNode where
  fmap f (ParentNode x) = ParentNode (f x)

instance Monoid a => Monoid (Node a) where
  mempty = Node mempty
  (Node n) `mappend` (Node m) = Node (n `mappend` m)

instance Monoid a => Monoid (ParentNode a) where
  mempty = ParentNode mempty
  (ParentNode n) `mappend` (ParentNode m) = ParentNode (n `mappend` m)


data State a where
  ListOfParentNodes :: [ParentNode a] -> State a
  ListOfNodes :: [Node a] -> State a

sortListOfParentNode :: Ord a => [ParentNode a] -> [ParentNode a]
sortListOfParentNode = sort

{- addNewParentNode :: (Ord a, Monoid a) => ParentNode a -> State a -> State a
addNewParentNode (ParentNode pn) state = -}
