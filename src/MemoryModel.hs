{-# LANGUAGE GADTs #-}

module MemoryModel
        ( Node (..),
          ParentNode (..),
          State (..),
          addNewParentNode
        ) 
        where

import Data.List ( sort )

data Node a = Node a deriving (Show, Eq, Ord)

instance Functor Node where
  fmap f (Node x) = Node (f x)

data ParentNode a = ParentNode a deriving (Show, Eq, Ord)

instance Functor ParentNode where
  fmap f (ParentNode x) = ParentNode (f x)

data State a where
  ListOfParentNodes :: [ParentNode a] -> State a
  ListOfNodes :: [Node a] -> State a

addNewParentNode :: ParentNode String -> State String -> State String
addNewParentNode (ParentNode pn) state = case state of
  ListOfParentNodes xs -> ListOfParentNodes . sort $ xs
  ListOfNodes xs -> case any (== "") xs of
    False -> ListOfNodes ((Node pn) : xs)
    True -> ListOfNodes $ replace (Node "") (Node pn) xs where
      refl (Node x) = x
      replace a b = map (\x -> if ("" == x) then b else x)
