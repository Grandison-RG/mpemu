{-# LANGUAGE GADTs #-}

module MemoryModel
        ( ParentNode (..),
          ListOfParentNodes,
          appendParentNode
        )
        where

data ParentNode = ParentNode
    {  service :: String
     , nodeIndex :: Word
    }
    deriving (Show, Eq, Ord)

type ListOfParentNodes = [ParentNode]

appendParentNode :: ParentNode
                 -> ListOfParentNodes
                 -> ListOfParentNodes
appendParentNode pn []  = [pn]
appendParentNode pn (p:pns)
  | (service pn) <= (service p) = pn:p:pns
  | otherwise                   = p:(appendParentNode pn pns)
