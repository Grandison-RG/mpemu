{-# LANGUAGE GADTs #-}

module MemoryModel
        ( ParentNode (..),
          ListOfParentNodes,
          appendParentNode
        )
        where

data ParentNode = ParentNode
    {  service :: String
     , nodeIndex :: Int
    }
    deriving (Show, Eq, Ord)

type ListOfParentNodes = [ParentNode]

appendParentNode :: ParentNode
                 -> ListOfParentNodes
                 -> ListOfParentNodes
appendParentNode pn []  = [pn]
appendParentNode pn (p:pns)
  | (service pn) < (service p)  = pn:p:pns
  | (service pn) == (service p) = p:pns
  | otherwise                   = p:(appendParentNode pn pns)

appendService :: String
              -> ListOfParentNodes
              -> ListOfParentNodes
appendService srv pns = let nodeIndex = length pns in
  appendParentNode (ParentNode srv nodeIndex) pns

checkParentNodeByService :: String
                         -> ListOfParentNodes
                         -> Bool
checkParentNodeByService srv [] = False
checkParentNodeByService srv (p:pns)
  | srv == (service p)          = True
  | otherwise                   = checkParentNodeByService srv pns
