{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module MemoryModel
        ( {-ParentNode (..),
          ListOfParentNodes,
          appendParentNode,
          checkParentNodeByService,
          appendService -}
          Storage (..)
        )
        where

import Control.Lens

type ServiceName = String

data ChildNode = ChildNode
  { _login :: String
  , _cNodeIndex :: Int
  }
  deriving (Show, Eq, Ord)

data ParentNode = ParentNode
    { _childNodes :: [ChildNode]
     , _service :: String
     , _pNodeIndex :: Int
    }
    deriving (Show, Eq, Ord)

data Storage = Storage
  { _parentNodes :: [ParentNode]
  , _lastIndex :: Int
  , _context :: ServiceName
  }

makeLenses ''ChildNode
makeLenses ''ParentNode
makeLenses ''Storage

{-
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
-}
