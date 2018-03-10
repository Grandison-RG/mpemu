{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module MemoryModel
  ( Storage (..)
  , checkParentNodeByService
  , appendService
  )
where

import Control.Lens ( makeLenses
                    , (&)
                    , (^.)
                    , (%~)
                    )

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

appendParentNode :: ParentNode
                 -> [ParentNode]
                 -> [ParentNode]
appendParentNode pn []  = [pn]
appendParentNode pn (p:pns)
  | (_service pn) < (_service p)  = pn:p:pns
  | (_service pn) == (_service p) = p:pns
  | otherwise                   = p:(appendParentNode pn pns)

appendService :: ServiceName
              -> Storage
              -> Storage
appendService name st = (st & parentNodes %~ appendParentNode newNode)
                        & lastIndex %~ (+1)
                        where newNode = ParentNode
                                        { _childNodes = []
                                        , _service = name
                                        , _pNodeIndex = st ^. lastIndex
                                        }

checkParentNodeByService :: ServiceName
                         -> Storage
                         -> Bool
checkParentNodeByService name st =
  any hasName $ st ^. parentNodes
  where hasName pn = pn ^. service == name
