{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module MemoryModel
  ( Storage (..)
  , checkParentNodeByService
  , appendService
  , addLoginCurrent
  , parentNodes
  , childNodes
  , login
  , cNodeIndex
  )
where

import Control.Lens
import Data.Function (on)

{-
import ZipperList

type ParentNodes = ZipperList ParentNode

focusParentNodeByService :: ServiceName
                         -> ParentNodes
                         -> ParentNodes
focusParentNodeByService _ _ = undefined

appendParentNode' :: ParentNode
                  -> ParentNodes
                  -> ParentNodes
appendParentNode' _ _ = undefined
-}

type ServiceName = String

data ChildNode = ChildNode
  { _login :: String
  , _password :: Maybe String
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
  , _context :: (ServiceName, String)
  }

makeLenses ''ChildNode
makeLenses ''ParentNode
makeLenses ''Storage

appendParentNode :: ParentNode
                 -> [ParentNode]
                 -> [ParentNode]
appendParentNode pn []  = [pn]
appendParentNode pn (p:pns)
  | onService (<) pn p = pn:p:pns
  | onService (==) pn p = p:pns
  | otherwise                   = p:(appendParentNode pn pns)
  where onService predicate = predicate `on` (^. service)

appendService :: ServiceName
              -> Storage
              -> Storage
appendService name st = st & parentNodes %~ appendParentNode newNode
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


updateLogin :: String
            -> ServiceName
            -> [ParentNode]
            -> [ParentNode]
updateLogin ln current pns =
  map (\p -> if (p ^. service == current)
             then (p & childNodes %~ appendChild)
             else p) pns
  where appendChild cns = if (any (\c -> c ^. login == ln) cns)
                          then cns
                          else cns ++ [c]
                          where c = ChildNode
                                    { _login = ln
                                    , _cNodeIndex = length cns
                                    , _password = Nothing
                                    }

addLoginCurrent :: Storage
                -> String
                -> Storage
addLoginCurrent st login =
  st & parentNodes %~ updateLogin login (fst $ st ^. context)

updatePassword = undefined

setPassword :: Storage
            -> String
            -> Storage
setPassword st pw =
  st & parentNodes %~ updatePassword pw (fst $ st ^. context)
