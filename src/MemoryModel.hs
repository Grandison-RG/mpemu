{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module MemoryModel
  ( Storage (..)
  , checkParentNodeByService
  , appendService
  , addLoginCurrent
  , setPasswordStateUpdate
  , parentNodes
  , childNodes
  , login
  , cNodeIndex
  , ChildNode (..)
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
appendService name st = st
  & parentNodes %~ appendParentNode newNode
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

activeUpdateParent :: (ParentNode -> ParentNode)
                   -> (Storage -> Storage)
activeUpdateParent f st = parentNodes.traversed.(filtered active) %~ f $ st
  where active pnode = pnode^.service == current
        current = st^.context._1

activeUpdateChild :: Storage
                  -> (ChildNode -> ChildNode)
                  -> (ParentNode -> ParentNode)
activeUpdateChild st f = childNodes.traversed.(filtered active) %~ f
  where active cnode = cnode^.login == current
        current = st^.context._2

activeUpdate :: (ChildNode -> ChildNode)
             -> (Storage -> Storage)
activeUpdate f st = activeUpdateParent (activeUpdateChild st f) $ st

{-withActiveChild :: (ChildNode -> a)
                -> Storage
                -> a
withActiveChild f st = head . (map f) $
  st^.parentNodes.traversed.(filtered pActive).traversed.(filtered cActive)
  where pActive pnode = pnode^.service == st^.context._1
        cActive cnode = cnode^.login == st^.context._2-}
        
addLoginCurrent :: String
                -> Storage
                -> Storage
addLoginCurrent newLogin = activeUpdateParent $
  childNodes.filtered' %~ appendChild
  where
    filtered' = filtered $
      all (\c -> c^.login /= newLogin)
    appendChild cns = cns ++ [c]
      where c = ChildNode
              { _login = newLogin
              , _cNodeIndex = length cns
              , _password = Nothing
              }

setPasswordStateUpdate :: String
                       -> Storage
                       -> Storage
setPasswordStateUpdate password = activeUpdate $
        \c -> ChildNode{
                _login = (_login c)
              , _cNodeIndex = (_cNodeIndex c)
              , _password = Just password
        }
