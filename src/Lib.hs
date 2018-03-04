
module Lib
    ( application
    ) where

import           Dispatch
import qualified Network.WebSockets         as WS
import           Control.Monad (forever)
import           Data.ByteString.Lazy       hiding (putStrLn)
import           MemoryModel
import           Control.Concurrent

application :: MVar ListOfParentNodes -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ do
      msg <- WS.receiveData conn
      putStrLn "Request:"
      print . unpack $ msg
      let response = dispatchRequest state $ commandMap msg
      result <- response
      WS.sendBinaryData conn result
      putStrLn "Result:"
      print . unpack $ result
