
module Lib
    ( application
    ) where

import           Prelude hiding (drop)
import           Dispatch
import qualified Network.WebSockets         as WS
import           Control.Monad (forever)
import           Data.ByteString.Lazy       hiding (putStrLn)
import           MemoryModel
import           Control.Concurrent.MVar
import           Control.Monad.Trans.State (runStateT)

application :: MVar Storage -> WS.ServerApp
application var pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ do
      state <- takeMVar var
      msg <- WS.receiveData conn
      putStrLn "Request:"
      print . unpack $ msg
      let response = dispatchRequest
                       (commandMap msg)
                       (drop 2 msg)
      (result, state') <- runStateT response state
      putMVar var state'
      WS.sendBinaryData conn result
      putStrLn "Result:"
      print . unpack $ result
