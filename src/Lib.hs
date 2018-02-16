module Lib
    ( application
    ) where

import Data.ByteString.Lazy
import qualified Network.WebSockets as WS
import Control.Monad (forever)

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ do
      msg <- WS.receiveData conn
      print . unpack $ msg
      WS.sendBinaryData conn msg
