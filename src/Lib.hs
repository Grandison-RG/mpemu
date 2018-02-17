module Lib
    ( application
    ) where

import Data.ByteString.Lazy
import qualified Network.WebSockets as WS
import Control.Monad (forever)

dispatchRequest :: ByteString -> ByteString
dispatchRequest msg = noSuchCommand
  where noSuchCommand = pack [0, 255]

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ do
      msg <- WS.receiveData conn
      print . unpack $ msg
      let response = dispatchRequest msg
      WS.sendBinaryData conn response
