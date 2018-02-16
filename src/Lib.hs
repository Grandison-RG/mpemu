module Lib
    ( application
    ) where

import Data.ByteString.Lazy
import qualified Network.WebSockets as WS
import Control.Monad (forever)

printAndSendBack :: WS.Connection -> ByteString -> IO ()
printAndSendBack conn msg = do
  print . unpack $ msg
  WS.sendBinaryData conn msg

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ do
      msg <- WS.receiveDataMessage conn
      case msg of
        WS.Text b _ -> printAndSendBack conn b
        WS.Binary b -> printAndSendBack conn b
