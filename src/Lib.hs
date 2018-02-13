module Lib
    ( application
    ) where

import Data.ByteString.Lazy
import qualified Network.WebSockets as WS

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveDataMessage conn
    case msg of
      WS.Text b t -> return ()
      WS.Binary b -> print . unpack $ b
