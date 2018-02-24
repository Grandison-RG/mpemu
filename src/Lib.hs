
module Lib
    ( application
    ) where

import           Dispatch
import qualified Network.WebSockets         as WS
import           Control.Monad (forever)
import           Data.ByteString.Lazy       hiding (putStrLn)


application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ do
      msg <- WS.receiveData conn
      putStrLn "Request:"
      print . unpack $ msg
      let response = dispatchRequest . commandMap $ msg
      WS.sendBinaryData conn response
      putStrLn "Response:"
      print . unpack $ response
