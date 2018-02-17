{-# LANGUAGE BinaryLiterals #-}
module Lib
    ( application
    ) where

import Data.ByteString.Lazy
import qualified Network.WebSockets as WS
import Control.Monad (forever)

dispatchRequest :: ByteString -> ByteString
dispatchRequest msg
  | msg == testCommand = testResponse
  | otherwise = noSuchCommand
  where noSuchCommand = pack [0x0, 0xff]
        testCommand = pack [0x00, 0xb9]
        testResponse = pack [0x01, 0xb9, 0b101]

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ do
      msg <- WS.receiveData conn
      print . unpack $ msg
      let response = dispatchRequest msg
      WS.sendBinaryData conn response
