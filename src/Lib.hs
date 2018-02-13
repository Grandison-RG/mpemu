{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( application
    ) where

import qualified Data.Text as T
import qualified Network.WebSockets as WS

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    putStrLn (T.unpack msg)
