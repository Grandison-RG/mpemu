module Main where

import qualified Network.WebSockets as WS
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Lib

main :: IO ()
main = do
    putStrLn "running..."
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 9160 $ application state
