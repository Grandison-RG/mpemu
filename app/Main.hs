module Main where

import qualified Network.WebSockets as WS
import Control.Concurrent
import Lib
import MemoryModel

memory :: ListOfParentNodes
memory = []

main :: IO ()
main = do
    state <- newMVar memory
    putStrLn "running..."
    WS.runServer "127.0.0.1" 30036 (application state)
