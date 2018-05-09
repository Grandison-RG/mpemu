module Main where

import qualified Network.WebSockets as WS
import Control.Concurrent(MVar, newMVar)
import Lib
import MemoryModel

memory :: Storage
memory = Storage
  { _parentNodes = []
  , _lastIndex = 0
  , _context = ("", "")
  }

main :: IO ()
main = do
    state <- newMVar memory
    putStrLn "running..."
    WS.runServer "127.0.0.1" 30036 (application state)
