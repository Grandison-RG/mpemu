module Main where

import qualified Network.WebSockets as WS
import Lib

main :: IO ()
main = do
    putStrLn "running..."
    WS.runServer "127.0.0.1" 30036 application
