{-# LANGUAGE BinaryLiterals #-}
module Lib
    ( application
    ) where

import Prelude hiding (length, tail)

import Data.ByteString.Lazy hiding (putStrLn)
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad (forever)

type Command = ByteString
type Response = ByteString

dispatchRequest :: Command -> Response
dispatchRequest msg
  | msg == testCmd = testRsp
  | msg == versionCmd = versionRsp
  | otherwise = noSuchCmd
  where noSuchCmd = pack [0x00, 0xff]      :: Response

        testRsp = pack [0x01, 0xb9, 0b101] :: Response
        versionRsp = cons len datum        :: Response
                     where
                       datum = pack [0xa2, 0x08] `append`
                               (C.pack "v1.0_emul_remote")
                       len = fromIntegral . length $ datum

        testCmd = pack [0x00, 0xb9]        :: Command
        versionCmd = pack [0x00, 0xa2]     :: Command

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    forever $ do
      msg <- WS.receiveData conn
      putStrLn "Request:"
      print . unpack $ msg
      let response = dispatchRequest msg
      WS.sendBinaryData conn response
      putStrLn "Response:"
      print . unpack $ response
