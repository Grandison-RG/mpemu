{-# LANGUAGE BinaryLiterals #-}
module Lib
    ( application
    ) where

import Prelude hiding (length, tail)

import           Data.ByteString.Lazy       hiding (putStrLn)
import qualified Network.WebSockets         as WS
import qualified Data.ByteString.Lazy.Char8 as C
import           Control.Monad (forever)
import           Data.Word(Word8)

data Cmd = VERSION | MOOLTIPASS_STATUS | ERR
  deriving Show

-- emulVer :: String
-- emulVer = "v1.0_emul_remote"

moolipassStatus :: ByteString
moolipassStatus = pack [0x01, 0xb9, 0b101]

emulVer :: ByteString
emulVer = cons len datum
  where datum = pack [0xa2,0x08] `append` C.pack "v1.0_emul_remote"
        len   = fromIntegral (length datum - 1)

err :: ByteString
err = pack [0x0, 0xff]

secondByte :: ByteString -> Word8
secondByte = (\(_:x:_) -> x) . unpack

commandMap   :: ByteString -> Cmd
commandMap c = case secondByte c of
                0xA2 -> VERSION
                0xB9 -> MOOLTIPASS_STATUS
                _    -> ERR

dispatchRequest     :: ByteString -> ByteString
dispatchRequest msg = case commandMap msg of
                        MOOLTIPASS_STATUS -> moolipassStatus
                        VERSION           -> emulVer
                        ERR               -> err


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
