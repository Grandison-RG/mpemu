{-# LANGUAGE BinaryLiterals #-}
module Lib
    ( application
    ) where

import           Prelude                    hiding (length, tail)

import           Data.ByteString.Lazy       hiding (putStrLn)
import qualified Network.WebSockets         as WS
import qualified Data.ByteString.Lazy.Char8 as C
import           Control.Monad (forever)
import           Data.Word(Word8)
import           Data.Function(on)

data Cmd = VERSION | MOOLTIPASS_STATUS | ERR
  deriving Show

secondByte :: ByteString -> Word8
secondByte = (\(_:x:_) -> x) . unpack

commandMap   :: ByteString -> Cmd
commandMap c = case secondByte c of
                0xA2 -> VERSION
                0xB9 -> MOOLTIPASS_STATUS
                _    -> ERR

dispatchRequest   :: Cmd -> ByteString
dispatchRequest c = case c of
                      MOOLTIPASS_STATUS -> mooltipassStatus
                      VERSION           -> emulVer
                      ERR               -> err
  where addNullChar str  = on append C.pack str "\0"  
        mooltipassStatus = pack [0x01, 0xb9, 0b101]
        emulVer          = cons len datum
          where datum    = pack [0xa2,0x08] `append` addNullChar "v1.0_emul_remote"
                len      = fromIntegral (length datum - 1)
        err              = pack [0x0, 0xff]


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
