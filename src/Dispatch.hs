{-# LANGUAGE BinaryLiterals #-}
module Dispatch
  (dispatchRequest,commandMap)
where

import           Prelude                    hiding (length, tail)
import           Data.ByteString.Lazy       hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as C
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

addNullChar     :: String -> ByteString
addNullChar str = on append C.pack str "\0"

addLen :: ByteString -> ByteString
addLen bs = let len = fromIntegral . (+(-1)) . length $ bs
            in cons len bs 
  
mooltipassStatus :: ByteString 
mooltipassStatus = pack [0x01, 0xb9, 0b101]

emulVer :: ByteString
emulVer = addLen datum
  where datum    = pack [0xa2,0x08] `append` addNullChar "v1.0_emul_remote"

err :: ByteString
err = pack [0x0, 0xff]

dispatchRequest   :: Cmd -> ByteString
dispatchRequest c = case c of
                      MOOLTIPASS_STATUS -> mooltipassStatus
                      VERSION           -> emulVer
                      ERR               -> err

        
        
