{-# LANGUAGE BinaryLiterals #-}
module Dispatch
  ( dispatchRequest
  ,  commandMap
  ) where

import           Prelude                    hiding (length, tail)
import           Data.ByteString.Lazy       hiding (putStrLn,iterate,take)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Word(Word8)
import           Data.Function(on)
import           System.Random(newStdGen, randoms)
import           MemoryModel
import           Control.Concurrent ( MVar
                                    , putMVar
                                    , readMVar
                                    , takeMVar
                                    )
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class

type StateIO a = StateT Storage IO a

data Cmd = VERSION
         | MOOLTIPASS_STATUS
         | END_MEMORYMGMT
         | GET_MOOLTIPASS_PARM
         | SET_DATE
         | GET_CUR_CARD_CPZ
         | GET_RANDOM_NUMBER
         | CONTEXT
         | ADD_CONTEXT
         | SET_LOGIN
         | SET_PASSWORD
         | CHECK_PASSWORD
         | ERR
  deriving Show

secondByte :: ByteString -> Maybe Word8
secondByte = match . unpack
              where match (_:x:_) = Just x
                    match _ = Nothing

commandMap :: ByteString -> Cmd
commandMap c = case secondByte c of
                 Just byte ->
                   case byte of
                     0xA2 -> VERSION
                     0xB9 -> MOOLTIPASS_STATUS
                     0xD3 -> END_MEMORYMGMT
                     0xB2 -> GET_MOOLTIPASS_PARM
                     0xBB -> SET_DATE
                     0xC2 -> GET_CUR_CARD_CPZ
                     0xAC -> GET_RANDOM_NUMBER
                     0xA3 -> CONTEXT
                     0xA9 -> ADD_CONTEXT
                     0xA6 -> SET_LOGIN
                     0xA7 -> SET_PASSWORD
                     0xA8 -> CHECK_PASSWORD
                     _    -> ERR
                 Nothing -> ERR

addNullChar     :: String -> ByteString
addNullChar str = on append C.pack str "\0"

addLen :: ByteString -> ByteString
addLen bs = let len = fromIntegral . (+(-1)) . length $ bs
            in cons len bs 

rand = newStdGen >>= (\g -> return $ take 32 (randoms g::[Word8]) )

{--
rand = do
  stdGen <- newStdGen
  return $ take 32 (randoms stdGen :: [Word8])
--}

mooltipassStatus :: ByteString 
mooltipassStatus = pack [0x01, 0xb9, 0b101]

emulVer :: ByteString
emulVer = addLen datum
  where datum    = pack [0xa2,0x08] `append` addNullChar "v1.0_emul_remote"

memoryMgmt :: ByteString
memoryMgmt = pack [0x01, 0xd3, 0x01]

getMooltipassParm :: ByteString
getMooltipassParm = pack [0x01, 0xb2, 0x00]

setDate :: ByteString
setDate = pack [1, 0xbb, 0x01]

getCurCardCpz :: ByteString
getCurCardCpz = addLen bs
  where bs = pack $ [0xC2] ++ tt
        tt = take 8 $ iterate id 0xff

getRandomNumber :: StateIO ByteString
getRandomNumber = do
  r <- lift rand
  let res = addLen . pack $ [0xAC] ++ r
  return res

getContext :: String
            -> StateIO ByteString
getContext name = do
  storage <- get
  let res = checkParentNodeByService name storage
  case res of
    True  -> return $ pack [1, 0xA3, 0x01]
    False -> return $ pack [1, 0xA3, 0x00]

addContext :: String
               -> StateIO ByteString
addContext name = do
  storage <- get
  put $ appendService name storage
  return $ pack [1, 0xA9, 0x01]

setLogin :: String
          -> StateIO ByteString
setLogin login = do
  storage <- get
  put $ addLoginCurrent login storage
  return . pack $ [0x01, 0xA6, 0x01]
  
err :: ByteString
err = pack [0x0, 0xff]

checkPassword :: String
               -> StateIO ByteString
checkPassword password = do
  return . pack $ [0x01, 0xA8, 0x00]

dispatchRequest :: Cmd
                 -> ByteString
                 -> StateIO ByteString
dispatchRequest c input =
  case c of
    MOOLTIPASS_STATUS   -> return mooltipassStatus
    VERSION             -> return emulVer
    END_MEMORYMGMT      -> return memoryMgmt
    GET_MOOLTIPASS_PARM -> return getMooltipassParm
    SET_DATE            -> return setDate
    GET_CUR_CARD_CPZ    -> return getCurCardCpz
    GET_RANDOM_NUMBER   -> getRandomNumber
    CONTEXT             -> getContext    strInput
    ADD_CONTEXT         -> addContext    strInput
    SET_LOGIN           -> setLogin      strInput
    CHECK_PASSWORD      -> checkPassword strInput
    ERR                 -> return err
  where strInput :: String
        strInput = C.unpack input
