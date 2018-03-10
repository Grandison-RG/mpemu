{-# LANGUAGE BinaryLiterals #-}
module Dispatch
  ( dispatchRequest,
    commandMap
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

data Cmd = VERSION
         | MOOLTIPASS_STATUS
         | END_MEMORYMGMT
         | GET_MOOLTIPASS_PARM
         | SET_DATE
         | GET_CUR_CARD_CPZ
         | GET_RANDOM_NUMBER
         | CONTEXT
         | ADD_CONTEXT
         | ERR
  deriving Show

secondByte :: ByteString -> Word8
secondByte = (\(_:x:_) -> x) . unpack

commandMap :: ByteString -> Cmd
commandMap c = case secondByte c of
                 0xA2 -> VERSION
                 0xB9 -> MOOLTIPASS_STATUS
                 0xD3 -> END_MEMORYMGMT
                 0xB2 -> GET_MOOLTIPASS_PARM
                 0xBB -> SET_DATE
                 0xC2 -> GET_CUR_CARD_CPZ
                 0xAC -> GET_RANDOM_NUMBER
                 0xA3 -> CONTEXT
                 0xA9 -> ADD_CONTEXT
                 _    -> ERR

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

getRandomNumber :: IO ByteString
getRandomNumber = do
  r <- rand
  let res = addLen . pack $ [0xAC] ++ r
  return res

getContext :: MVar Storage
              -> String
              -> IO ByteString
getContext state name = do
  storage <- readMVar state
  let res = True --checkParentNodeByService name storage
  case res of
    True  -> return $ pack [1, 0xA3, 0x01]
    False -> return $ pack [1, 0xA3, 0x00]

addContext :: MVar Storage
              -> String
              -> IO ByteString
addContext state name = do
  storage <- takeMVar state
  putMVar state $ storage --appendService name storage
  return $ pack [1, 0xA9, 0x01]
  
err :: ByteString
err = pack [0x0, 0xff]

dispatchRequest :: MVar Storage
                -> Cmd
                -> ByteString
                -> IO ByteString
dispatchRequest state c input = case c of
                                  MOOLTIPASS_STATUS   -> return mooltipassStatus
                                  VERSION             -> return emulVer
                                  END_MEMORYMGMT      -> return memoryMgmt
                                  GET_MOOLTIPASS_PARM -> return getMooltipassParm
                                  SET_DATE            -> return setDate
                                  GET_CUR_CARD_CPZ    -> return getCurCardCpz
                                  GET_RANDOM_NUMBER   -> getRandomNumber
                                  CONTEXT             -> getContext state $ C.unpack input
                                  ADD_CONTEXT         -> addContext state $ C.unpack input
                                  ERR                 -> return err
