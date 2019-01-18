{-# LANGUAGE ViewPatterns #-}

module Database.Sqlite.Utils
    (
        BState(..)
    ,   BStateMonad
    ,   taken
    ,   peekn
    ,   fixEndian32
    ,   fixEndian16
    ,   getWord8
    ,   pairAdjacent
    ,   getListOfWords
    ) where

import qualified Data.ByteString as B
import qualified Control.Monad.State as State
import qualified Data.Word as Word
import Data.Bits

data BState = BState
    {
        bytes :: B.ByteString
    ,   pgOffset :: Int
    } deriving(Show, Eq)

type BStateMonad a = State.State BState a

taken :: Int -> BStateMonad B.ByteString
taken n = do
    BState curBytes pageOffset <- State.get
    State.put $ BState (B.drop n curBytes) pageOffset
    return $ B.take n curBytes

peekn :: Int -> BStateMonad B.ByteString
peekn n = do
    b@(BState curBytes _) <- State.get
    State.put b
    return (B.take n curBytes)

getCurrentPageOffset :: BStateMonad Int
getCurrentPageOffset = do
    b@(BState _ pageOffset) <- State.get
    State.put b
    return pageOffset

{--
 -- At somepoint make this conversion to host type, 
 -- rather than assuming little endian for the host.
--}
fixEndian32 :: B.ByteString -> Word.Word32
fixEndian32 (B.unpack -> [b1, b2, b3, b4]) = let
        a = (.|.) (fromIntegral b1 `shiftL` 24)  (fromIntegral b2 `shiftL` 16)
        b = (.|.) (fromIntegral b3 `shiftL` 8)  (fromIntegral b4)
        in (.|.) a b

fixEndian16 :: B.ByteString -> Word.Word16
fixEndian16 (B.unpack -> [l', r']) = 
    (.|.) (fromIntegral l' `shiftL` 8)  (fromIntegral r')

getWord8 :: B.ByteString -> Word.Word8
getWord8 (B.unpack -> [b]) = b

pairAdjacent :: [a] -> [(a,a)]
pairAdjacent (x:y:xs) = (x,y) : pairAdjacent xs
pairAdjacent _        = []

getListOfWords :: B.ByteString -> [Word.Word8]
getListOfWords = B.unpack

