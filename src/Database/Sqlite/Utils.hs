{-# LANGUAGE ViewPatterns #-}

module Database.Sqlite.Utils
    (
        BState(..)
    ,   BStateMonad
    ,   taken
    ,   fixEndian32
    ,   fixEndian16
    ) where

import qualified Data.ByteString as B
import qualified Control.Monad.State as State
import qualified Data.Word as Word
import Data.Bits

newtype BState = BState
    {
        bytes :: B.ByteString
    } deriving(Show, Eq)

type BStateMonad a = State.State BState a

taken :: Int -> BStateMonad B.ByteString
taken n = do
    BState curBytes <- State.get
    State.put (BState $ B.drop n curBytes)
    return (B.take n curBytes)

fixEndian32 :: B.ByteString -> Word.Word32
fixEndian32 (B.unpack -> [b1, b2, b3, b4]) = let
        a = (.|.) (fromIntegral b1 `shiftL` 24)  (fromIntegral b2 `shiftL` 16)
        b = (.|.) (fromIntegral b3 `shiftL` 8)  (fromIntegral b4)
        in (.|.) a b

fixEndian16 :: B.ByteString -> Word.Word16
fixEndian16 (B.unpack -> [l', r']) = 
    (.|.) (fromIntegral l' `shiftL` 8)  (fromIntegral r')

