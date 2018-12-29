module Database.Sqlite.SqliteDBHeader
    (
        parseHeader
    ,   parseHeaderWithState
    ,   SqliteHeader(..)
    ) where

import qualified Data.ByteString as B
import qualified Data.Word as Word
import qualified Control.Monad.State as State
import qualified Database.Sqlite.Utils as BState

parseHeader :: B.ByteString -> (SqliteHeader, BState.BState)
parseHeader bytes= State.runState parseHeaderWithState (BState.BState bytes)

-- TODO: missing error handling. 
-- use monad Transformers?? 
-- stack can be StateT with Either
parseHeaderWithState :: BState.BStateMonad SqliteHeader
parseHeaderWithState = do
    hString <- BState.taken 16
    pSize <- BState.fixEndian16 <$> BState.taken 2
    wSize <- fromIntegral.head.B.unpack <$> BState.taken 1
    rSize <- fromIntegral.head.B.unpack <$> BState.taken 1
    pageRsvdSpace <- fromIntegral.head.B.unpack <$> BState.taken 1
    maxEmbPayload <- fromIntegral.head.B.unpack <$> BState.taken 1
    minEmbPayload <- fromIntegral.head.B.unpack <$> BState.taken 1
    leafPayloadFraction' <- fromIntegral.head.B.unpack <$> BState.taken 1
    dbChangeCounter' <- BState.fixEndian32 <$> BState.taken 4
    inHeaderDBSize' <- BState.fixEndian32 <$> BState.taken 4
    firstFreeListTrunkPage' <- BState.fixEndian32 <$> BState.taken 4
    totalFreeListPages' <- BState.fixEndian32 <$> BState.taken 4
    schemaCookie' <- BState.fixEndian32 <$> BState.taken 4
    schemaFormat' <- BState.fixEndian32 <$> BState.taken 4
    defaultPageCacheSize' <- BState.fixEndian32 <$> BState.taken 4
    largestRootBTreeRoot' <- BState.fixEndian32 <$> BState.taken 4
    encoding' <- BState.fixEndian32 <$> BState.taken 4
    userVersion' <- BState.fixEndian32 <$> BState.taken 4
    isIncVacMode' <- BState.fixEndian32 <$> BState.taken 4
    appId' <- BState.fixEndian32 <$> BState.taken 4
    reserved' <- BState.taken 20
    versionValidFor' <- BState.fixEndian32 <$> BState.taken 4
    sqliteVersionNum' <- BState.fixEndian32 <$> BState.taken 4
    return $ SqliteHeader 
        hString pSize wSize 
        rSize pageRsvdSpace
        maxEmbPayload minEmbPayload
        leafPayloadFraction' dbChangeCounter'
        inHeaderDBSize' firstFreeListTrunkPage'
        totalFreeListPages' schemaCookie'
        schemaFormat' defaultPageCacheSize'
        largestRootBTreeRoot' encoding'
        userVersion' isIncVacMode'
        appId' reserved'
        versionValidFor' sqliteVersionNum'

data SqliteHeader = SqliteHeader 
    {
        magicString :: B.ByteString
    ,   pageSize :: Word.Word16
    ,   writeVersion :: Word.Word8
    ,   readVersion :: Word.Word8
    ,   pageReservedSpace :: Word.Word8
    ,   maxEmbeddedPayload :: Word.Word8
    ,   minEmbeddedPayload :: Word.Word8
    ,   leafPayloadFraction :: Word.Word8
    ,   dbChangeCounter :: Word.Word32
    ,   inHeaderDBSize :: Word.Word32
    ,   firstFreeListTrunkPage :: Word.Word32
    ,   totalFreeListPages :: Word.Word32
    ,   schemaCookie :: Word.Word32
    ,   schemaFormat :: Word.Word32
    ,   defaultPageCacheSize :: Word.Word32
    ,   largestRootBTreeRoot :: Word.Word32
    ,   encoding :: Word.Word32
    ,   userVersion :: Word.Word32
    ,   isIncVacMode :: Word.Word32
    ,   appId :: Word.Word32
    ,   reserved :: B.ByteString
    ,   versionValidFor :: Word.Word32
    ,   sqliteVersionNum :: Word.Word32
    } deriving(Eq, Show)


