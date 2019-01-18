module Database.Sqlite.SqliteBTreePages
    (
        parseBTreeLeafPageWithState
    ,   parseFirstPage
    ,   SqliteBTreeFirstPage(..)
    ) where

-- parsing utils for the BTree Pages.

import qualified Database.Sqlite.SqliteDBHeader as FileHeader
import qualified Database.Sqlite.Utils as Utils
import qualified Data.Word as Word
import Data.Maybe(fromJust)
import Data.Tuple(swap)
import Data.Bits

data SqliteBTreeLeafPage = SqliteBTreeLeafPage 
    {
        btreePageHeader :: SqliteBTreePageHeader
    ,   cellPointers :: [Word.Word16]
    } deriving (Show)

data SqliteBTreeInteriorPage = SqliteBTreeInteriorPage
        {
        } deriving (Show)

data SqliteBTreeFirstPage = SqliteBTreeFirstPage
    {
        fileHeader :: FileHeader.SqliteHeader
    ,   leafPage :: SqliteBTreeLeafPage
    } deriving (Show)

data SqliteBTreePageType = InteriorIndex
        | InteriorTable
        | LeafIndex
        | LeafTable
        deriving (Eq, Show)

pageTypeMap :: [(Word.Word8, SqliteBTreePageType)]
pageTypeMap = 
    [
        (2, InteriorIndex), 
        (5, InteriorTable), 
        (10, LeafIndex), 
        (13, LeafTable)
    ]

getPageTypeFromValue = fromJust . flip lookup pageTypeMap
getPageValueFromType = fromJust . flip lookup (map swap pageTypeMap)

data SqliteBTreePageHeader = SqliteBTreePageHeader 
    {
        pageType :: SqliteBTreePageType
    ,   firstFreeBlock :: Word.Word16
    ,   numberOfCells :: Word.Word16
    ,   startOfCellContent :: Word.Word16
    ,   fragmentedFreeBytes :: Word.Word16
    ,   rightMostPointer :: Maybe Word.Word32
    } deriving (Eq, Show)

parseFirstPage :: Utils.BStateMonad SqliteBTreeFirstPage
parseFirstPage = do
    fileHeader' <- FileHeader.parseHeaderWithState 
    leafPage' <- parseBTreeLeafPageWithState
    return $ SqliteBTreeFirstPage fileHeader' leafPage'

parseBTreeLeafPageWithState :: Utils.BStateMonad SqliteBTreeLeafPage
parseBTreeLeafPageWithState = do
    btreePageHeader' <- parseBTreePageHeader
    cellPointers' <- collectCellPointers btreePageHeader'
    return $ SqliteBTreeLeafPage 
        btreePageHeader' cellPointers'

collectCellPointers :: SqliteBTreePageHeader -> Utils.BStateMonad [Word.Word16]
collectCellPointers pageHeader = let numCells = numberOfCells pageHeader
                                     numWords = fromIntegral (numCells * 2)
                                 in do
                                    cellPtrs <- map (\(x,y) -> (.|.) ((fromIntegral x::Word.Word16) `shiftL` 8)
                                                                          (fromIntegral y :: Word.Word16))
                                                    . Utils.pairAdjacent . Utils.getListOfWords <$> Utils.taken numWords
                                    return cellPtrs


parseBTreePageHeader :: Utils.BStateMonad SqliteBTreePageHeader
parseBTreePageHeader = do
    pageType' <- getPageTypeFromValue . Utils.getWord8 <$> Utils.taken 1
    firstFreeBlock' <- Utils.fixEndian16 <$> Utils.taken 2
    numberOfCells' <- Utils.fixEndian16 <$> Utils.taken 2
    startOfCellContent' <- Utils.fixEndian16 <$> Utils.taken 2
    fragmentedFreeBytes' <- Utils.fixEndian16 <$> Utils.taken 2
    if pageType' == InteriorIndex || pageType' == InteriorTable
    then do
        rightMostPointer' <- Utils.fixEndian32 <$> Utils.taken 4
        return $ SqliteBTreePageHeader
            pageType' firstFreeBlock'
            numberOfCells' startOfCellContent'
            fragmentedFreeBytes' (Just rightMostPointer')
    else 
        return $ SqliteBTreePageHeader
            pageType' firstFreeBlock'
            numberOfCells' startOfCellContent'
            fragmentedFreeBytes' Nothing

