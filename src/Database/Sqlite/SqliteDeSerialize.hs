module Database.Sqlite.SqliteDeSerialize
    (
        parse
    ) where

import qualified Data.ByteString as B
import qualified Database.Sqlite.Utils as BState
import qualified Control.Monad.State as State
import qualified Database.Sqlite.SqliteBTreePages as SqliteBTreePages

parse :: B.ByteString -> SqliteAst
parse bytes = parse' (BState.BState bytes)

parse' :: BState.BState -> SqliteAst
parse' = fst.State.runState parseWithState 

-- will be moving this to a transformer stack 
-- to deal with errors. For now assume perfect world.
parseWithState :: BState.BStateMonad SqliteAst
parseWithState = do
    firstPage' <- SqliteBTreePages.parseFirstPage
    return $ SqliteAst firstPage' []

data SqliteAst = SqliteAst
        {
            firstPage :: SqliteBTreePages.SqliteBTreeFirstPage
        ,   freeListPageNumbers :: [Int]
        } deriving (Show)


