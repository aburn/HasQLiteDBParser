module Database.Sqlite.SqliteDeSerialize
    (
        parse
    ) where

import qualified Data.ByteString as B
import qualified Database.Sqlite.SqliteDBHeader as SqliteDBHeader
import Control.Monad(liftM)

parse :: B.ByteString -> SqliteAst
parse = SqliteAst . (fst.SqliteDBHeader.parseHeader)

data SqliteAst = SqliteAst
        {
            header :: SqliteDBHeader.SqliteHeader
        } deriving (Show)


