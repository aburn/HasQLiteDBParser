module Main where

import Lib
import System.Environment(getArgs)
import qualified Data.ByteString as B

-- update with proper UI or integrate with Yesod.

main :: IO ()
main = do
    [file] <- getArgs
    content <- B.readFile file
    print $ parse content

