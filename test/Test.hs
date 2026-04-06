module Main where 

import qualified TestLexer 
import qualified TestGrouper
import qualified TestParser 

main :: IO ()
main = do
    TestLexer.test
    TestGrouper.test
    TestParser.test
