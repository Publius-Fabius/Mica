module Main where 

import qualified TestLexer 
import qualified TestGrouper
import qualified TestParser 
import qualified TestChecker

main :: IO ()
main = do
    TestLexer.test
    TestGrouper.test
    TestParser.test
    TestChecker.test
