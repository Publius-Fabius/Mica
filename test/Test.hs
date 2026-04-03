module Main where 

import qualified TestLexer 
import qualified TestGrouper

main :: IO ()
main = do
    TestLexer.test
    TestGrouper.test
