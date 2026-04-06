{-# LANGUAGE OverloadedStrings #-}

module TestGrouper where 

import Mica.Grouper 
import Mica.Lexer
import Text.Megaparsec.Error
import Data.Text (Text)
import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

strip p = (const () <$>) <$> p

strips p = map (const () <$>) <$> p

--runner :: Text -> Grouper [LexTree SP] 
runner txt = case parse lMica "" txt of 
    Right tokens -> parse gMica "" tokens
    Left err -> parse (fail (errorBundlePretty err)) "" []

failer txt = case parse lMica "" txt of 
    Right tokens -> parse gMica "" `shouldFailOn` tokens
    Left err -> putStrLn $ errorBundlePretty err 

grouperTest1 :: Spec 
grouperTest1 = describe "Grouper (Test 1)" $ do
    it "parses a basic identifier" $ do 
        let result = runner "x" in 
            strips result `shouldParse` [LLeaf (LId () "x")]
    it "parses two idens in a row" $ do 
        let result = runner "x y" in 
            strips result `shouldParse` [
                LLeaf (LId () "x"),
                LLeaf (LId () "y")]
    it "parses an empty parentheses" $ do 
        let result = runner "( )" in 
            strips result `shouldParse` [LParen () []]
    it "parses an empty curly" $ do 
        let result = runner "{ /* hi */ }" in 
            strips result `shouldParse` [LCurly () []]
    it "parses idens inside a parentheses" $ do 
        let result = runner "(x y z)" in 
            strips result `shouldParse` [LParen () [
                LLeaf (LId () "x"),
                LLeaf (LId () "y"),
                LLeaf (LId () "z")]]
    it "parses values inside a square bracket" $ do 
        let result = runner "[ x y z ]" in 
            strips result `shouldParse` [LBrack () [
                LLeaf (LId () "x"),
                LLeaf (LId () "y"),
                LLeaf (LId () "z")]]
    it "parses a parentheses inside a parentheses" $ do 
        let result = runner "( (x) z)" in 
            strips result `shouldParse` [LParen () [
                LParen () [LLeaf (LId () "x")],
                LLeaf (LId () "z")]]
    it "parses a block inside a block" $ do 
        let result = runner "{ {x} y }" in 
            strips result `shouldParse` [ LCurly () [
                LCurly () [LLeaf (LId () "x")],
                LLeaf (LId () "y")]]
    it "parses a series of mixed tokens" $ do 
        let result = runner "3.14 %% (x)" in 
            strips result `shouldParse` [ 
                LLeaf (LDbl () 3.14),
                LLeaf (LOp () "%%"),
                LParen () [LLeaf (LId () "x")]]
   
test :: IO ()
test = hspec grouperTest1