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
        let result = runner "x;" in 
            strips result `shouldParse` [LBranch [LLeaf (LId () "x")]]
    it "parses two statements in a row" $ do 
        let result = runner "x;y;" in 
            strips result `shouldParse` [
                LBranch [LLeaf (LId () "x")],
                LBranch [LLeaf (LId () "y")]]
    it "parses an empty parentheses" $ do 
        let result = runner "( );" in 
            strips result `shouldParse` [LBranch [LParen () []]]
    it "parses values inside a parentheses" $ do 
        let result = runner "( x y z);" in 
            strips result `shouldParse` [LBranch [LParen () [
                LLeaf (LId () "x"),
                LLeaf (LId () "y"),
                LLeaf (LId () "z")]]]
    it "parses values inside a square bracket" $ do 
        let result = runner "[ x y z];" in 
            strips result `shouldParse` [LBranch [LBrack () [
                LLeaf (LId () "x"),
                LLeaf (LId () "y"),
                LLeaf (LId () "z")]]]
    it "parses a parentheses inside a parentheses" $ do 
        let result = runner "( (x) z);" in 
            strips result `shouldParse` [LBranch [LParen () [
                LParen () [LLeaf (LId () "x")],
                LLeaf (LId () "z")]]]
    it "parses a statement after a preprocessor directive" $ do 
        let result = runner "#pre\nxyz;" in 
            strips result `shouldParse` [
                LBranch [LLeaf (LPre () "pre")],
                LBranch [LLeaf (LId () "xyz")]]
    it "parses a statement before a preprocessor directive" $ do 
        let result = runner "xyz;\n#pre" in 
            strips result `shouldParse` [
                LBranch [LLeaf (LId () "xyz")],
                LBranch [LLeaf (LPre () "pre")]]
    it "parses a block inside a block" $ do 
        let result = runner "{ x; y; };" in 
            strips result `shouldParse` [ LBranch [ LCurly () [
                LBranch [LLeaf (LId () "x")],
                LBranch [LLeaf (LId () "y")]]]]
    it "parses a series of tokens" $ do 
        let result = runner "3.14 %% (x);" in 
            strips result `shouldParse` [ LBranch [
                LLeaf (LDbl () 3.14),
                LLeaf (LOp () "%%"),
                LParen () [LLeaf (LId () "x")]]]
    it "does not parse semicolons in a paren" (failer "(x;);") 

test :: IO ()
test = hspec grouperTest1