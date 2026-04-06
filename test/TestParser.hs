{-# LANGUAGE OverloadedStrings #-}

module TestParser where 

import Mica.Grouper 
import Mica.Lexer
import Mica.Parser
import Mica.Type
import Text.Megaparsec.Error
import Data.Text (Text)
import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

strip p = (const () <$>) <$> p

strips p = map (const () <$>) <$> p

runner p txt = case parse lMica "" txt of 
    Right tokens -> case parse gMica "" tokens of 
        Right trees -> parse p "" trees 
        Left err -> parse (fail (errorBundlePretty err)) "" []
    Left err -> parse (fail (errorBundlePretty err)) "" []

failer p txt = case parse lMica "" txt of 
    Right tokens -> case parse gMica "" tokens of 
        Right trees -> parse p "" `shouldFailOn` trees 
        Left err -> putStrLn $ errorBundlePretty err 
    Left err -> putStrLn $ errorBundlePretty err 

test1 :: Spec 
test1 = describe "Parser (Test 1)" $ do
    it "parses a raw identifier" $
        let result = runner (pRawIden <* eof) "x" in 
            result `shouldParse` "x"
    it "parses a raw string literal" $ 
        let result = runner (pRawStrLit <* eof) "\"hi\"" in 
            result `shouldParse` "hi"
    it "parses an expected operator" $ 
        let result = runner (pExpectOp ";" <* eof) ";" in 
            result `shouldParse` initialPos ""
    it "should not parse an expected operator when there's an identifier" $ 
        failer (pExpectOp "%" <* eof) "f"
    it "parses an expected identifier" $ 
        let result = runner (pExpectIden "hi" <* eof) "hi" in 
            result `shouldParse` initialPos ""
    it "should not parse an expected identifier when there's an operator" $
        failer (pExpectIden "x" <* eof) "&"
    it "parses an identifier into a Name constructor" $ 
        let result = runner (pIden Name <* eof) "hi" in
            (strip result) `shouldParse` (Name () "hi")
    it "does not parse an identifier when there's an operator" $
        failer (pIden Name <* eof) "$"
    it "does not parse an identifier when there's a paren" $
        failer (pIden Name <* eof) "()"
    it "parses an identifier inside a paren" $ 
        let result = runner (pParen (pIden Name <* eof) <* eof) "(hi)" in
            (strip result) `shouldParse` (Name () "hi")
    it "parses an identifier inside a curly" $ 
        let result = runner (pCurly (pIden Name <* eof) <* eof) "{hi}" in
            (strip result) `shouldParse` (Name () "hi")
    it "parses an identifier inside a brack" $ 
        let result = runner (pBrack (pIden Name <* eof) <* eof) "[hi]" in
            (strip result) `shouldParse` (Name () "hi")
    it "parses an identifier atom" $ 
        let result = runner (pAtom <* eof) "hi" in    
            (strip result) `shouldParse` (Iden () "hi")
    it "parses double literal atom" $ 
        let result = runner (pAtom <* eof) "3.14" in    
            (strip result) `shouldParse` (DblLit () 3.14)
    it "parses an integer literal atom" $ 
        let result = runner (pAtom <* eof) "314" in    
            (strip result) `shouldParse` (IntLit () 314)
    it "parses a string literal atom" $ 
        let result = runner (pAtom <* eof) "\"hello\"" in    
            (strip result) `shouldParse` (StrLit () "hello")
    it "parses a char literal atom" $ 
        let result = runner (pAtom <* eof) "\'a\'" in    
            (strip result) `shouldParse` (CharLit () 'a')
    it "parses a unary atom" $ 
        let result = runner (pAtom <* eof) "+a" in 
            (strip result) `shouldParse` (Una () "+" (Iden () "a")) 
    it "parses a nested unary atom" $ 
        let result = runner (pAtom <* eof) "+ -a" in 
            (strip result) `shouldParse` 
                (Una () "+" (Una () "-" (Iden () "a"))) 
    it "parses an identifier expression" $
        let result = runner (pExp <* eof) "a" in 
            (strip result) `shouldParse` (Iden () "a")
    it "parses a unary expression" $
        let result = runner (pExp <* eof) "+a" in 
            (strip result) `shouldParse` (Una () "+" (Iden () "a")) 
    it "parses a binary expression" $
        let result = runner (pExp <* eof) "a+b" in 
            (strip result) `shouldParse` 
            (Bin () "+" (Iden () "a") (Iden () "b")) 
    it "parses two binary operations with proper precedence (reverse order)" $
        let result = runner (pExp <* eof) "a+b*c" in 
            (strip result) `shouldParse` 
            (Bin () "+" (Iden () "a") (Bin () "*" (Iden () "b") (Iden () "c")))
    it "parses two binary operations with proper precedence (same op)" $
        let result = runner (pExp <* eof) "a+b+c" in 
            (strip result) `shouldParse` 
            (Bin () "+" (Bin () "+" (Iden () "a") (Iden () "b")) (Iden () "c"))
    it "parses a binary with a unary in the second operand" $
        let result = runner (pExp <* eof) "a+ -b" in 
            (strip result) `shouldParse` 
            (Bin () "+" (Iden () "a") (Una () "-" (Iden () "b")))
    it "parses a binary with a unary in the first operand" $
        let result = runner (pExp <* eof) "-1 + 2" in 
            (strip result) `shouldParse` 
            (Bin () "+" (Una () "-" (IntLit () 1)) (IntLit () 2))
    it "parses a binary with a unary in the first and second operand" $
        let result = runner (pExp <* eof) "-1 + -2" in 
            (strip result) `shouldParse` 
            (Bin () "+" (Una () "-" (IntLit () 1)) (Una () "-" (IntLit () 2)))
    it "does not parse a semi as an expression" $
        failer (pExp  <* eof) ";"
    it "associates operands by parentheses" $
        let result = runner (pExp <* eof) "(a+b)*c" in 
            (strip result) `shouldParse` 
            (Bin () "*" 
                (Paren () (Bin () "+" (Iden () "a") (Iden () "b"))) 
                (Iden () "c"))  
    it "parses an assignment operation" $ 
        let result = runner (pBlockStmt <* eof) "x := y;" in 
            (strip result) `shouldParse` 
            (Assign (Bin () ":=" (Iden () "x") (Iden () "y")))
        
test :: IO ()
test = hspec test1