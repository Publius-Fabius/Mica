{-# LANGUAGE OverloadedStrings #-}

module TestLexer where 

import Mica.Lexer 
import Text.Megaparsec.Error
import Data.Text (Text)

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

ip = initialPos ""

strip p = (const () <$>) <$> p
strips p = map (const () <$>) <$> p

lexerTest1 :: Spec
lexerTest1 = describe "Lexer (Test 1)" $ do
    it "lexes a basic identifier" $
        parse lId "" "buffer_16KB" `shouldParse` LId ip "buffer_16KB"
    it "lexes a basic integer" $
        parse lInt "" "1234" `shouldParse` LInt ip 1234
    it "lexes a basic double" $
        parse lDbl "" "3.14" `shouldParse` LDbl ip (3.14)
    it "lexes a basic string" $
        parse lStr "" "\"hello\"" `shouldParse` LStr ip "hello"
    it "doesn't lex strings with new lines" $
        parse lStr "" `shouldFailOn` "\"hello \n \""
    it "lexes a basic operator" $
        parse lOp "" "&&^" `shouldParse` LOp ip "&&^"
    it "lexes a complex identifier" $
        parse lId "" "_x_123" `shouldParse` LId ip "_x_123"
    it "lexes a delimiter" $ 
        let result = parse lDelim "" "(" in 
        strip result `shouldParse` LDelim () '('
    it "lexes a preprocessor directive at the head of a file" $
        let result = parse lPreHead "" "#hi" in 
        strip result `shouldParse` LPre () "hi" 
    it "lexes a preprocessor directive at the head of a file with comments" $
        let result = parse lPreHead "" "// \n /* s \n */ #hi" in
        strip result `shouldParse` LPre () "hi" 
    it "lexes a preprocessor directive in the body" $ 
        let result = parse lPreBody "" "\n#hi" in
        strip result `shouldParse` LPre () "hi" 
    it "lexes a preprocessor directive in the body with comments" $ 
        let result = parse lPreBody "" "\n /* \n */ \n #hi" in
        strip result `shouldParse` LPre () "hi" 
    it "doesn't accept preprocessor directives in the body without a newline" $
        parse lPreBody "" `shouldFailOn` " /* \n */ \n #hi"
    
lexerTest2 :: Spec 
lexerTest2 = describe "Lexer (Test 2)" $ do 
    it "can lex multiple idens in a row" $
        let result = parse lMica "" "x y z" in
        strips result `shouldParse` [LId () "x", LId () "y", LId () "z"]
    it "can lex multiple mixed terms in a row" $
        let result = parse lMica "" "x 2.5 z" in
        strips result `shouldParse` [LId () "x", LDbl () 2.5, LId () "z"]
    it "ignores multi-line comments while lexing multiple terms" $
        let result = parse lMica "" "x /* y */ z" in
        strips result `shouldParse` [LId () "x", LId () "z"]
    it "ignores single-line comments while lexing multiple terms" $
        let result = parse lMica "" "x // z \nw" in
        strips result `shouldParse` [LId () "x", LId () "w"]
    it "can lex a preprocessor directive at the head of file" $ 
        let result = parse lMica "" "#hi" in 
        strips result `shouldParse` [LPre () "hi"]
    it "can lex two preprocessor directives in a row" $ 
        let result = parse lMica "" "#hi\n  #yo" in 
        strips result `shouldParse` [LPre () "hi", LPre () "yo"]
    it "can lex two preprocessor directives with comments between them" $
        let result = parse lMica "" "#hi\n /* */ #yo" in 
        strips result `shouldParse` [LPre () "hi", LPre () "yo"]
    it "can lex two preprocessor directives in a row within the body" $
        let result = parse lMica "" "f\n#hi\n /* x \n x \n */ #yo" in 
        strips result `shouldParse` [LId () "f", LPre () "hi", LPre () "yo"]
    it "can lex a symbol after lexing multiple preprocessor directives" $
        let result = parse lMica "" "#hi\n#yo\n f" in 
        strips result `shouldParse` [LPre () "hi", LPre () "yo", LId () "f"]
    it "can lex a multi-symbol preprocessor directive" $ 
        let result = parse lMica "" "f\n#define X 1" in 
        strips result `shouldParse` [LId () "f", LPre () "define X 1"]
    it "can lex indented preprocessor directives at the head of file" $ 
        let result = parse lMica "" "    #indented" in 
        strips result `shouldParse` [LPre () "indented"]
    it "can lex indented preprocessor directives in the file body" $ 
        let result = parse lMica "" "f\n    #indented" in 
        strips result `shouldParse` [LId () "f", LPre () "indented"]
    it "doesn't lex directives that don't start after a newline + ws" $
        let result = parse lMica "" "f #not" in 
        strips result `shouldParse` [LId () "f", LOp () "#", LId () "not"]
    it "tests of 16KB is two separate atoms" $ 
        let result = parse lMica "" "16KB" in 
        strips result `shouldParse` [LInt () 16, LId () "KB"]
    it "can lex a comment sandwich followed by a preprocessor directive" $
        let result = parse lMica "" "x = 10 // end of line \n #define Y 20" in 
        strips result `shouldParse` [
            LId () "x", LOp () "=", LInt () 10, LPre () "define Y 20"]
    it "can lex a comment sandwich at the beginning of the file" $ 
        let result = parse lMica "" " // end of line \n #define Y 20\n#" in 
        strips result `shouldParse` [LPre () "define Y 20", LPre () ""]
    it "it can lex an empty preprocessor directive" $ 
        let result = parse lMica "" "#hi\n#\n#yo" in 
        strips result `shouldParse` [LPre () "hi", LPre () "", LPre () "yo"]
    it "properly lexes a paren block" $ 
        let result = parse lMica "" "( x )" in 
        strips result `shouldParse` [LDelim () '(', LId () "x", LDelim () ')']
    it "ignores white space at the end of the file" $ 
        let result = parse lMica "" "\n#hi\n  \n" in
        strips result `shouldParse` [LPre () "hi"] 

test :: IO ()
test = hspec $ lexerTest1 >> lexerTest2