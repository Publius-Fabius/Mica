{-# LANGUAGE OverloadedStrings #-}

module TestLexer where 

import Mica.Lexer 
import Text.Megaparsec.Error

test1 :: IO () 
test1 = do
    case runLexer "" "x y z" of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "x" : LId _ "y" : LId _ "z" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "x 2.5 z" of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "x" : LDbl _ 2.5 : LId _ "z" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "x 567 z" of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "x" : LInt _ 567 : LId _ "z" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "x /* y */ z" of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "x" :  LId _ "z" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "x // a s d" of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "x" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "x \"xyz\" 4" of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "x" : LStr _ "xyz" : LInt _ 4 : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "x { }" of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "x" : LDelim _ '{' : LDelim _ '}' : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "x >= ** " of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "x" : LOp _ ">=" : LOp _ "**" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "#hi" of 
        Left err -> error $ errorBundlePretty err 
        Right (LPre _ "hi" :  []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "#hi\n  #yo" of 
        Left err -> error $ errorBundlePretty err 
        Right (LPre _ "hi" : LPre _ "yo" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "#hi\n /* */ #yo" of 
        Left err -> error $ errorBundlePretty err 
        Right (LPre _ "hi" : LPre _ "yo" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "#hi\n // \n #yo" of 
        Left err -> error $ errorBundlePretty err 
        Right (LPre _ "hi" : LPre _ "yo" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "#hi\n // \nff" of 
        Left err -> error $ errorBundlePretty err 
        Right (LPre _ "hi" : LId _ "ff" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "f\n // \n#ya" of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "f" : LPre _ "ya" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "f\n // \n#ya \n /* */ #g" of 
        Left err -> error $ errorBundlePretty err 
        Right (LId _ "f" : LPre _ "ya " : LPre _ "g" : []) -> return () 
        _ -> error "lex pattern mismatch"
    case runLexer "" "#hi\n#yo" of 
        Right [LPre _ "hi", LPre _ "yo"] -> return ()
        _ -> error "Failed: EOF directive"
    case runLexer "" "f\n#define X 1" of 
        Right [LId _ "f", LPre _ "define X 1"] -> return ()
        _ -> error "Failed: Trailing directive"
    case runLexer "" "#hi\n   #indented" of 
        Right [LPre _ "hi", LPre _ "indented"] -> return ()
        _ -> error "Failed: Indented directive"
    case runLexer "" "f #not_a_directive" of 
        Left err -> error $ errorBundlePretty err
        (Right [LId _ "f", LOp _ "#", LId _ "not_a_directive"]) -> return ()
        _ -> error "lex pattern mismatch"
    case runLexer "" "16KB" of 
        Right [LInt _ 16, LId _ "KB"] -> return () -- If they are separate atoms
        _ -> error "Failed: Numeric suffix check"
    case runLexer "" "x = 10 // end of line \n #define Y 20" of 
        Right [LId _ "x", LOp _ "=", LInt _ 10, LPre _ "define Y 20"] -> return ()
        _ -> error "Failed: Comment sandwich check"
    case runLexer "" " // end of line \n #define Y 20\n#" of 
        Right [LPre _ "define Y 20", LPre _ ""] -> return ()
        _ -> error "Failed: preHeader"
    case runLexer "" "#hi\n#\n#yo" of 
        Right [LPre _ "hi", LPre _ "", LPre _ "yo"] -> return ()
        _ -> error "Failed: Empty directive check"
    case runLexer "" "_internal_var_ + status_" of 
        Right [LId _ "_internal_var_", LOp _ "+", LId _ "status_"] -> return ()
        _ -> error "Failed: Underscore support"   

test :: IO ()
test = do 
    putStrLn "Testing..."
    test1