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

-- strip p = (const () <$>) <$> p

-- strips p = map (const () <$>) <$> p

-- runner p txt = case parse lMica "" txt of 
--     Right tokens -> case parse gBlock "" tokens of 
--         Right trees -> parse p "" trees 
--         Left err -> parse (fail (errorBundlePretty err)) "" []
--     Left err -> parse (fail (errorBundlePretty err)) "" []

-- testPrimpary = describe "Parser (Test Primary)" $ do 
--     it "parses an identifier" $
--         let result = runner (pBranch pPrimary) "x;" in 
--             strip result `shouldParse` Iden () "x"
--     it "parses an integer literal" $
--         let result = runner (pBranch pPrimary) "1;" in 
--             strip result `shouldParse` IntLit () 1
--     it "parses a double literal" $
--         let result = runner (pBranch pPrimary) "3.14;" in 
--             strip result `shouldParse` DblLit () 3.14
--     it "parses a string literal" $
--         let result = runner (pBranch pPrimary) "\"hi\";" in 
--             strip result `shouldParse` StrLit () "hi"

-- test :: IO ()
-- test = hspec testPrimpary