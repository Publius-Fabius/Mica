{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Mica.Lexer where

import Data.Text (Text)
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.List.NonEmpty as NE
import Control.Monad

type SP = SourcePos 

data MToken p = 
    LId p String |              -- Identifier
    LInt p Integer |            -- Integer
    LDbl p Double |             -- Floating Point Number
    LStr p String |             -- UTF8 String Literal
    LOp p String |              -- Any combo (sans ws) of !#$%&*+./<=>?@\^|-~
    LDelim p Char |             -- (){}[];
    LPre p String
    deriving (Show, Eq, Ord, Functor)
    
type Parser = Parsec Void Text
isNextEnd = lookAhead (void eol) <|> lookAhead eof

lPre :: Parser (MToken SP)
lPre = do 
    pos <- getSourcePos 
    char '#'
    LPre pos <$> manyTill anySingle isNextEnd

lPreHead :: Parser (MToken SP)
lPreHead = try $ vsc *> lPre

lPreBody :: Parser (MToken SP)
lPreBody = try $ eol *> vsc *> lPre

vsc :: Parser () 
vsc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

hsc :: Parser ()
hsc = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

lexeme :: (SP -> a -> (MToken SP)) -> Parser a -> Parser (MToken SP)
lexeme con par = (con <$> getSourcePos <*> par) <* hsc

lDbl :: Parser (MToken SP)
lDbl = lexeme LDbl L.float

lInt :: Parser (MToken SP)
lInt = lexeme LInt L.decimal

lStr :: Parser (MToken SP)
lStr = lexeme LStr $ 
    char '"' *> 
    (many $ satisfy (\c -> c /= '\n' && c /= '"')) <* 
    char '"'
    
lDelim :: Parser (MToken SP)
lDelim = lexeme LDelim (oneOf ("(){}[];"::String))

lOp :: Parser (MToken SP)
lOp = lexeme LOp (some $ oneOf ("~!@#$%^&*-+=|\\:?/.>,<"::String))

lId :: Parser (MToken SP)
lId = lexeme LId ((:) <$> 
    (letterChar <|> char '_') <*> 
    many (alphaNumChar <|> char '_'))

lNumber :: Parser (MToken SP)
lNumber = try lDbl <|> lInt

lToken :: Parser (MToken SP)
lToken = lPreBody <|> (vsc >> choice [
    lNumber, 
    lStr, 
    lDelim, 
    lOp,
    lId])

lMica :: Parser [MToken SP]
lMica = 
    ((:) <$> lPreHead <*> many lToken) <|> (many lToken) <*
    vsc <*
    eof
