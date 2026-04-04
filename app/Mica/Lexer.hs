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
    
type Lexer = Parsec Void Text
isNextEnd = lookAhead (void eol) <|> lookAhead eof

lPre :: Lexer (MToken SP)
lPre = do 
    pos <- getSourcePos 
    char '#'
    LPre pos <$> manyTill anySingle isNextEnd

lPreHead :: Lexer (MToken SP)
lPreHead = try $ vsc *> lPre

lPreBody :: Lexer (MToken SP)
lPreBody = try $ eol *> vsc *> lPre

vsc :: Lexer () 
vsc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

hsc :: Lexer ()
hsc = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

lexeme :: (SP -> a -> MToken SP) -> Lexer a -> Lexer (MToken SP)
lexeme con par = (con <$> getSourcePos <*> par) <* hsc

lDbl :: Lexer (MToken SP)
lDbl = lexeme LDbl L.float

lInt :: Lexer (MToken SP)
lInt = lexeme LInt L.decimal

lStr :: Lexer (MToken SP)
lStr = lexeme LStr $ 
    char '"' *> 
    many (satisfy (\c -> c /= '\n' && c /= '"')) <* 
    char '"'
    
lDelim :: Lexer (MToken SP)
lDelim = lexeme LDelim (oneOf ("(){}[];"::String))

lOp :: Lexer (MToken SP)
lOp = lexeme LOp (some $ oneOf ("~!@#$%^&*-+=|\\:?/.>,<"::String))

lId :: Lexer (MToken SP)
lId = lexeme LId ((:) <$> 
    (letterChar <|> char '_') <*> 
    many (alphaNumChar <|> char '_'))

lNumber :: Lexer (MToken SP)
lNumber = try lDbl <|> lInt

lToken :: Lexer (MToken SP)
lToken = try $ lPreBody <|> (vsc >> choice [
    lNumber, 
    lStr, 
    lDelim, 
    lOp,
    lId])

lMica :: Lexer [MToken SP]
lMica = 
    ((:) <$> lPreHead <*> many lToken) <|> many lToken <*
    vsc <*
    eof
