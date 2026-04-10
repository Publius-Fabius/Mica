{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Mica.Lexer where

import Data.Text
import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.List.NonEmpty as NE
import Control.Monad

type SP = SourcePos 

data MToken p = 
    LId p Text |                -- Identifier
    LInt p Integer |            -- Integer
    LDbl p Double |             -- Floating Point Number
    LStr p Text |               -- UTF8 String Literal
    LChar p Char |              -- Char Literal
    LOp p Text |                -- Any combo (sans ws) of !#$%&*+./<=>?@\^|-~
    LDelim p Char |             -- (){}[];
    LPre p Text
    deriving (Show, Eq, Ord, Functor)
    
type Lexer = Parsec Void Text
isNextEnd = lookAhead (void eol) <|> lookAhead eof

mTokenPos :: MToken a -> a 
mTokenPos (LId p _) = p
mTokenPos (LInt p _) = p
mTokenPos (LDbl p _) = p
mTokenPos (LStr p _) = p
mTokenPos (LOp p _) = p
mTokenPos (LDelim p _) = p
mTokenPos (LPre p _) = p

prettyMToken :: MToken a -> Text
prettyMToken (LId _ s) = s 
prettyMToken (LInt _ i) = pack $ show i
prettyMToken (LDbl _ d) = pack $ show d
prettyMToken (LStr _ s) = "\"" <> s <> "\""
prettyMToken (LOp _ s) = s 
prettyMToken (LDelim _ c) = Data.Text.singleton c
prettyMToken (LPre _ s) = "#" <> s

lPre :: Lexer (MToken SP)
lPre = do 
    pos <- getSourcePos 
    char '#'
    LPre pos . pack <$> manyTill anySingle isNextEnd

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
    takeWhileP (Just "string lit char") 
    (\c -> c /= '\n' && c /= '"') <* 
    char '"'
    
lChar :: Lexer (MToken SP) 
lChar = lexeme LChar $ char '\'' *> anySingle <* char '\'' 

lDelim :: Lexer (MToken SP)
lDelim = lexeme LDelim (oneOf ("(){}[]"::String))

lOp :: Lexer (MToken SP)
lOp = lexeme LOp (
    takeWhile1P (Just "op char") $ 
    (\c -> Data.Text.any (c==) "~!@#$%^&*-+=|\\:?/.>,<;"))

lId :: Lexer (MToken SP)
lId = do
    sp <- getSourcePos
    first <- letterChar <|> char '_'
    rest <- takeWhileP 
        (Just "alphanumeric or _") 
        (\c -> isAlphaNum c || c == '_')
    pure $ LId sp (Data.Text.singleton first <> rest)

lNumber :: Lexer (MToken SP)
lNumber = try lDbl <|> lInt

lToken :: Lexer (MToken SP)
lToken = try $ lPreBody <|> (vsc >> choice [
    lNumber, 
    lStr, 
    lChar, 
    lDelim, 
    lOp,
    lId])

lMica :: Lexer [MToken SP]
lMica = 
    ((:) <$> lPreHead <*> many lToken) <|> many lToken <*
    vsc <*
    eof
