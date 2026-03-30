{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies      #-}

module Mica.Lexer where

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
-- import qualified Data.List.NonEmpty as NE

data MToken = 
    TIden SourcePos String |    -- Identifier
    TInt SourcePos Integer |    -- Integer
    TDbl SourcePos Double |     -- Floating Point Number
    TStr SourcePos String |     -- UTF8 String Literal
    TOp SourcePos String |      -- Any combo (sans ws) of !#$%&*+./<=>?@\^|-~
    TDelim SourcePos Char       -- (){}[];
    deriving (Show, Eq, Ord)

data LexTree =
    TParen [LexTree] |          -- Everything inside ( )
    TCurly [LexTree] |          -- Everything inside { }
    TBrack [LexTree] |          -- Everything inside [ ]
    TBranch [LexTree] |         -- White space and ';'
    TLeaf MToken
    deriving (Show, Eq)
    
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/") 

lexeme :: (SourcePos -> a -> MToken) -> Parser a -> Parser MToken
lexeme c p = L.lexeme sc (c <$> getSourcePos <*> p)

lDouble :: Parser MToken
lDouble = lexeme TDbl L.float

lInt :: Parser MToken
lInt = lexeme TInt L.decimal

lString :: Parser MToken
lString = lexeme TStr (char '"' *> manyTill L.charLiteral (char '"'))

lDelim :: Parser MToken
lDelim = lexeme TDelim (oneOf ("(){}[];"::String))

lOp :: Parser MToken
lOp = lexeme TOp (some $ oneOf ("!#$%&*+./<=>?@\\^|-~"::String))

lIden :: Parser MToken
lIden = lexeme TIden ((:) <$> letterChar <*> many alphaNumChar)

lToken :: Parser MToken
lToken = choice [ lNumber, lString, lDelim, lOp, lIden ]

lNumber :: Parser MToken
lNumber = try lDouble <|> lInt

lMica :: Parser [MToken]
lMica = sc *> many lToken <* eof

type Grouper = Parsec Void [MToken]

isDelim :: Char -> MToken -> Bool
isDelim d (TDelim _ x) = x == d
isDelim _ _ = False

gDelim :: Char -> Grouper MToken
gDelim d = satisfy (isDelim d) <?> ("delimiter " ++ [d])

gParen :: Grouper LexTree
gParen = TParen <$> between (gDelim '(') (gDelim ')') gNodes

gCurly :: Grouper LexTree
gCurly = TCurly <$> between (gDelim '{') (gDelim '}') gBlock

gBrack :: Grouper LexTree
gBrack = TBrack <$> between (gDelim '[') (gDelim ']') gNodes

notDelim :: MToken -> Bool
notDelim (TDelim _ _) = False
notDelim _          = True

gNotDelim :: Grouper MToken
gNotDelim = satisfy notDelim <?> ("not delimiter")

gLeaf :: Grouper LexTree 
gLeaf = TLeaf <$> gNotDelim 

gNode :: Grouper LexTree
gNode = choice [
    gParen,
    gCurly,
    gBrack,
    gLeaf ]

gNodes :: Grouper [LexTree]
gNodes = many gNode

gBlock:: Grouper [LexTree]
gBlock = (TBranch <$> gNodes) `sepEndBy` (gDelim ';')

gMica :: Grouper [LexTree]
gMica = gBlock <* eof

type ParserError = ParseErrorBundle Text Void

runLexer :: String -> Text -> Either ParserError [MToken]
runLexer = runParser lMica

type GrouperError = ParseErrorBundle [MToken] Void

runGrouper :: String -> [MToken] -> Either GrouperError [LexTree]
runGrouper = runParser gMica
