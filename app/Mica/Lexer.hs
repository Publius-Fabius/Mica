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
    LId SourcePos String |      -- Identifier
    LInt SourcePos Integer |    -- Integer
    LDbl SourcePos Double |     -- Floating Point Number
    LStr SourcePos String |     -- UTF8 String Literal
    LOp SourcePos String |      -- Any combo (sans ws) of !#$%&*+./<=>?@\^|-~
    LDelim SourcePos Char |     -- (){}[];
    LPre SourcePos String
    deriving (Show, Eq, Ord)

data LexTree =
    LParen [LexTree] |          -- Everything inside ( )
    LCurly [LexTree] |          -- Everything inside { }
    LBrack [LexTree] |          -- Everything inside [ ]
    LBranch [LexTree] |         -- White space and ';'
    LLeaf MToken
    deriving (Show, Eq)
    
type Parser = Parsec Void Text

isNextEnd = lookAhead (eol >> pure ()) <|> lookAhead eof

lPre :: Parser MToken
lPre = do 
    pos <- getSourcePos 
    char '#'
    LPre pos <$> manyTill anySingle isNextEnd

lPreHeader :: Parser MToken 
lPreHeader = try $ vsc >> lPre

lPreBody :: Parser MToken 
lPreBody = try $ eol >> vsc >> lPre

vsc :: Parser () 
vsc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

hsc :: Parser ()
hsc = L.space hspace1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/") 

lexeme :: (SourcePos -> a -> MToken) -> Parser a -> Parser MToken
lexeme con par = (con <$> getSourcePos <*> par) <* hsc

lDouble :: Parser MToken
lDouble = lexeme LDbl L.float

lInt :: Parser MToken
lInt = lexeme LInt L.decimal

lString :: Parser MToken
lString = lexeme LStr (char '"' *> manyTill L.charLiteral (char '"'))

lDelim :: Parser MToken
lDelim = lexeme LDelim (oneOf ("(){}[];"::String))

lOp :: Parser MToken
lOp = lexeme LOp (some $ oneOf ("~!@#$%^&*-+=|\\:?/.>,<"::String))

lIden :: Parser MToken
lIden = lexeme LId ((:) <$> 
    (letterChar <|> char '_') <*> 
    many (alphaNumChar <|> char '_'))

lNumber :: Parser MToken
lNumber = try lDouble <|> lInt

lToken :: Parser MToken
lToken = do 
    mp <- optional lPreBody
    case mp of 
        Just p -> pure p 
        Nothing -> vsc >> choice [ lNumber, lString, lDelim, lOp, lIden ]

lMica :: Parser [MToken]
lMica = do 
    hdr <- optional lPreHeader
    toks <- case hdr of 
        Just h -> ((h:) <$> many lToken) 
        Nothing -> many lToken 
    vsc >> eof
    pure toks
    
type Grouper = Parsec Void [MToken]

isDelim :: Char -> MToken -> Bool
isDelim d (LDelim _ x) = x == d
isDelim _ _ = False

gDelim :: Char -> Grouper MToken
gDelim d = satisfy (isDelim d) <?> ("delimiter " ++ [d])

gParen :: Grouper LexTree
gParen = LParen <$> between (gDelim '(') (gDelim ')') gNodes

gCurly :: Grouper LexTree
gCurly = LCurly <$> between (gDelim '{') (gDelim '}') gBlock

gBrack :: Grouper LexTree
gBrack = LBrack <$> between (gDelim '[') (gDelim ']') gNodes

notDelim :: MToken -> Bool
notDelim (LDelim _ _) = False
notDelim _          = True

gNotDelim :: Grouper MToken
gNotDelim = satisfy notDelim <?> ("not delimiter")

gLeaf :: Grouper LexTree 
gLeaf = LLeaf <$> gNotDelim 

gNode :: Grouper LexTree
gNode = choice [
    gParen,
    gCurly,
    gBrack,
    gLeaf ]

gNodes :: Grouper [LexTree]
gNodes = many gNode

gBlock:: Grouper [LexTree]
gBlock = (LBranch <$> gNodes) `sepEndBy` (gDelim ';')

gMica :: Grouper [LexTree]
gMica = gBlock <* eof

type ParserError = ParseErrorBundle Text Void

runLexer :: String -> Text -> Either ParserError [MToken]
runLexer = runParser lMica

type GrouperError = ParseErrorBundle [MToken] Void

runGrouper :: String -> [MToken] -> Either GrouperError [LexTree]
runGrouper = runParser gMica
 