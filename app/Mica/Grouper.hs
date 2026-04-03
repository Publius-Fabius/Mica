{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Mica.Grouper where 

import Mica.Lexer
import Text.Megaparsec
import Data.Void
import qualified Data.List.NonEmpty as NE

data LexTree a =
    LParen [LexTree a] |          -- Everything inside ( )
    LCurly [LexTree a] |          -- Everything inside { }
    LBrack [LexTree a] |          -- Everything inside [ ]
    LBranch [LexTree a] |         -- White space and ';'
    LLeaf (MToken a)
    deriving (Show, Eq, Ord, Functor)

type Grouper = Parsec Void [MToken SP]

isDelim :: Char -> MToken a -> Bool
isDelim d (LDelim _ x) = x == d
isDelim _ _ = False

gDelim :: Char -> Grouper (MToken SP)
gDelim d = satisfy (isDelim d)

gParen :: Grouper (LexTree SP)
gParen = LParen <$> between (gDelim '(') (gDelim ')') (many gTerm)

gCurly :: Grouper (LexTree SP)
gCurly = LCurly <$> between (gDelim '{') (gDelim '}') gBlock

gBrack :: Grouper (LexTree SP)
gBrack = LBrack <$> between (gDelim '[') (gDelim ']') (many gTerm)

isLeaf :: MToken a -> Bool
isLeaf (LDelim _ _) = False
isLeaf (LPre _ _) = False
isLeaf _          = True

gLeaf :: Grouper (LexTree SP)
gLeaf = LLeaf <$> satisfy isLeaf

gTerm :: Grouper (LexTree SP)
gTerm = choice [
    gParen,
    gCurly,
    gBrack,
    gLeaf ]

gStmt :: Grouper [LexTree SP]
gStmt = some gTerm <* satisfy (isDelim ';')

isPre :: MToken a -> Bool
isPre (LPre _ _) = True
isPre _ = False

gPre :: Grouper (LexTree SP)
gPre =  LLeaf <$> satisfy isPre

gBlock:: Grouper [LexTree SP]
gBlock = many $ LBranch <$> (((:[]) <$> gPre) <|> gStmt)

prettyMToken :: MToken a -> String
prettyMToken (LId _ s) = s 
prettyMToken (LInt _ i) = show i
prettyMToken (LDbl _ d) = show d
prettyMToken (LStr _ s) = "\"" ++ s ++ "\""
prettyMToken (LOp _ s) = s 
prettyMToken (LDelim _ c) = [c]
prettyMToken (LPre _ s) = "#" ++ s

mTokenPos :: MToken a -> a 
mTokenPos (LId p _) = p
mTokenPos (LInt p _) = p
mTokenPos (LDbl p _) = p
mTokenPos (LStr p _) = p
mTokenPos (LOp p _) = p
mTokenPos (LDelim p _) = p
mTokenPos (LPre p _) = p

instance VisualStream [MToken SP] where 
     showTokens _ = unwords . map prettyMToken . NE.toList 
    
instance TraversableStream [MToken SP] where 
    reachOffsetNoLine o pst = 
        let offsetDiff = o - pstateOffset pst in
        let tokensLeft = drop offsetDiff (pstateInput pst) in
        case tokensLeft of
            []    -> pst
            (t:_) -> pst { 
                    pstateOffset = o,
                    pstateSourcePos = mTokenPos t }