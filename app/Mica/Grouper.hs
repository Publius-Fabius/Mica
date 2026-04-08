{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Mica.Grouper where 

import Mica.Lexer
import Text.Megaparsec
import Data.Void
import qualified Data.List.NonEmpty as NE

data LexTree a =
    LParen a [LexTree a] |          -- Everything inside ( )
    LCurly a [LexTree a] |          -- Everything inside { }
    LBrack a [LexTree a] |          -- Everything inside [ ]
    LLeaf (MToken a)
    deriving (Show, Eq, Ord, Functor)

type Grouper = Parsec Void [MToken SP]

gDelim :: Char -> Grouper (MToken SP)
gDelim d = satisfy $ \case 
    (LDelim _ x) -> x == d 
    _ -> False

gTerm :: Grouper (LexTree SP) 
gTerm = try $ anySingle >>= \case 
    LDelim p '(' -> LParen p <$> (many gTerm <* gDelim ')')
    LDelim p '{' -> LCurly p <$> (many gTerm <* gDelim '}')
    LDelim p '[' -> LBrack p <$> (many gTerm <* gDelim ']') 
    LDelim p _ -> fail "expected gTerm, got endBy delimiter"
    token -> pure $ LLeaf token

gMica :: Grouper [LexTree SP] 
gMica = (many gTerm) <* eof 

prettyMToken :: MToken a -> String
prettyMToken (LId _ s) = s 
prettyMToken (LInt _ i) = show i
prettyMToken (LDbl _ d) = show d
prettyMToken (LStr _ s) = "\"" ++ s ++ "\""
prettyMToken (LOp _ s) = s 
prettyMToken (LDelim _ c) = [c]
prettyMToken (LPre _ s) = "#" ++ s

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