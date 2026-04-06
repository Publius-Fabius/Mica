{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Mica.Parser where 
import Mica.Grouper
import Mica.Lexer
import Mica.Type
import Text.Megaparsec
import Data.Void
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

type Parser = Parsec Void [LexTree SP]

pRawIden :: Parser String 
pRawIden = try $ anySingle >>= \case  
    LLeaf (LId p s) -> pure s 
    _ -> fail "expected a raw identifier"

pRawStrLit :: Parser String 
pRawStrLit = try $ anySingle >>= \case  
    LLeaf (LStr p s) -> pure s 
    _ -> fail "expected a raw string literal identifier"

pExpectOp :: String -> Parser SP 
pExpectOp name = try $ anySingle >>= \case
    LLeaf (LOp p s) -> if name == s 
        then pure p 
        else fail $ "expected " ++ name ++ ", instead got " ++ s
    _ -> fail $ "expected operator " ++ name

pExpectIden :: String -> Parser SP 
pExpectIden iden = try $ anySingle >>= \case
    LLeaf (LId p i) -> if iden == i
        then pure p 
        else fail $ "expected " ++ iden ++ ", instead got " ++ i
    _ -> fail $ "expected identifier " ++ iden

pIden :: (SP -> String -> a) -> Parser a
pIden f = try $ anySingle >>= \case 
    LLeaf (LId p i) -> pure $ f p i 
    _ -> fail "expected an identifier" 

pReg :: Parser (Reg SP) 
pReg = 
    (Stack <$> pExpectIden "Stack") <|> 
    (Heap <$> pExpectIden "Heap") <|>
    (Data <$> pExpectIden "Data") <|>
    (Data <$> pExpectIden "Local") 

pAcc :: Parser (Acc SP)
pAcc = 
    (Read <$> pExpectIden "R") <|> 
    (Write <$> pExpectIden "W")

recurse :: [LexTree SP] -> Parser a -> Parser a 
recurse ts p = case parse (p <* eof) "" ts of 
    Right a -> pure a 
    Left err -> parseError (NE.head (bundleErrors err))

pParen :: Parser a -> Parser a 
pParen p = try $ anySingle >>= \case 
    LParen _ as -> recurse as p
    _ -> fail "expected a parentheses"

pCurly :: Parser a -> Parser a 
pCurly p = try $ anySingle >>= \case 
    LCurly _ as -> recurse as p 
    _ -> fail "expected a curly brackets"

pBrack :: Parser a -> Parser a 
pBrack p = try $ anySingle >>= \case 
    LBrack _ as -> recurse as p
    _ -> fail "expected a square brackets"

isCtrlOp :: LexTree SP -> Bool 
isCtrlOp (LLeaf (LOp _ ";")) = True 
isCtrlOp (LLeaf (LOp _ "=>")) = True
isCtrlOp (LLeaf (LOp _ "=")) = True
isCtrlOp (LLeaf (LOp _ ",")) = True
isCtrlOp _ = False

liftPrimary :: MToken SP -> Parser (Exp SP) 
liftPrimary (LId p v) = pure $ Iden p v
liftPrimary (LInt p v) = pure $ IntLit p v
liftPrimary (LDbl p v) = pure $ DblLit p v 
liftPrimary (LStr p v) = pure $ StrLit p v 
liftPrimary (LChar p v) = pure $ CharLit p v
liftPrimary t = fail $ "bad primary type " ++ show t

pArg :: Parser (Arg SP)
pArg = Arg <$> pIden Name <*> optional (pExpectOp ":" *> pExp)

pBody :: Parser (Body SP) 
pBody = 
    (Compound <$> pCurly pBlockStmt) <|> 
    Inline <$> pExp <* pExpectOp ";"

pLam :: SP -> Parser (Exp SP) 
pLam = undefined 

pAtom :: Parser (Exp SP)
pAtom = try $ satisfy (not . isCtrlOp) >>= \case 
    LParen sp as -> Paren sp <$> recurse as pExp 
    LBrack p as -> Brack p <$> recurse as pExp
    LLeaf (LId p "lam") -> pLam p 
    LLeaf (LOp p op) -> Una p op <$> pAtom
    LLeaf token -> liftPrimary token
    LCurly _ _ -> fail "expected an atom, instead got curly"

pExp :: Parser (Exp SP)
pExp = pAtom >>= pExpNxt 

pExpNxt :: Exp SP -> Parser (Exp SP) 
pExpNxt lx = tryNext <|> pure lx where 

    tryNext = try $ satisfy (not . isCtrlOp) >>= \case 

        LParen sp as -> 
            (recurse as pExp >>= insertAppLR lx sp . Paren sp) >>= pExpNxt

        LBrack sp as -> 
            (recurse as pExp >>= insertAppLR lx sp . Brack sp) >>= pExpNxt

        LLeaf (LId sp "lam") -> 
            pLam sp >>= insertAppLR lx sp >>= pExpNxt

        LLeaf (LOp sp op) -> opInfo op >>= \case 
            (prec, "U") -> 
                (pAtom >>= insertAppLR lx sp . Una sp op) >>= pExpNxt
            (prec, "B") -> 
                pAtom >>= insertLR lx sp op prec >>= pExpNxt 

        LLeaf t -> 
            liftPrimary t >>= insertAppLR lx (mTokenPos t) >>= pExpNxt

        LCurly _ _ -> fail "expected an exp, instead got curly bracket"
   
insertAppLR :: Exp SP -> SP -> Exp SP -> Parser (Exp SP)
insertAppLR lx sp rx = try $ do 
    (prec, _) <- opInfo " "
    insertLR lx sp " " prec rx

insertLR :: Exp SP -> SP -> String -> Int -> Exp SP -> Parser (Exp SP)
insertLR lx@(Bin lsp lop ll lr) rsp rop rprec rx = do 
    (lprec,_) <- opInfo lop 
    if rprec > lprec  
        then Bin lsp lop ll <$> insertLR lr rsp rop rprec rx
        else pure $ Bin rsp rop lx rx
insertLR lx@(Una lsp lop lr) rsp rop rprec rx = do 
    (lprec,_) <- opInfo lop 
    if rprec > lprec 
        then Una lsp lop <$> insertLR lr rsp rop rprec rx
        else pure $ Bin rsp rop lx rx
insertLR lx sp op rprec rx = pure $ Bin sp op lx rx

pFor :: SP -> Parser (BlockStmt SP) 
pFor p = header <*> pBlockStmt where
    header =
        pParen $ For p <$>
        pBlockStmt <*> 
        (pExpectOp ";" *> pExp) <*> 
        (pExpectOp ";" *> pBlockStmt)

pCase :: Parser (Case SP) 
pCase = Case <$> 
    (pIden Name) <*> 
    many (pIden Name) <*>
    pBlockStmt

pBlockStmt :: Parser (BlockStmt SP) 
pBlockStmt = explicitStmt <|> (Assign <$> pExp <* pExpectOp ";") where 

    explicitStmt = try $ anySingle >>= \case 

        LLeaf (LId p "if")-> 
            If p <$> pParen pExp <*> pBlockStmt 
        
        LLeaf (LId p "else") -> 
            Else p <$> pBlockStmt
        
        LLeaf (LId p "while") -> 
            While p <$> pParen pExp <*> pBlockStmt 
        
        LLeaf (LId p "do") -> DoWhile p <$> 
            pCurly pBlockStmt <*> 
            (pExpectIden "while" *> pParen pExp)
        
        LLeaf (LId p "return") -> 
            Ret p <$> optional pExp <* pExpectOp ";"
        
        LLeaf (LId p "continue") -> 
            Cont p <$ pExpectOp ";"
        
        LLeaf (LId p "break") -> 
            Brk p <$ pExpectOp ";"
        
        LCurly p bs -> 
            Block p <$> recurse bs (many pBlockStmt)

        LLeaf (LId p "match") -> 
            Mat p <$> pParen pExp <*> pCurly (many pCase) 

        LLeaf (LId p "for") -> pFor p
        
        LLeaf (LId p "let") -> Let p <$> 
            (pArg <* pExpectOp "=") <*> 
            (pExp <* pExpectOp ";")
        
        _ -> fail "expected an explicit block statement"

pMemb :: Parser (Memb SP)
pMemb = Memb <$> pIden Name <* pExpectOp ":" <*> pExp

pDefine = undefined 

pFileStmt :: Parser (FileStmt SP)
pFileStmt = try $ anySingle >>= \ case

    LLeaf (LId p "import") -> 
        Imp p <$> pRawStrLit 

    LLeaf (LId p "include") -> 
        Inc p <$> pRawStrLit 

    LLeaf (LId p "sum") -> Sum p <$> 
        pIden Name <*> 
        many (pIden Name) <*> 
        pCurly (pMemb `sepEndBy` pExpectOp ";") 

    LLeaf (LId p "record") -> Rec p <$> 
        pIden Name <*> 
        many (pIden Name) <*> 
        pCurly (pMemb `sepEndBy` pExpectOp ";") 

    LLeaf (LId p "judge") -> Jdg p <$> 
        (pIden Name <* pExpectOp ":") <*> 
        (pExp <* pExpectOp ";") 

    LLeaf (LId p "map") -> Map p <$> 
        pIden Name <*> 
        (pArg `sepBy` pExpectOp ",") <*>
        (pExpectOp "=" *> pBody) 

    LLeaf (LId p "define") -> pDefine p

    _ -> fail "expected a file statement"

pMica :: Parser [FileStmt SP]
pMica = (many pFileStmt) <* eof

prettyLexTree :: LexTree a -> String 
prettyLexTree (LParen _ ts) = "(" ++ unwords (map prettyLexTree ts) ++ ")"
prettyLexTree (LCurly _ ts) = "{" ++ unwords (map prettyLexTree ts) ++ "}"
prettyLexTree (LBrack _ ts) = "[" ++ unwords (map prettyLexTree ts) ++ "]"
prettyLexTree (LLeaf l) = prettyMToken l

lexTreePos :: LexTree SP -> SP
lexTreePos (LParen sp _) = sp
lexTreePos (LCurly sp _) = sp
lexTreePos (LBrack sp _) = sp
lexTreePos (LLeaf l) = mTokenPos l

instance VisualStream [LexTree SP] where 
     showTokens _ = unwords . map prettyLexTree . NE.toList 
    
instance TraversableStream [LexTree SP] where 
    reachOffsetNoLine o pst = 
        let offsetDiff = o - pstateOffset pst in
        let tokensLeft = drop offsetDiff (pstateInput pst) in
        case tokensLeft of
            []    -> pst
            (t:_) -> pst { 
                pstateOffset = o,
                pstateSourcePos = lexTreePos t }

opInfo :: String -> Parser (Int, String)
opInfo op = case Map.lookup op precs of 
    Just a -> pure a 
    Nothing -> fail $ "operator not found " ++ op 

precs :: Map.Map String (Int, String) 
precs = Map.fromList precAssocs 

precAssocs :: [(String, (Int, String))]
precAssocs = [ 
    (".", (100, "B")),
    ("@", (95, "U")), 
    ("~", (90, "U")),
    ("!", (80, "U")),
    (" ", (77, "B")),
    ("<<", (75, "B")), (">>", (75, "B")), 
    ("*", (70, "B")), ("/", (70, "B")), 
    ("+", (60, "B")), ("-", (60, "B")), 
    ("&", (50, "B")), 
    ("^", (45, "B")), 
    ("|", (40, "B")), 
    ("<", (35, "B")), (">", (35, "B")), 
    ("<=", (35, "B")), (">=", (35, "B")), 
    ("==", (30, "B")), ("!=", (30, "B")), 
    ("&&", (20, "B")), ("||", (20, "B")), 
    (",", (10, "B")),
    ("$", (5, "B")),
    (":=", (0, "B")),
    ("+=", (0, "B")),
    ("-=", (0, "B")),
    ("*=", (0, "B")),
    ("^=", (0, "B")),
    ("|=", (0, "B")),
    ("&=", (0, "B")),
    ("%=", (0, "B"))]
