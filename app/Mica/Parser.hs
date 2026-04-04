{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Mica.Parser where 
import Mica.Grouper
import Mica.Lexer
import Mica.Type
import Text.Megaparsec
import Data.Void
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

type Parser = Parsec Void [LexTree SP]

-- isLeaf :: LexTree SP -> Bool 
-- isLeaf (LLeaf _) = True 
-- isLeaf _ = False 

-- -- | Parses Int, Dbl, Str, Pre 
-- pPrimary :: Parser (Exp SP)
-- pPrimary = try $ anySingle >>= \case 
--     LLeaf (LId p s) -> pure $ Iden p s  
--     LLeaf (LInt p i) -> pure $ IntLit p i  
--     LLeaf (LDbl p d) -> pure $ DblLit p d
--     LLeaf (LStr p s) -> pure $ StrLit p s
--     _ -> fail "expected a primary node"

-- pRawIden :: Parser String 
-- pRawIden = try $ anySingle >>= \case  
--     LLeaf (LId p s) -> pure s 
--     _ -> fail "expected a raw identifier"

-- pStrLit :: Parser String 
-- pStrLit = try $ anySingle >>= \case  
--     LLeaf (LStr p s) -> pure s 
--     _ -> fail "expected a raw string literal identifier"
    
-- recurse :: [LexTree SP] -> Parser a -> Parser a 
-- recurse ts p = case parse p "" ts of 
--     Right a -> pure a 
--     Left err -> parseError (NE.head (bundleErrors err))

-- isBranch :: LexTree a -> Bool
-- isBranch (LBranch _) = True 
-- isBranch _ = False 

-- pBranch :: Parser a -> Parser a 
-- pBranch p = try $ anySingle >>= \case 
--     LBranch as -> recurse as p 
--     _ -> fail "expected a branch"

-- pParen :: Parser a -> Parser a 
-- pParen p = try $ anySingle >>= \case 
--     LParen as -> recurse as p
--     _ -> fail "expected a parentheses"

-- isCurly :: LexTree a -> Bool 
-- isCurly (LCurly _) = True 
-- isCurly _ = False 

-- pCurly :: Parser a -> Parser a 
-- pCurly p = try $ anySingle >>= \case 
--     LCurly as -> recurse as p 
--     _ -> fail "expected a curly brackets"

-- isBrack :: LexTree a -> Bool 
-- isBrack (LBrack _) = True 
-- isBrack _ = False 

-- pBrack :: Parser a -> Parser a 
-- pBrack p = try $ anySingle >>= \case 
--     LBrack as -> recurse as p
--     _ -> fail "expected a square brackets"

-- pFileStmt :: Parser (FileStmt SP)
-- pFileStmt = try $ anySingle >>= \ case
--     LBranch (LLeaf (LId p "fun") : ns) -> recurse ns (pFun p) 
--     LBranch (LLeaf (LId p "jdg") : ns) -> recurse ns (pJdg p)
--     LBranch (LLeaf (LId p "data") : ns) -> recurse ns (pData p)
--     LBranch (LLeaf (LId p "struct") : ns) -> recurse ns (pStruct p)
--     LBranch (LLeaf (LId p "dec") : ns) -> recurse ns (pDec p)
--     LBranch (LLeaf (LId p "def") : ns) -> recurse ns (pDef p)
--     LBranch (LLeaf (LId p "import") : ns) -> recurse ns (pImport p)
--     _ -> fail "expected a file statement"

-- pExpectedOp :: String -> Parser SP 
-- pExpectedOp name = try $ anySingle >>= \case
--     LLeaf (LOp p s) -> if name == s 
--         then pure p 
--         else fail $ "expected " ++ name ++ ", instead got " ++ s
--     _ -> fail $ "expected operator:" ++ name

-- pExpectedIden :: String -> Parser SP 
-- pExpectedIden iden = try $ anySingle >>= \case
--     LLeaf (LId p i) -> if iden == i
--         then pure p 
--         else fail $ "expected " ++ iden ++ ", instead got " ++ i
--     _ -> fail $ "expected operator:" ++ iden

-- pFun :: SP -> Parser (FileStmt SP)
-- pFun p = 
--     Fun p <$> pRawIden <*> (pParen pExp <* pExpectedOp "=") <*> pExpStmt 

-- pJdg :: SP -> Parser (FileStmt SP) 
-- pJdg p = Jdg p <$> (pRawIden <* pExpectedOp ":") <*> pExp 

-- pMember :: Parser (String, Exp SP)
-- pMember = (,) <$> (pRawIden <* pExpectedOp ":") <*> pExp 

-- pData :: SP -> Parser (FileStmt SP)
-- pData p = 
--     Dat p <$> pRawIden <*> pParen pExp <*> pCurly (many pMember) 
     
-- pStruct :: SP -> Parser (FileStmt SP)
-- pStruct p = 
--     Stru p <$> pRawIden <*> pParen pExp <*> pCurly (many pMember) 

-- pDec :: SP -> Parser (FileStmt SP)
-- pDec p = Dec p <$> 
--     optional (pParen pExp) <*> (pRawIden <* pExpectedOp "=") <*> pExp 

-- pDef :: SP -> Parser (FileStmt SP)
-- pDef p = Def p <$> pRawIden <*> pExp 

-- pImport :: SP -> Parser (FileStmt SP)
-- pImport p = Imp p <$> pStrLit 

-- pBlockStmt :: Parser (BlockStmt SP) 
-- pBlockStmt = try $ anySingle >>= \case 
--     LBranch (LLeaf (LId p "if") : ns) -> recurse ns (pIf p) 
--     LBranch (LLeaf (LId p "else") : ns) -> recurse ns (pElse p)
--     LBranch (LLeaf (LId p "while") : ns) -> recurse ns (pWhile p)
--     LBranch (LLeaf (LId p "do") : ns) -> recurse ns (pDoWhile p)
--     LBranch (LLeaf (LId p "for") : ns) -> recurse ns (pFor p)
--     LBranch (LLeaf (LId p "return") : ns) -> recurse ns (pRet p)
--     LBranch (LLeaf (LId p "break") : ns) -> recurse ns (pBrk p) 
--     LBranch (LLeaf (LId p "let") : ns) -> recurse ns (pLet p)
--     LBranch (LLeaf (LId p "match") : ns) -> recurse ns (pMat p)
--     LBranch ns -> recurse ns pSet
--     _ -> fail "expected a block statement"

-- pIf :: SP -> Parser (BlockStmt SP) 
-- pIf p = If p <$> pParen pExp <*> pExpStmt 
    
-- pElse :: SP -> Parser (BlockStmt SP) 
-- pElse p = Else p <$> pExpStmt

-- pWhile :: SP -> Parser (BlockStmt SP) 
-- pWhile p = While p <$> pParen pExp <*> pExpStmt  

-- pDoWhile :: SP -> Parser (BlockStmt SP) 
-- pDoWhile p =  
--     DoWhile p <$> pExpStmt <*> (pExpectedIden "while" *> pParen pExp)

-- pFor :: SP -> Parser (BlockStmt SP) 
-- pFor p = For p <$> pParen pExp <*> pExpStmt 

-- pRet :: SP -> Parser (BlockStmt SP) 
-- pRet p = Ret p <$> optional pExp  

-- pBrk :: SP -> Parser (BlockStmt SP) 
-- pBrk = pure . Brk 

-- pSet :: Parser (BlockStmt SP) 
-- pSet = Set <$> pExp

-- pLet :: SP -> Parser (BlockStmt SP) 
-- pLet p = Let p <$> (pRawIden <* pExpectedOp "=") <*> pExp  

-- pMat :: SP -> Parser (BlockStmt SP) 
-- pMat p = Mat p <$> pParen pExp <*> pCurly (many pCase) 

-- pCase :: Parser (String, Exp SP, ExpStmt SP)
-- pCase = (,,) <$> pRawIden <*> pParen pExp <*> pExpStmt 

-- pExpStmt :: Parser (ExpStmt SP)
-- pExpStmt = 
--     try (ExpStmtBlock <$> pCurly (many pBlockStmt)) <|>  
--     try (ExpStmtExp <$> pExp)

-- pExp :: Parser (Exp SP)
-- pExp = undefined 

-- pAtom :: Parser (Exp SP)
-- pAtom = try $ anySingle >>= \case 
--     LParen a -> recurse a pExp 
--     LBrack a -> Brack <$> recurse a pExp
--     LLeaf (LId p "lam") -> pLam p 
--     LLeaf (LOp p op) -> Una p op <$> pAtom
--     LLeaf (LId p s) -> pure $ Iden p s  
--     LLeaf (LInt p i) -> pure $ IntLit p i  
--     LLeaf (LDbl p d) -> pure $ DblLit p d
--     LLeaf (LStr p s) -> pure $ StrLit p s
--     LBranch _ -> fail "expected an atom, instead got a branch"
--     LCurly _ -> fail "expected an atom, instead got a curly brack"
--     LLeaf t -> fail $ "expected an atom, instead got " ++ prettyMToken t

-- pLam :: SP -> Parser (Exp SP) 
-- pLam = undefined 

-- pExpAccum :: Exp a -> Parser a 
-- pExpAccum lx = try $ anySingle >>= \case 
--     LParen as -> recurse as (pExpAccum lx)
--     _ -> undefined
    -- LLeaf (LOp p op) -> do 
    --     rx <- pAtom
    --     ex <- insertLR lx op 

-- insertAppLR :: Exp a -> Exp a -> Parser (Exp a)
-- insertAppLR lx rx = try $ do 
--     (prec, _) <- opInfo " "
--     ex <- insertLR lx (Op ) prec rx
--     pExpAccum ex

-- getSP :: LexTree SP -> SP 
-- getSP (LParen (a:_)) = getSP a 
-- getSP (LCurly (a:_)) = getSP a 
-- getSP (LBrack (a:_)) = getSP a 
-- getSP (LBranch (a:_)) = getSP a 
-- getSP (LLeaf token) = mTokenPos token
-- getSP _ = error "empty "

-- insertLR :: Expr -> String -> Int -> Expr -> ParserF Expr 
-- insertLR lx@(Bin lop ll lr) rop rprec rx = do 
--     (lprec, _) <- getOpInfo lop 
--     if lprec < rprec
--         then Bin lop ll <$> insertLR lr rop rprec rx
--         else pure $ Bin rop lx rx 
-- insertLR lx@(Una lop lr) rop rprec rx = do 
--     (lprec, _) <- getOpInfo lop
--     if lprec < rprec 
--         then Una lop <$> insertLR lr rop rprec rx 
--         else pure $ Bin rop lx rx 
-- insertLR lx rop _ rx = pure $ Bin rop lx rx

-- parseExprAccum :: (Expr, [LexTree]) -> ParserF (Expr, [LexTree]) 
-- parseExprAccum (lx, (LLeaf (LOp _ op1) : l2@(LLeaf (LOp _ op2)) : rs)) = do 
--     (prec, ty) <- getOpInfo op1 
--     case ty of 
--         "U" -> do 
--             (rx, rrs) <- parseAtomU (Una op1) (l2:rs)
--             insertAppAccum lx rrs rx 
--         "B" -> do
--             (rx, rrs) <- parseAtomU (Una op2) rs
--             ex <- insertLR lx op1 prec rx
--             parseExprAccum (ex, rrs)
--         _ -> err $ "Operator must be unary or binary"
-- parseExprAccum (lx, (LLeaf (LOp _ op) : rs)) = do 
--     (prec, ty) <- getOpInfo op 
--     case ty of 
--         "U" -> do 
--             (rx, rrs) <- parseAtomU (Una op) rs 
--             insertAppAccum lx rrs rx 
--         "B" -> do 
--             (rx, rrs) <- parseAtom rs 
--             ex <- insertLR lx op prec rx 
--             parseExprAccum (ex, rrs)
--         _ -> err $ "Operator must be unary or binary"
-- parseExprAccum (lx, (LLeaf tok) : bs) = 
--     parseToken tok >>= insertAppAccum lx bs
-- parseExprAccum (lx, (LBrack as) : bs) =  
--     parseExpr as >>= insertAppAccum lx bs
-- parseExprAccum (lx, (LBranch as) : bs) = 
--     parseExpr as >>= insertAppAccum lx bs
-- parseExprAccum (lx, (LParen as) : bs) = 
--     parseExpr as >>= insertAppAccum lx bs
-- parseExprAccum (lx, a) = pure (lx, a)


-- prettyLexTree :: LexTree a -> String 
-- prettyLexTree (LParen ts) = "(" ++ unwords (map prettyLexTree ts) ++ ")"
-- prettyLexTree (LCurly ts) = "{" ++ unwords (map prettyLexTree ts) ++ "}"
-- prettyLexTree (LBrack ts) = "[" ++ unwords (map prettyLexTree ts) ++ "]"
-- prettyLexTree (LBranch ts) = unwords (map prettyLexTree ts) ++ ";"
-- prettyLexTree (LLeaf l) = prettyMToken l

-- lexTreePos :: LexTree SP -> SP
-- lexTreePos (LParen (a:_)) = lexTreePos a
-- lexTreePos (LCurly (a:_)) = lexTreePos a
-- lexTreePos (LBrack (a:_)) = lexTreePos a
-- lexTreePos (LBranch (a:_)) = lexTreePos a
-- lexTreePos (LLeaf l) = mTokenPos l

-- instance VisualStream [LexTree SP] where 
--      showTokens _ = unwords . map prettyLexTree . NE.toList 
    
-- instance TraversableStream [LexTree SP] where 
--     reachOffsetNoLine o pst = 
--         let offsetDiff = o - pstateOffset pst in
--         let tokensLeft = drop offsetDiff (pstateInput pst) in
--         case tokensLeft of
--             []    -> pst
--             (t:_) -> pst { 
--                 pstateOffset = o,
--                 pstateSourcePos = lexTreePos t }

-- parseExpr :: [LexTree] -> ParserF Expr 
-- parseExpr as = fst <$> (parseAtom as >>= parseExprAccum)


-- parseLambda :: [LexTree] -> ParserF (Expr, [LexTree]) 
-- parseLambda (LLeaf (LId _ region) : LBrack caps : LParen args : body) = 
--     case body of 
--         (LCurly as) : bs -> (\x y z w -> (Lam x y z w, bs)) <$> 
--             parseRegion region <*>
--             parseExpr caps <*> 
--             parseExpr args <*> 
--             (Left <$> parseBlock as)
--         expr -> (\x y z w -> (Lam x y z w, [])) <$> 
--             parseRegion region <*>
--             parseExpr caps <*> 
--             parseExpr args <*> 
--             (Right <$> parseExpr expr)
-- parseLambda _ = err "Malformed lambda"

-- parseRegion :: String -> ParserF Region 
-- parseRegion "L" = pure Local
-- parseRegion "G" = pure Global 
-- parseRegion "F" = pure Fenced 
-- parseRegion "S" = pure Stack 
-- parseRegion r = err $ "Malformed region:" ++ r

-- getOpInfo :: String -> ParserF (Int, String)
-- getOpInfo op = case Map.lookup op precs of 
--     Just (x, y) -> pure (x, y)
--     Nothing -> err $ "Operator doesn't exist: " ++ op

-- parseExprAccum :: (Expr, [LexTree]) -> ParserF (Expr, [LexTree]) 
-- parseExprAccum (lx, (LLeaf (LOp _ op1) : l2@(LLeaf (LOp _ op2)) : rs)) = do 
--     (prec, ty) <- getOpInfo op1 
--     case ty of 
--         "U" -> do 
--             (rx, rrs) <- parseAtomU (Una op1) (l2:rs)
--             insertAppAccum lx rrs rx 
--         "B" -> do
--             (rx, rrs) <- parseAtomU (Una op2) rs
--             ex <- insertLR lx op1 prec rx
--             parseExprAccum (ex, rrs)
--         _ -> err $ "Operator must be unary or binary"
-- parseExprAccum (lx, (LLeaf (LOp _ op) : rs)) = do 
--     (prec, ty) <- getOpInfo op 
--     case ty of 
--         "U" -> do 
--             (rx, rrs) <- parseAtomU (Una op) rs 
--             insertAppAccum lx rrs rx 
--         "B" -> do 
--             (rx, rrs) <- parseAtom rs 
--             ex <- insertLR lx op prec rx 
--             parseExprAccum (ex, rrs)
--         _ -> err $ "Operator must be unary or binary"
-- parseExprAccum (lx, (LLeaf tok) : bs) = 
--     parseToken tok >>= insertAppAccum lx bs
-- parseExprAccum (lx, (LBrack as) : bs) =  
--     parseExpr as >>= insertAppAccum lx bs
-- parseExprAccum (lx, (LBranch as) : bs) = 
--     parseExpr as >>= insertAppAccum lx bs
-- parseExprAccum (lx, (LParen as) : bs) = 
--     parseExpr as >>= insertAppAccum lx bs
-- parseExprAccum (lx, a) = pure (lx, a)

-- insertAppAccum :: Expr -> [LexTree] -> Expr -> ParserF (Expr, [LexTree])  
-- insertAppAccum lx rs rx = do 
--     (prec, _) <- getOpInfo " "
--     ex <- insertLR lx " " prec rx
--     parseExprAccum (ex, rs)

-- insertLR :: Expr -> String -> Int -> Expr -> ParserF Expr 
-- insertLR lx@(Bin lop ll lr) rop rprec rx = do 
--     (lprec, _) <- getOpInfo lop 
--     if lprec < rprec
--         then Bin lop ll <$> insertLR lr rop rprec rx
--         else pure $ Bin rop lx rx 
-- insertLR lx@(Una lop lr) rop rprec rx = do 
--     (lprec, _) <- getOpInfo lop
--     if lprec < rprec 
--         then Una lop <$> insertLR lr rop rprec rx 
--         else pure $ Bin rop lx rx 
-- insertLR lx rop _ rx = pure $ Bin rop lx rx

-- opInfo :: String -> Parser (Int, String)
-- opInfo op = case Map.lookup op precs of 
--     Just a -> pure a 
--     Nothing -> fail $ "operator not found " ++ op 

-- precs :: Map.Map String (Int, String) 
-- precs = Map.fromList precAssocs 

-- precAssocs :: [(String, (Int, String))]
-- precAssocs = [ 
--     (".", (100, "B")),
--     ("@", (95, "U")), 
--     ("~", (90, "U")),
--     ("!", (80, "U")),
--     (" ", (77, "B")),
--     ("<<", (75, "B")), (">>", (75, "B")), 
--     ("*", (70, "B")), ("/", (70, "B")), 
--     ("+", (60, "B")), ("-", (60, "B")), 
--     ("&", (50, "B")), 
--     ("^", (45, "B")), 
--     ("|", (40, "B")), 
--     ("<", (35, "B")), (">", (35, "B")), 
--     ("<=", (35, "B")), (">=", (35, "B")), 
--     ("==", (30, "B")), ("!=", (30, "B")), 
--     ("&&", (20, "B")), ("||", (20, "B")), 
--     (",", (10, "B")),
--     ("$", (5, "B"))]

