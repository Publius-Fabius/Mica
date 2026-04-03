module Mica.Parser where

import Mica.Lexer 
import Mica.Type
import qualified Data.Map as Map

-- type ParserF a = Either String a

-- err :: String -> ParserF a
-- err = Left

-- parse :: [LexTree] -> ParserF [FileStmt]
-- parse [] = pure []
-- parse ((LBranch stmt) : bs) = (:) <$> parseStmt stmt <*> parse bs
-- parse _ = err "No statement found"  -- TODO: report neighbors sourcepos

-- -- dec optional_spec iden ':' type ;
-- -- def iden = ... ; 
-- parseStmt :: [LexTree] -> ParserF FileStmt 
-- parseStmt (LLeaf (LId _ "fun") : rest) = parseFun rest
-- parseStmt (LLeaf (LId _ "jdg") : rest) = parseJdg rest
-- parseStmt (LLeaf (LId _ "data") : rest) = parseDat rest
-- parseStmt (LLeaf (LId _ "struct") : rest) = parseStru rest
-- parseStmt (LLeaf (LId _ "declare") : rest) = parseDec rest
-- parseStmt (LLeaf (LId _ "define") : rest) = parseDef rest
-- parseStmt (LLeaf (LId _ "import") : rest) = parseImport rest
-- parseStmt (LLeaf (LId _ "extern") : rest) = parseExtern rest
-- parseStmt _ = err "Malformed statement"

-- -- fun iden(args) expr|bodystmt ;
-- parseFun :: [LexTree] -> ParserF FileStmt 
-- parseFun (LLeaf (LId _ i) : LLeaf (LOp _ "=") : (LParen args) : bdy ) =
--     Fun i <$> (parseExpr args) <*> (parseFunBlock bdy)
-- parseFun _ = err "Malformed function statement"

-- -- expr | { block }
-- parseFunBlock :: [LexTree] -> ParserF FunBlock 
-- parseFunBlock ((LCurly block) : []) = Left <$> parseBlock block 
-- parseFunBlock expr = Right <$> parseExpr expr

-- -- x = 1; y = 2;
-- parseBlock :: [LexTree] -> ParserF [BlockStmt] 
-- parseBlock (a : bs) = (:) <$> parseBlockStmt a <*> parseBlock bs 
-- parseBlock [] = pure []

-- parseBlockStmt :: LexTree -> ParserF BlockStmt 
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "if") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "else") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "while") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "for") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "return") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "continue") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "break") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "match") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "set") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "let") : rs)) = undefined
-- -- parseBlockStmt (LBranch (LLeaf (LId _ "reg") : rs)) = undefined
-- parseBlockStmt _ = err "Malformed block statement"

-- -- jdg iden ':' type ;
-- parseJdg :: [LexTree] -> ParserF FileStmt 
-- parseJdg (LLeaf (LId _ i) : LLeaf (LOp _ ":") : ty) = 
--     Jdg i <$> parseExpr ty 
-- parseJdg _ = err "Malformed judgement"

-- -- data iden { conA : ... ; conB : ...; };
-- parseDat :: [LexTree] -> ParserF FileStmt 
-- parseDat (LLeaf (LId _ i) : LCurly cs : []) = Dat i <$> parseCons cs 
-- parseDat _ = err "Malformed data"

-- parseCons :: [LexTree] -> ParserF [Constr] 
-- parseCons (LBranch (LLeaf (LId _ i) : LLeaf (LOp _ ":") : ty) : cs) = 
--     (:) . (\x -> (i, x)) <$> parseExpr ty <*> parseCons cs 
-- parseCons [] = pure []
-- parseCons _ = err "Malformed constructor"

-- -- struct iden { memA : ... ; memB : ...; };
-- parseStru :: [LexTree] -> ParserF FileStmt 
-- parseStru (LLeaf (LId _ i) : LCurly cs : []) = Stru i <$> parseCons cs 
-- parseStru _ = err "Malformed data"

-- parseDef :: [LexTree] -> ParserF FileStmt 
-- parseDef = undefined

-- parseDec :: [LexTree] -> ParserF FileStmt 
-- parseDec = undefined 

-- parseImport :: [LexTree] -> ParserF FileStmt 
-- parseImport = undefined 

-- parseExtern :: [LexTree] -> ParserF FileStmt 
-- parseExtern = undefined 

-- parseToken :: MToken -> ParserF Expr 
-- parseToken (LId _ i) = pure $ Iden i
-- parseToken (LDbl _ n) = pure $ LitDbl n
-- parseToken (LInt _ n) = pure $ LitInt n
-- parseToken (LStr _ s) = pure $ LitStr s
-- parseToken _ = err "Could not parse token"

-- parseExpr :: [LexTree] -> ParserF Expr 
-- parseExpr as = fst <$> (parseAtom as >>= parseExprAccum)

-- parseAtom :: [LexTree] -> ParserF (Expr, [LexTree]) 
-- parseAtom (LParen as : bs) = (\x -> (x, bs)) <$> parseExpr as 
-- parseAtom (LBrack as : bs) = (\x -> (x, bs)) <$> parseExpr as 
-- parseAtom (LCurly as : bs) = (\x -> (x, bs)) <$> parseExpr as 
-- parseAtom (LBranch as : bs) = (\x -> (x, bs)) <$> parseExpr as 
-- parseAtom (LLeaf (LId _ "lam") : bs) = parseLambda bs
-- parseAtom (LLeaf (LOp _ op) : bs) = parseAtomU (Una op) bs
-- parseAtom (LLeaf t : bs) = (\x -> (x, bs)) <$> parseToken t 
-- parseAtom _ = err "Malformed atom"

-- parseAtomU :: (Expr -> Expr) -> [LexTree] -> ParserF (Expr, [LexTree])
-- parseAtomU f (LLeaf (LOp _ op) : bs) = parseAtomU (f . (Una op)) bs
-- parseAtomU f (LLeaf (LId _ "lam") : bs) = 
--     (\(x, ys) -> (f x, ys)) <$> parseLambda bs
-- parseAtomU f (LLeaf t : bs) = (\x -> (f x, bs)) <$> parseToken t 
-- parseAtomU f (LParen as : bs) = (\x -> (f x, bs)) <$> parseExpr as
-- parseAtomU _ _ = err "Malformed atom unary"

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

