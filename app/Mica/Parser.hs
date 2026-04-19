{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mica.Parser where

import Mica.Grouper
import Mica.Lexer
import Mica.Type
import Text.Megaparsec
import Data.Void
import Data.Text
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set

type Parser = Parsec Void [LexTree SP]

data OpTy = 
    Binary | 
    Prefix | 
    Postfix 
    deriving (Eq, Ord, Show)

data OpRec = OpRec { opTy :: OpTy, lbp :: Int, rbp :: Int } 

type OpMap = Map.Map Text OpRec

pRawIden :: Parser Text 
pRawIden = 
    token extract Set.empty <?> unpack "raw identifier"
    where 
        extract (LLeaf (LId _ s)) = Just s 
        extract _ = Nothing

pRawStrLit :: Parser Text 
pRawStrLit = 
    token extract Set.empty <?> unpack "Text literal"
    where 
        extract (LLeaf (LStr _ s)) = Just s
        extract _ = Nothing
    
pExpectOp :: Text -> Parser SP 
pExpectOp op = 
    token extract Set.empty <?> unpack ("expects operator " <> op)
    where 
        extract (LLeaf (LOp p op')) = if op == op' 
            then Just p 
            else Nothing
        extract _ = Nothing 

pExpectIden :: Text -> Parser SP 
pExpectIden iden = 
    token extract Set.empty <?> unpack ("expects identifier " <> iden)
    where 
        extract (LLeaf (LId p iden')) = if iden == iden' 
            then Just p 
            else Nothing
        extract _ = Nothing

pIden :: (SP -> Text -> a) -> Parser a
pIden f = token extract Set.empty <?> "identifier"
    where 
        extract (LLeaf (LId p i)) = Just $ f p i 
        extract _ = Nothing 

recurse :: [LexTree SP] -> Parser a -> Parser a 
recurse ts p = case parse (p <* eof) "" ts of 
    Right a -> pure a 
    Left err -> parseError (NE.head (bundleErrors err))

pParen :: Parser a -> Parser a 
pParen m = do 
    (p, as) <- token extract Set.empty <?> "LParen"
    recurse as m
    where 
        extract (LParen p as) = Just (p, as)
        extract _ = Nothing 

pCurly :: Parser a -> Parser a 
pCurly m = do 
    (p, as) <- token extract Set.empty <?> "LCurly"
    recurse as m
    where 
        extract (LCurly p as) = Just (p, as)
        extract _ = Nothing 

pCurlySP :: (SP -> Parser a) -> Parser a
pCurlySP m = do 
    (p, as) <- token extract Set.empty <?> "LCurly"
    recurse as (m p)
    where 
        extract (LCurly p as) = Just (p, as)
        extract _ = Nothing 

pBrack :: Parser a -> Parser a 
pBrack m = do 
    (p, as) <- token extract Set.empty <?> "LBrack"
    recurse as m
    where 
        extract (LBrack p as) = Just (p, as)
        extract _ = Nothing 

isCtrlOp :: LexTree SP -> Bool 
isCtrlOp (LLeaf (LOp _ ";")) = True 
isCtrlOp (LLeaf (LOp _ "=")) = True 
isCtrlOp (LLeaf (LOp _ ",")) = True
isCtrlOp _ = False

liftPrimary :: MToken SP -> Parser (Exp SP) 
liftPrimary (LId p v) = pure $ Iden p v
liftPrimary (LInt p v) = pure $ IntLit p v
liftPrimary (LDbl p v) = pure $ DblLit p v 
liftPrimary (LStr p v) = pure $ StrLit p v 
liftPrimary (LChar p v) = pure $ CharLit p v
liftPrimary t = fail $ unpack $ "bad primary type " <> pack (show t)

pNud :: Int -> Parser (Exp SP)
pNud thresh = satisfy (not . isCtrlOp) >>= \case 
    LParen sp as -> do
        ex <- recurse as (pNud 0) 
        pLed thresh ex
    LBrack sp as -> do
        ex <- Brack sp <$> recurse as (pNud 0) 
        pLed thresh ex
    LLeaf (LOp p op) -> do
        opRec <- opInfo ("_" <> op) expOps
        pNudUna (rbp opRec) (Una p op) 
    LLeaf (LId p "lam") -> undefined
    LLeaf token -> do
        ex <- liftPrimary token
        pLed thresh ex 
    LCurly _ _ -> fail "expected a nud, instead got a curly"

pNudUna :: Int -> (Exp SP -> Exp SP) -> Parser (Exp SP)
pNudUna thresh cont = satisfy (not . isCtrlOp) >>= \case 
    LParen sp as -> do
        ex <- recurse as (pNud 0) 
        pLed thresh (cont ex)
    LBrack sp as -> do
        ex <- Brack sp <$> recurse as (pNud 0) 
        pLed thresh (cont ex)
    LLeaf (LOp p op) -> do
        opRec <- opInfo ("_" <> op) expOps
        pNudUna (rbp opRec) (cont . Una p op) 
    LCurly _ _ -> fail "expected a nud, instead got a curly"
    LLeaf token -> do
        ex <- liftPrimary token
        pLed thresh (cont ex)

pLed :: Int -> Exp SP -> Parser (Exp SP)
pLed thresh lx = doNext <|> pure lx where 
    doNext = lookAhead (satisfy (not . isCtrlOp)) >>= \case  

        LParen sp as -> do 
            opRec <- opInfo " " expOps 
            if lbp opRec > thresh 
                then do 
                    anySingle
                    rx <- recurse as (pNud 0)
                    pLed thresh (Bin sp " " lx rx)
                else pure lx

        LBrack sp as -> do 
            opRec <- opInfo " " expOps 
            if lbp opRec > thresh 
                then do 
                    anySingle
                    rx <- Brack sp <$> recurse as (pNud 0)
                    pLed thresh (Bin sp " " lx rx)
                else pure lx

        LLeaf (LOp sp op) -> do 
            opRec <- opInfo op expOps 
            case opTy opRec of 
                Prefix -> do 
                    apRec <- opInfo " " expOps 
                    if lbp apRec > thresh 
                        then do 
                            rx <- pNud (rbp apRec)  
                            pLed thresh (Bin sp " " lx rx)
                        else pure lx 
                Binary -> if lbp opRec > thresh 
                    then do 
                        anySingle
                        rx <- pNud (rbp opRec)
                        pLed thresh (Bin sp op lx rx)
                    else pure lx

        LLeaf (LId sp "lam") -> undefined

        LLeaf token -> do 
            opRec <- opInfo " " expOps 
            if lbp opRec > thresh 
                then do 
                    anySingle
                    rx <- liftPrimary token
                    pLed thresh (Bin (mTokenPos token) " " lx rx)
                else pure lx

        LCurly _ _ -> fail "expected a led, instead got curly bracket"

pExp :: Parser (Exp SP)
pExp = pNud 0 >>= pLed 0 

pArg :: Parser (Arg SP)
pArg = complexArg <|> simpleArg 
    where  
        complexArg = 
            pParen $ Arg <$> pIden Name <*> optional (pExpectOp ":" *> pExp)
        simpleArg =
            Arg <$> pIden Name <*> pure Nothing 
    
pBody :: Parser (Body SP) 
pBody = compoundBody <|> inlineBody 
    where 
        compoundBody = pCurlySP $ \sp -> Compound sp <$> many pBlockStmt
        inlineBody = Inline <$> pExp <* pExpectOp ";"

pBlock :: Parser [BlockStmt SP]
pBlock = curlyBlock <|> simpleBlock
    where 
        curlyBlock = pCurly (many pBlockStmt)
        simpleBlock = (:[]) <$> pBlockStmt

pFor :: SP -> Parser (BlockStmt SP) 
pFor p = header <*> pBlock 
    where
        header =
            pParen $ For p <$>
            pExp <*> 
            (pExpectOp ";" *> pExp) <*> 
            (pExpectOp ";" *> pExp)

pCase :: Parser (Case SP) 
pCase = Case <$> 
    pIden Name <*> 
    many (pIden Name) <*>
    (pExpectOp "=" *> pBlock)

pBlockStmt :: Parser (BlockStmt SP) 
pBlockStmt = 
    let 
        pIf = pExpectIden "if" >>= \p -> If p <$> pParen pExp <*> pBlock 
        pElse = pExpectIden "else" >>= \p -> Else p <$> pBlock 
        pWhile = pExpectIden "while" >>= \p -> 
            While p <$> 
                pParen pExp <*> 
                pBlock
        pDo = pExpectIden "do" >>= \p -> 
            DoWhile p <$> 
                pCurly (many pBlockStmt) <*>
                (pExpectIden "while" *> pParen pExp) 
        pRet = pExpectIden "return" >>= \p ->
            Ret p <$> 
                (optional pExp <* pExpectOp ";")
        pCont = pExpectIden "continue" >>= \p -> Cont p <$ pExpectOp ";"
        pBrk = pExpectIden "break" >>= \p -> Brk p <$ pExpectOp ";" 
        pMat = pExpectIden "match" >>= \p -> 
            Mat p <$> 
                pParen pExp <*> 
                pCurly (many pCase)
        pLet = pExpectIden "let" >>= \p -> 
            Let p <$> 
                (pArg <* pExpectOp "=") <*> 
                (pExp <* pExpectOp ";")

        pForStmt = pExpectIden "for" >>= pFor
        pExpSt = ExpStmt <$> pExp <* pExpectOp ";"
    in 
        (pIf <?> ("if statement"::String)) <|> 
        (pElse <?> ("else statement"::String)) <|> 
        (pWhile <?> ("while statement"::String)) <|> 
        (pDo <?> ("do-while statement"::String)) <|> 
        (pForStmt <?> ("for statement"::String)) <|> 
        (pRet <?> ("return statement"::String)) <|> 
        (pCont <?> ("continue statement"::String)) <|> 
        (pBrk <?> ("break statement"::String)) <|> 
        (pMat <?> ("match statement"::String)) <|> 
        (pLet <?> ("let statement"::String)) <|> 
        (pExpSt <?> ("expression statement"::String))
 
pMemb :: Parser (Memb SP)
pMemb = Memb <$> pIden Name <* pExpectOp ":" <*> pExp

pInj :: Parser (Inj SP)
pInj = Inj <$> pIden Name <* pExpectOp ":" <*> pExp

pSpec :: Parser Spe
pSpec =
    let 
        pStat = Static <$ pExpectIden "static"
        pTL = ThreadLocal <$ pExpectIden "thread_local" 
        pMut = Mutable <$ pExpectIden "mutable"
        pNR = NoReturn <$ pExpectIden "no_return" 
    in 
        pStat <|> pTL <|> pMut <|> pNR
  
pFileStmt :: Parser (FileStmt SP)
pFileStmt = 
    let 
        pImp = pExpectIden "import" >>= \p -> Imp p <$> pRawStrLit 
        pInc = pExpectIden "include" >>= \p -> Inc p <$> pRawStrLit
        pOpt ma = optional ma >>= \case
            Just a -> pure $ Just a
            Nothing -> pExpectOp ";" $> Nothing
        pSum = pExpectIden "variant" >>= \p -> 
            Sum p <$> 
                pIden Name <*> 
                (many (pIden Name) <* pExpectOp "=") <*>
                pOpt (pCurly $ pInj `sepEndBy` pExpectOp ";")
        pRec = pExpectIden "record" >>= \p -> 
            Rec p <$> 
                pIden Name <*> 
                (many (pIden Name) <* pExpectOp "=") <*>
                pOpt (pCurly $ pMemb `sepEndBy` pExpectOp ";")
        pExt = pExpectIden "foreign" >>= \p -> 
            Ext p <$> pIden Name <*> pExp <*> pRawStrLit
        pDec specs name = 
            Dec specs name <$> 
                (pExpectOp ":" *> pExp) <* pExpectOp ";" 
        pFun specs name = 
            Fun specs name <$> 
                pParen (pArg `sepBy` pExpectOp ",") <*> 
                (pExpectOp "=" *> pBody) 
        pTrm specs name = 
            Trm specs name <$> 
                (pExpectOp "=" *> pExp) <* pExpectOp ";"
        pPoly = many pSpec >>= \s -> pIden Name >>= \n -> 
            (pDec s n <?> ("type declaration"::String)) <|> 
            (pFun s n <?> ("function definition"::String)) <|> 
            (pTrm s n <?> ("term definition"::String))
    in 
        (pImp <?> ("import directive"::String)) <|> 
        (pInc <?> ("include directive"::String)) <|> 
        (pSum <?> ("variant definition"::String)) <|> 
        (pRec <?> ("record defintion"::String)) <|> 
        (pExt <?> ("foreign definition"::String)) <|> 
        pPoly

pMica :: Parser [FileStmt SP]
pMica = many pFileStmt <* eof

prettyLexTree :: LexTree a -> Text 
prettyLexTree (LParen _ ts) =
    "(" <> Data.Text.unwords (Prelude.map prettyLexTree ts) <> ")"
prettyLexTree (LCurly _ ts) = 
    "{" <> Data.Text.unwords (Prelude.map prettyLexTree ts) <> "}"
prettyLexTree (LBrack _ ts) = 
    "[" <> Data.Text.unwords (Prelude.map prettyLexTree ts) <> "]"
prettyLexTree (LLeaf l) = prettyMToken l

lexTreePos :: LexTree SP -> SP
lexTreePos (LParen sp _) = sp
lexTreePos (LCurly sp _) = sp
lexTreePos (LBrack sp _) = sp
lexTreePos (LLeaf l) = mTokenPos l

instance VisualStream [LexTree SP] where 
     showTokens _ = 
        unpack . Data.Text.unwords . Prelude.map prettyLexTree . NE.toList 
    
instance TraversableStream [LexTree SP] where 
    reachOffsetNoLine o pst = 
        let offsetDiff = o - pstateOffset pst in
        let tokensLeft = Prelude.drop offsetDiff (pstateInput pst) in
        case tokensLeft of
            []    -> pst
            (t:_) -> pst { 
                pstateOffset = o,
                pstateSourcePos = lexTreePos t }

opInfo :: Text -> OpMap -> Parser OpRec
opInfo op opMap = case Map.lookup op opMap of 
    Just a -> pure a 
    Nothing -> fail $ unpack $ "operator not found " <> op 

expOps :: OpMap
expOps = Map.fromList [ 
    (".", OpRec Binary 100 100),
    ("@", OpRec Prefix 95 95),
    ("_*", OpRec Prefix 95 95), 
    ("-->", OpRec Binary 94 9),
    ("~", OpRec Prefix 90 90),
    ("_~", OpRec Prefix 90 90),
    ("!", OpRec Prefix 85 85),
    ("_!", OpRec Prefix 85 85),
    ("_+", OpRec Prefix 85 85),
    ("_-", OpRec Prefix 85 85), 
    ("**", OpRec Binary 85 84),
    (" ", OpRec Binary 80 80), 
    ("<<", OpRec Binary 75 75), 
    (">>", OpRec Binary 75 75), 
    ("*", OpRec Binary 75 75),
    ("/", OpRec Binary 70 70), 
    ("+", OpRec Binary 60 60),  
    ("-", OpRec Binary 60 60), 
    ("&", OpRec Binary 50 50), 
    ("^", OpRec Binary 45 45), 
    ("|", OpRec Binary 40 40), 
    ("<", OpRec Binary 35 35), 
    (">", OpRec Binary 35 35), 
    ("<=", OpRec Binary 35 35), 
    (">=", OpRec Binary 35 35), 
    ("==", OpRec Binary 30 30), 
    ("!=", OpRec Binary 30 30), 
    ("&&", OpRec Binary 20 20), 
    ("||", OpRec Binary 20 20), 
    ("->", OpRec Binary 15 14),
    (",", OpRec Binary 13 12),
    (":=", OpRec Binary 5 4),
    ("+=", OpRec Binary 5 4),
    ("-=", OpRec Binary 5 4),
    ("*=", OpRec Binary 5 4),
    ("^=", OpRec Binary 5 4),
    ("|=", OpRec Binary 5 4),
    ("&=", OpRec Binary 5 4),
    ("%=", OpRec Binary 5 4)]
