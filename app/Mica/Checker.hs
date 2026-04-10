{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mica.Checker where 

import Mica.Type 
import Control.Monad.Except
import Control.Monad.State
import Data.Map
import Data.Text
import Text.Megaparsec

type SP = SourcePos 

data ChkSt = ChkSt {
    ctx :: Map Text (Exp SP),     -- type vars to types
    env :: Map Text (Exp SP),     -- terms to types
    retTy :: Maybe (Exp SP)         -- Return type
} deriving (Show)

cNewChkSt = ChkSt { 
    ctx = Data.Map.empty, 
    env = Data.Map.empty,
    retTy = Nothing }

type Checker a = StateT ChkSt (Except Text) a

type SymTbl a = Map Text a 

cGetRetTy :: Checker (Maybe (Exp SP)) 
cGetRetTy = retTy <$> get

cSetRetTy :: Maybe (Exp SP) -> Checker ()
cSetRetTy rt = state (\s -> ((), s { retTy = rt }))

cWithNewRetCtx :: Checker a -> Checker a 
cWithNewRetCtx ma = do 
    rTy <- cGetRetTy 
    cSetRetTy Nothing
    ma <* cSetRetTy rTy

cScopedCtx :: Checker a -> Checker a 
cScopedCtx ma = do 
    ct <- ctx <$> get 
    a <- ma 
    state (\s -> (a, s{ ctx = ct }))

cScopedEnv :: Checker a -> Checker a 
cScopedEnv ma = do 
    en <- env <$> get 
    a <- ma 
    state (\s -> (a, s{ env = en }))

cLookupEnv :: Text -> Checker (Exp SP) 
cLookupEnv iden = get >>= \st -> case Data.Map.lookup iden (env st) of 
    Just ex -> pure ex 
    Nothing -> throwError $ "type not found in environment for " <> iden

cInsertEnv :: Text -> Exp SP -> Checker (Exp SP) 
cInsertEnv s e = 
    state (\st -> (e, st{ env = Data.Map.insert s e (env st)}))

cInsertCtx :: Text -> Exp SP -> Checker (Exp SP) 
cInsertCtx s e = 
    state (\st -> (e, st{ ctx = Data.Map.insert s e (ctx st)}))

cMapTVs :: (a -> Text -> Exp a) -> Exp a -> Exp a 
cMapTVs f (TVar sp v) = f sp v
cMapTVs f (Bin sp op lx rx) = Bin sp op (cMapTVs f lx) (cMapTVs f rx)
cMapTVs f (Una sp op ex) = Una sp op $ cMapTVs f ex
cMapTVs f (Brack sp ex) = Brack sp $ cMapTVs f ex
cMapTVs f (TLam sp ex) = TLam sp $ cMapTVs f ex
cMapTVs _ ex = ex 

cFoldTVs :: (p -> Text -> a -> a) -> Exp p -> a -> a 
cFoldTVs f (TVar sp v) a = f sp v a 
cFoldTVs f (Bin sp op lx rx) a = cFoldTVs f rx $ cFoldTVs f lx a
cFoldTVs f (Una sp op ex) a = cFoldTVs f ex a
cFoldTVs f (Brack sp ex) a = cFoldTVs f ex a
cFoldTVs f (TLam sp ex) a = cFoldTVs f ex a
cFoldTVs _ _ a = a

cGetTVars :: Exp a -> SymTbl (Exp a)
cGetTVars ex = cFoldTVs (\sp tv tbl -> 
    Data.Map.insert tv (TVar sp tv) tbl 
    ) ex Data.Map.empty 

cFindUnused :: Text -> Int -> SymTbl (Exp a) -> SymTbl (Exp a) -> Text
cFindUnused tv cnt t1 t2 = let nxt = tv <> pack (show cnt) in 
    if Data.Map.member nxt t1 || Data.Map.member nxt t2 
        then cFindUnused tv (cnt + 1) t1 t2
        else nxt

cFreshenSyms :: SymTbl (Exp a) -> SymTbl (Exp a) -> SymTbl (Exp a) 
cFreshenSyms ts cs = let 
    folder k (TVar sp _) ts' = if Data.Map.member k cs 
        then Data.Map.insert k (TVar sp $ cFindUnused k 0 cs ts') ts'
        else ts'
    in Data.Map.foldrWithKey folder ts ts 

cFreshen :: Exp SP -> Checker (Exp SP, SymTbl (Exp SP)) 
cFreshen ex = let ts = cGetTVars ex in ctx <$> get >>= \cs ->
    let 
        fresh = cFreshenSyms ts cs 
        mapper sp tv = Data.Map.findWithDefault (TVar sp tv) tv fresh
    in 
        pure (cMapTVs mapper ex, fresh)

cFresh :: Checker Text
cFresh = (ctx <$> get) >>= pure . cFindUnused "x" 0 Data.Map.empty 

cFindRoot :: Text -> Exp SP -> Checker (Exp SP) 
cFindRoot s ex = get >>= \st -> case Data.Map.lookup s (ctx st) of 
    Just v@(TVar _ tv) -> cFindRoot tv v
    Just gx -> pure gx
    Nothing -> pure ex

cOccursChk :: Text -> Exp SP -> Checker ()
cOccursChk tv ex = cFoldTVs (\sp tv' m -> 
    if tv == tv' 
        then throwError $ "occurs check " <> tv <> " = " <> tv'
        else m >> pure ()) ex (pure ()) 

cBind :: (SP, Text) -> Exp SP -> Checker (Exp SP)
cBind (srcPos, varName) newTy = do
    simpTy <- cSubst newTy 
    root <- cFindRoot varName (TVar srcPos varName) 
    case root of 
        (TVar _ tv) -> do 
            cOccursChk tv simpTy 
            cInsertCtx tv simpTy
        _ -> 
            root `cUnify` simpTy

cUnify :: Exp SP -> Exp SP -> Checker (Exp SP)
cUnify (Iden lp lv) (Iden rp rv) = if lv == rv 
    then pure (Iden lp lv)
    else throwError $ "could not unify iden " <> lv <> " with " <> rv

cUnify (TVar lp lv) rx = (lp, lv) `cBind` rx
cUnify lx (TVar rp rv) = (rp, rv) `cBind` lx

cUnify (Bin lp lo ll lr) (Bin rp ro rl rr) = if lo == ro
    then Bin rp ro <$> (ll `cUnify` rl) <*> (lr `cUnify` rr)
    else throwError $ "could not unify operator " <> lo <> " with " <> ro

cUnify (Una lp lo lr) (Una rp ro rr) = if lo == ro 
    then Una rp ro <$> (lr `cUnify` rr)
    else throwError $ "could not unify operator " <> lo <> " with " <> ro

cUnify (Brack lp lx) (Brack rp rx) = lx `cUnify` rx

cUnify lx rx = throwError $ 
    "could not unify " <> prettyExp lx <> " with " <> prettyExp rx

cSubst :: Exp SP -> Checker (Exp SP) 
cSubst (TVar sp tv) = do
    root <- cFindRoot tv (TVar sp tv)
    case root of 
        TVar sp' tv' -> pure $ TVar sp' tv' 
        ex -> cSubst ex 
cSubst (Una sp op rx) = Una sp op <$> cSubst rx 
cSubst (Bin sp op lx rx) =  Bin sp op <$> cSubst lx <*> cSubst rx 
cSubst (Brack sp x) = Brack sp <$> cSubst x  
cSubst ex = pure ex

cApp :: Exp SP -> Exp SP -> Checker (Exp SP)
cApp (Bin p "->" dom cod) vty = do 
    cUnify dom vty
    cSubst cod 
cApp lx rx = throwError $
    "could not apply type " <> prettyExp rx <> " to " <> prettyExp lx 

cTy :: Exp SP -> Checker (Exp SP)
cTy (Iden p a) = cLookupEnv a
cTy (IntLit p _) = pure $ Iden p "IntLike"
cTy (DblLit p _) = pure $ Iden p "DblLike"
cTy (StrLit p _) = pure $ Iden p "Str"
cTy (CharLit p _) = pure $ Iden p "Char"
cTy (Bin p o l r) = do 
    ot <- cLookupEnv o
    lt <- cTy l 
    rt <- cTy r 
    t <- cApp ot lt 
    cApp t rt 
cTy (Una p o r) = do 
    ot <- cLookupEnv o 
    rt <- cTy r 
    cApp ot rt 
cTy (Brack p ex) = Brack p <$> cTy ex
cTy (Lam p args bdy) = do 
    argsF <- introArgs id args 
    retTy <- cTyBody bdy
    pure $ TLam p (argsF retTy)
    where 
        introArgs f ((Arg (Name sp var) (Just ty)):bs) = do
            (ty', tvs) <- cFreshen ty
            cInsertEnv var ty' 
            state (\s -> ((), s{ ctx = (ctx s) `Data.Map.union` tvs }))
            introArgs (f . Bin sp "->" ty') bs 
        introArgs f ((Arg (Name sp var) Nothing):bs) = do
            tv <- cFresh 
            cInsertEnv var (TVar sp tv)
            cInsertCtx tv (TVar sp tv)
            introArgs (f . Bin sp "->" (TVar sp tv)) bs 
        introArgs f [] = pure f
cTy (Tri _ _ _ _) = undefined 

cTyBody :: Body SP -> Checker (Exp SP)
cTyBody (Compound stmts) = cWithNewRetCtx $ do 
    mapM_ mapper stmts 
    cGetRetTy >>= \case 
        Just rTy -> pure rTy 
        Nothing -> pure TVoid
    where 
        mapper (Ret sp (Just ex)) = cGetRetTy >>= \case  
            Just rTy -> cTy ex >>= cUnify rTy >>= cSetRetTy . Just
            Nothing -> cTy ex >>= cSetRetTy . Just 
        mapper (Ret sp Nothing) = cGetRetTy >>= \case  
            Just a -> throwError $ 
                "returns void but expected type " <> prettyExp a
            Nothing -> pure ()
        mapper _ = pure ()
cTyBody (Inline exp) = cTy exp 

cFun :: FileStmt SP -> Checker (FileStmt SP)
cFun (Fun p n as ms) = undefined 

cBlockStmt :: BlockStmt SP -> Checker (BlockStmt SP)
cBlockStmt = undefined 

cFileStmt :: FileStmt SP -> Checker (FileStmt SP)
cFileStmt = undefined 

cPreType :: Exp SP -> Checker (Exp SP) 
cPreType (TLam sp ex) = TLam sp <$> cPreType ex
cPreType (Bin sp op lx rx) = Bin sp op <$> cPreType lx <*> cPreType rx 
cPreType (Una sp op rx) = Una sp op <$> cPreType rx 
cPreType (Brack sp ex) = Brack sp <$> cPreType ex 
cPreType (TVoid) = pure TVoid
cPreType (Iden sp s) = env <$> get >>= \en -> if Data.Map.member s en
    then pure $ Iden sp s 
    else pure $ TVar sp s 
cPreType _ = throwError "Found term in type"

cPreArg :: Arg SP -> Checker (Arg SP)
cPreArg (Arg n (Just ty)) = Arg n . Just <$> cPreType ty 
cPreArg (Arg n Nothing) = pure (Arg n Nothing)

cPreTerm :: Exp SP -> Checker (Exp SP) 
cPreTerm (Lam sp args (Inline exp)) = 
    Lam sp <$> mapM cPreArg args <*> (Inline <$> cPreTerm exp)
cPreTerm (Lam sp args (Compound stmts)) = 
    Lam sp <$> mapM cPreArg args <*> (Compound <$> mapM cPreBlockStmt stmts)
cPreTerm (Bin sp op lx rx) = Bin sp op <$> cPreTerm lx <*> cPreTerm rx 
cPreTerm (Una sp op rx) = Una sp op <$> cPreTerm rx
cPreTerm (Brack sp ex) = Brack sp <$> cPreTerm ex 
cPreTerm (TLam _ _) = throwError $ "Found TLam in term"
cPreTerm (TVar _ _) = throwError $ "Found TVar in term"
cPreTerm ex = pure ex 

cPreBlockStmt :: BlockStmt SP -> Checker (BlockStmt SP)
cPreBlockStmt (If sp ex stmts) = 
    If sp <$> cPreTerm ex <*> mapM cPreBlockStmt stmts
cPreBlockStmt (Else sp stmts) = 
    Else sp <$> mapM cPreBlockStmt stmts
cPreBlockStmt (While sp ex stmts) = 
    While sp <$> cPreTerm ex <*> mapM cPreBlockStmt stmts 
cPreBlockStmt (DoWhile sp stmts ex) = 
    DoWhile sp <$> mapM cPreBlockStmt stmts <*> cPreTerm ex 
cPreBlockStmt (For sp pre cond inc stmts) = 
    For sp <$> 
    cPreBlockStmt pre <*> 
    cPreTerm cond <*> 
    cPreBlockStmt inc <*> 
    mapM cPreBlockStmt stmts
cPreBlockStmt (Ret sp (Just ex)) = Ret sp . Just <$> cPreTerm ex
cPreBlockStmt (Let sp arg ex) = Let sp <$> cPreArg arg <*> cPreTerm ex
cPreBlockStmt (Mat sp ex cs) = Mat sp <$> cPreTerm ex <*> undefined 
cPreBlockStmt (Assign ex) = Assign <$> cPreTerm ex  
cPreBlockStmt stmt = pure stmt 

cPreMemb :: Memb SP -> Checker (Memb SP) 
cPreMemb (Memb n ex) = Memb n <$> cPreType ex

cPreFileStmt :: FileStmt SP -> Checker (FileStmt SP) 
cPreFileStmt (Sum sp n ns mems) = Sum sp n ns <$> mapM cPreMemb mems
cPreFileStmt (Rec sp n ns mems) = Rec sp n ns <$> mapM cPreMemb mems
cPreFileStmt (Fun sp n args (Inline ex)) = 
    Fun sp n <$> mapM cPreArg args <*> (Inline <$> cPreTerm ex)
cPreFileStmt (Dec sp n ex) = Dec sp n <$> cPreTerm ex  
cPreFileStmt (Def sp n arg ex) = Def sp n <$> cPreArg arg <*> cPreTerm ex 
cPreFileStmt stmt = pure stmt 