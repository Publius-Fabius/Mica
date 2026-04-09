module Mica.Checker where 

import Mica.Type 
import Control.Monad.Except
import Control.Monad.State
import Data.Map
import Text.Megaparsec

type SP = SourcePos 

data ChkSt = ChkSt {
    ctx :: Map String (Exp SP),     -- type vars to types
    env :: Map String (Exp SP)      -- terms to types
}

cNewChkSt = ChkSt { ctx = Data.Map.empty, env = Data.Map.empty }

type Checker a = StateT ChkSt (Except String) a

type SymTbl a = Map String a 

lookupEnv :: String -> Checker (Exp SP) 
lookupEnv iden = get >>= \st -> case Data.Map.lookup iden (env st) of 
    Just ex -> pure ex 
    Nothing -> throwError $ "type not found in environment for " ++ iden

insertEnv :: String -> Exp SP -> Checker (Exp SP) 
insertEnv s e = 
    state (\st -> (e, st{ env = Data.Map.insert s e (env st)}))

insertCtx :: String -> Exp SP -> Checker (Exp SP) 
insertCtx s e = 
    state (\st -> (e, st{ ctx = Data.Map.insert s e (ctx st)}))

cMapTVs :: (a -> String -> Exp a) -> Exp a -> Exp a 
cMapTVs f (TVar sp v) = f sp v
cMapTVs f (Bin sp op lx rx) = Bin sp op (cMapTVs f lx) (cMapTVs f rx)
cMapTVs f (Una sp op ex) = Una sp op $ cMapTVs f ex
cMapTVs f (Brack sp ex) = Brack sp $ cMapTVs f ex
cMapTVs _ ex = ex 

cFoldTVs :: (p -> String -> a -> a) -> Exp p -> a -> a 
cFoldTVs f (TVar sp v) a = f sp v a 
cFoldTVs f (Bin sp op lx rx) a = cFoldTVs f rx $ cFoldTVs f lx a
cFoldTVs f (Una sp op ex) a = cFoldTVs f ex a
cFoldTVs f (Brack sp ex) a = cFoldTVs f ex a
cFoldTVs _ _ a = a

cGetTVars :: Exp a -> SymTbl (Exp a)
cGetTVars ex = cFoldTVs (\sp tv tbl -> 
    Data.Map.insert tv (TVar sp tv) tbl 
    ) ex Data.Map.empty 

cFindUnused :: String -> Int -> SymTbl (Exp a) -> SymTbl (Exp a) -> String
cFindUnused tv cnt t1 t2 = let nxt = tv ++ show cnt in 
    if Data.Map.member nxt t1 || Data.Map.member nxt t2 
        then cFindUnused tv (cnt + 1) t1 t2
        else nxt

cFreshenSyms :: SymTbl (Exp a) -> SymTbl (Exp a) -> SymTbl (Exp a) 
cFreshenSyms ts cs = let 
    folder k (TVar sp _) ts' = if Data.Map.member k cs 
        then Data.Map.insert k (TVar sp $ cFindUnused k 0 cs ts') ts'
        else ts' 
    in Data.Map.foldrWithKey folder ts ts 

cFreshen :: Exp SP -> Checker (Exp SP) 
cFreshen ex = let ts = cGetTVars ex in ctx <$> get >>= \cs ->
    let 
        fresh = cFreshenSyms ts cs 
        mapper sp tv = Data.Map.findWithDefault (TVar sp tv) tv fresh
    in 
        pure $ cMapTVs mapper ex
  
cFindRoot :: String -> Exp SP -> Checker (Exp SP) 
cFindRoot s ex = get >>= \st -> case Data.Map.lookup s (ctx st) of 
    Just v@(TVar _ tv) -> cFindRoot tv v
    Just gx -> pure gx
    Nothing -> pure ex

cOccursChk :: String -> Exp SP -> Checker ()
cOccursChk tv ex = cFoldTVs (\sp tv' m -> 
    if tv == tv' 
        then throwError $ "occurs check " ++ tv ++ " = " ++ tv'
        else m >> pure ()) ex (pure ()) 

cBind :: (SP, String) -> Exp SP -> Checker (Exp SP)
cBind (srcPos, varName) newTy = do
    simpTy <- cSubst newTy 
    root <- cFindRoot varName (TVar srcPos varName) 
    case root of 
        (TVar _ tv) -> do 
            cOccursChk tv simpTy 
            insertCtx tv simpTy
        _ -> 
            root `cUnify` simpTy

cUnify :: Exp SP -> Exp SP -> Checker (Exp SP)
cUnify (Iden lp lv) (Iden rp rv) = if lv == rv 
    then pure (Iden lp lv)
    else throwError $ "could not unify iden " ++ lv ++ " with " ++ rv

cUnify (TVar lp lv) rx = (lp, lv) `cBind` rx
cUnify lx (TVar rp rv) = (rp, rv) `cBind` lx

cUnify (Bin lp lo ll lr) (Bin rp ro rl rr) = if lo == ro
    then Bin rp ro <$> (ll `cUnify` rl) <*> (lr `cUnify` rr)
    else throwError $ "could not unify operator " ++ lo ++ " with " ++ ro

cUnify (Una lp lo lr) (Una rp ro rr) = if lo == ro 
    then Una rp ro <$> (lr `cUnify` rr)
    else throwError $ "could not unify operator " ++ lo ++ " with " ++ ro

cUnify (Brack lp lx) (Brack rp rx) = lx `cUnify` rx

cUnify lx rx = throwError $ 
    "could not unify " ++ prettyExp lx ++ " with " ++ prettyExp rx

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
    "could not apply type " ++ prettyExp rx ++ " to " ++ prettyExp lx 

cTy :: Exp SP -> Checker (Exp SP)
cTy (Iden p a) = lookupEnv a >>= cFreshen 
cTy (IntLit p _) = pure $ Iden p "IntLike"
cTy (DblLit p _) = pure $ Iden p "DblLike"
cTy (StrLit p _) = pure $ Iden p "Str"
cTy (CharLit p _) = pure $ Iden p "Char"
cTy (Bin p o l r) = do 
    ot <- lookupEnv o
    lt <- cTy l 
    rt <- cTy r 
    t <- cApp ot lt 
    cApp t rt 
cTy (Una p o r) = do 
    ot <- lookupEnv o 
    rt <- cTy r 
    cApp ot rt 
cTy (Brack p ex) = Brack p <$> cTy ex
cTy (Lam _ _ _) = undefined 
cTy (Tri _ _ _ _) = undefined 

cFun :: FileStmt SP -> Checker (FileStmt SP)
cFun (Fun p n as ms) = undefined 

