{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mica.Checker where 

import Mica.Cute
import Mica.Type 
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.Map
import Data.Text
import Text.Megaparsec

type SP = SourcePos 

type SymTbl a = Map Text a 

data ChkSt = ChkSt {
    uid :: Int, 
    context :: SymTbl (Exp ()),           -- types to types
    environment :: SymTbl (Exp ()),       -- terms to types
    returns :: Maybe (Exp ())             -- Return type
} deriving (Show)

newChkSt = ChkSt { 
    uid = 0,
    context = Data.Map.empty, 
    environment = Data.Map.empty,
    returns = Nothing }

type Checker a = StateT ChkSt (Except Text) a

data Note = Note SP (Either Text (Exp ()))
    deriving (Show)

instance Cute Note where 
    cute (Note sp (Left msg)) = pack (sourcePosPretty sp) <> "\n" <> msg
    cute (Note sp (Right ex)) = pack (sourcePosPretty sp) <> " " <> cute ex

willReturn :: Maybe (Exp ()) -> Checker ()
willReturn rt = state (\st -> ((), st { returns = rt }))

withNewReturnCtx :: Checker a -> Checker a 
withNewReturnCtx ma = do 
    r <- returns <$> get 
    willReturn Nothing
    a <- ma
    willReturn r
    pure a 
    
scopedCtx :: Checker a -> Checker a 
scopedCtx ma = do 
    c <- context <$> get 
    u <- uid <$> get  
    a <- ma 
    state $ \s -> (a, s{ context = c, uid = u })

scopedEnv :: Checker a -> Checker a 
scopedEnv ma = do 
    env <- environment <$> get 
    a <- ma 
    state (\st -> (a, st{ environment = env }))

lookupEnv :: Text -> Checker (Exp ()) 
lookupEnv k = do 
    env <- environment <$> get
    case Data.Map.lookup k env of 
        Just ex -> pure ex 
        Nothing -> throwError $ "type not found in environment for " <> k

insertEnv :: Text -> Exp () -> Checker (Exp ()) 
insertEnv k ex = state $ \st -> 
    (ex, st{ environment = Data.Map.insert k ex $ environment st })

insertCtx :: Text -> Exp () -> Checker (Exp ()) 
insertCtx k ex = state $ \st -> 
    (ex, st{ context = Data.Map.insert k ex (context st) })

inCtx :: Text -> Checker (Bool) 
inCtx k = Data.Map.member k . context <$> get

nextUId :: Checker (Int) 
nextUId = state $ \st -> (uid st, st{ uid = uid st + 1 })

mapTVs :: (a -> Text -> Exp a) -> Exp a -> Exp a 
mapTVs f (TVar a v) = f a v
mapTVs f (Bin a op lx rx) = Bin a op (mapTVs f lx) (mapTVs f rx)
mapTVs f (Una a op ex) = Una a op $ mapTVs f ex
mapTVs f (Brack a ex) = Brack a $ mapTVs f ex
mapTVs f (TLam a ex) = TLam a $ mapTVs f ex
mapTVs _ ex = ex 

foldTVs :: (p -> Text -> x -> x) -> Exp p -> x -> x
foldTVs f (TVar p v) = f p v
foldTVs f (Bin _ _ lx rx) = foldTVs f rx . foldTVs f lx 
foldTVs f (Una _ _ ex) = foldTVs f ex 
foldTVs f (Brack _ ex) = foldTVs f ex 
foldTVs f (TLam _ ex) = foldTVs f ex 
foldTVs _ _ = id

problems :: Exp Note -> [Note]
problems ex = Data.Foldable.foldr folder [] ex
    where 
        folder (Note sp (Right ex)) b = b 
        folder n@(Note _ (Left _)) b = n : b 

notarize :: Functor f => Exp () -> f SP -> f Note
notarize ex = fmap (\sp -> Note sp (Right ex)) 

getTVars :: Exp a -> SymTbl Text
getTVars ex = foldTVs (\ _ tv -> Data.Map.insert tv tv) ex Data.Map.empty    

freshenTVar :: SymTbl Text -> Text -> Checker Text 
freshenTVar tvs tv = do 
    inCtx tv >>= \case 
        True -> doNext tv tvs 
        False -> pure tv 
    where 
        doNext x tvs = nextUId >>= \n -> let tv' = tv <> pack (show n) in do
            ctx <- context <$> get
            if Data.Map.member tv' tvs || Data.Map.member tv' ctx
                then doNext tv tvs 
                else pure tv'
            
freshenSymTbl :: SymTbl Text -> Checker (SymTbl Text) 
freshenSymTbl tvs = mapM (freshenTVar tvs) tvs 

freshen :: Exp a -> Checker (Exp a)
freshen ex = do 
    tvs <- freshenSymTbl (getTVars ex) 
    mapM_ (\tv -> insertCtx tv (TVar () tv)) tvs 
    pure $ mapTVs (apply tvs) ex
    where 
        apply tvs p tv = case Data.Map.lookup tv tvs of
            (Just tv') -> TVar p tv' 
            Nothing -> TVar p tv
 
fresh :: a -> Checker (Exp a) 
fresh a = freshen (TVar a "x")

findRoot :: Text -> Checker (Exp ()) 
findRoot tv = (context <$> get) >>= \ctx -> case Data.Map.lookup tv ctx of 
    Just (TVar _ tv') -> if tv /= tv'
        then findRoot tv'
        else pure $ TVar () tv
    Just ex -> pure ex
    Nothing -> pure $ TVar () tv

occursChk :: Text -> Exp () -> Checker ()
occursChk tv ex = foldTVs (\sp tv' m -> 
    if tv == tv' 
        then throwError $ "occurs checkExp " <> tv <> " = " <> tv'
        else m >> pure ()) ex (pure ()) 

bind :: Text -> Exp () -> Checker (Exp ())
bind varName newTy = do
    simpTy <- subst newTy 
    root <- findRoot varName 
    case root of 
        (TVar _ tv) -> do 
            occursChk tv simpTy 
            insertCtx tv simpTy
        _ -> 
            root `unify` simpTy

unify :: Exp () -> Exp () -> Checker (Exp ())
unify (Iden _ lv) (Iden _ rv) = if lv == rv 
    then pure (Iden () lv)
    else throwError $ "could not unify iden " <> lv <> " with " <> rv

unify (TVar _ lv) rx = bind lv rx
unify lx (TVar _ rv) = bind rv lx

unify (Bin sp lo ll lr) (Bin _ ro rl rr) = if lo == ro
    then Bin sp ro <$> unify ll rl <*> unify lr rr 
    else throwError $ "could not unify operator " <> lo <> " with " <> ro

unify (Una lp lo lr) (Una rp ro rr) = if lo == ro 
    then Una rp ro <$> unify lr rr 
    else throwError $ "could not unify operator " <> lo <> " with " <> ro

unify (Brack lp lx) (Brack rp rx) = unify lx rx

unify lx rx = throwError $ 
    "could not unify " <> cute lx <> " with " <> cute rx

subst :: Exp () -> Checker (Exp ()) 
subst (TVar () tv) = do
    root <- findRoot tv 
    case root of 
        TVar _ _' -> pure root
        ex -> subst ex 
subst (Una sp op rx) = Una sp op <$> subst rx 
subst (Bin sp op lx rx) =  Bin sp op <$> subst lx <*> subst rx 
subst (Brack sp x) = Brack sp <$> subst x  
subst ex = pure ex

checkApp :: Exp () -> Exp () -> Checker (Exp ())
checkApp (Bin _ "->" dom cod) vty = do 
    unify dom vty
    subst cod 
checkApp lx rx = throwError $
    "could not apply type " <> cute rx <> " to non-function type " <> cute lx 

annotate :: 
    SP -> 
    Text -> 
    Checker (Exp Note, Exp ()) -> 
    Checker (Exp Note, Exp ())
annotate sp msg ma = 
    let fullMsg e = e <> "\n" <> msg 
        stubify = Stub . Note sp . Left . fullMsg
    in catchError ma $ \e -> pure (stubify e, TBot)

checkExp :: Exp SP -> Checker (Exp Note, Exp ())
checkExp (Iden sp v) = 
    annotate sp ("type for identifier (" <> v <> ") not found") $ do 
        ty <- lookupEnv v >>= freshen
        pure (Iden (Note sp (Right ty)) v, ty)

checkExp ex@(IntLit sp v) = 
    let ty = Iden () "IntLike" in 
    pure (IntLit (Note sp (Right ty)) v, ty)

checkExp (DblLit sp v) = 
    let ty = Iden () "DblLike" in 
    pure (DblLit (Note sp (Right ty)) v, ty)

checkExp (StrLit sp v) = 
    let ty = Iden () "String" in
    pure (StrLit (Note sp (Right ty)) v, ty)

checkExp (CharLit sp v) = 
    let ty = Iden () "Char" in
    pure (Iden (Note sp (Right ty)) (pack [v]), ty)

checkExp (Brack sp ex) = do 
    (ex', ty) <- checkExp ex 
    pure (Brack (Note sp (Right ty)) ex', ty)

checkExp (Bin sp op lx rx) = 
    annotate sp ("binary operator (" <> op <> ") failed to type check") $ do
        ot <- lookupEnv op
        (lx', lt) <- checkExp lx
        (rx', rt) <- checkExp rx
        tt <- checkApp ot lt
        ty <- checkApp tt rt
        pure (Bin (Note sp (Right ty)) op lx' rx', ty)

checkExp (Una sp op rx) = 
    annotate sp ("unary operator (" <> op <> ") failed to type check") $ do
        ot <- lookupEnv op
        (rx', rt) <- checkExp rx
        ty <- checkApp ot rt
        pure (Una (Note sp (Right ty)) op rx', ty)

checkExp (Lam sp args bdy) = scopedEnv $ do 
    args' <- mapM checkArg args 
    (bdy', rty) <- checkBody bdy
    let lamTy = Bin () "-->" (argSig args') rty 
    pure (Lam (Note sp (Right lamTy)) args' bdy', lamTy) 

checkExp ex = throwError $ "could not type check expression " <> cute ex

argSig :: [Arg Note] -> Exp () 
argSig [Arg (Name (Note _ mty) _) _] = 
    either (const TBot) id mty
argSig (Arg (Name (Note _ mty) _) _ : bs) = 
    Bin () "," (either (const TBot) id mty) (argSig bs)
argSig [] = TUnit

checkArg :: Arg SP -> Checker (Arg Note) 
checkArg (Arg (Name p n) Nothing) = do 
    t <- fresh () 
    insertEnv n t
    pure $ Arg (Name (Note p $ Right t) n) Nothing 
checkArg (Arg (Name p n) (Just t)) = do 
    (t', _) <- freshen t >>= checkExp 
    let tyNeat = const () <$> t' 
    insertEnv n tyNeat
    pure $ Arg (Name (Note p $ Right tyNeat) n) Nothing

checkBody :: Body SP -> Checker (Body Note, Exp ())
checkBody (Compound stmts) = do 
    b <- Compound <$> mapM checkBlockStmt stmts 
    r <- returns <$> get
    case r of 
        Just t -> pure (b, t) 
        Nothing -> pure (b, TVoid) 
checkBody (Inline e) = do 
    (e', t) <- checkExp e 
    pure (Inline e', t)

formatMiss :: SP -> Exp SP -> Exp () -> Text -> Text 
formatMiss sp exp ty msg = 
    pack (sourcePosPretty sp) <> 
    " " <> msg <> " " <> cute exp <> ":" <> cute ty

checkBlockStmt :: BlockStmt SP -> Checker (BlockStmt Note)
checkBlockStmt (Ret sp (Just ex)) = returns <$> get >>= \case 
    Just rt -> do 
        (ex', ty) <- checkExp ex 
        rt' <- unify rt ty 
        willReturn $ Just rt'
        pure $ Ret (Note sp $ Right TVoid) (Just ex')
    Nothing -> do
        (ex', ty) <- checkExp ex 
        willReturn $ Just ty 
        pure $ Ret (Note sp $ Right TVoid) (Just ex')

checkBlockStmt (Ret sp Nothing) = returns <$> get >>= \case 
    Just rt -> 
        let msg = pack (sourcePosPretty sp) <> 
                " returned void instead of " <> cute rt 
        in pure $ Ret (Note sp $ Left msg) Nothing
    Nothing -> 
        pure $ Ret (Note sp (Right TVoid)) Nothing

checkBlockStmt (If sp cond stmts) = do 
    (cond', cty) <- checkExp cond 
    unify cty (Iden () "Bool")
    stmts' <- mapM checkBlockStmt stmts 
    pure $ If (Note sp (Right TVoid)) cond' stmts'

checkBlockStmt (While sp cond stmts) = do 
    (cond', cty) <- checkExp cond 
    unify cty (Iden () "Bool")
    stmts' <- mapM checkBlockStmt stmts 
    pure $ While (Note sp (Right TVoid)) cond' stmts'
    
checkBlockStmt (DoWhile sp stmts cond) = do 
    (cond', cty) <- checkExp cond 
    unify cty (Iden () "Bool") 
    stmts' <- mapM checkBlockStmt stmts 
    pure $ DoWhile (Note sp (Right TVoid)) stmts' cond'

checkBlockStmt (For sp init cond inc stmts) = do 
    (cond', cty) <- checkExp cond 
    unify cty (Iden () "Bool") 
    (init', _) <- checkExp init 
    (inc', _) <- checkExp inc
    stmts' <- mapM checkBlockStmt stmts  
    pure $ For (Note sp (Right TVoid)) init' cond' inc' stmts'

checkBlockStmt (Else sp stmts) = 
    Else (Note sp (Right TVoid)) <$> mapM checkBlockStmt stmts 

checkBlockStmt (Let sp a@(Arg (Name _ iden) Nothing) ex) = do 
    fresh () >>= insertEnv iden
    Let (Note sp (Right TVoid)) (notarize TVoid a) . fst <$> checkExp ex

checkBlockStmt (Let sp (Arg n@(Name nsp iden) (Just ty)) ex) = do 
    ty' <- fmap (const ()) . fst <$> (freshen ty >>= checkExp)
    insertEnv iden ty'
    (ex', _) <- checkExp ex 
    let arg' = Arg (Name (Note nsp $ Right ty') iden) Nothing
    pure $ Let (Note sp (Right TVoid)) arg' ex' 
 
checkBlockStmt (ExpStmt ex) = ExpStmt . fst <$> checkExp ex

checkBlockStmt (Mat sp cond cs) = undefined 

checkBlockStmt (Cont sp) = pure $ Cont $ Note sp $ Right TVoid
checkBlockStmt (Brk sp) = pure $ Brk $ Note sp $ Right TVoid

checkCase :: Case SP -> Checker (Case Note) 
checkCase = undefined 

checkFun :: FileStmt SP -> Checker (FileStmt SP)
checkFun (Fun p n as bs) = undefined 

checkFileStmt :: FileStmt SP -> Checker (FileStmt SP)
checkFileStmt = undefined 

precheckTy :: Exp SP -> Checker (Exp SP) 
precheckTy (TLam sp ex) = 
    TLam sp <$> 
    precheckTy ex
precheckTy (Bin sp op lx rx) = 
    Bin sp op <$> 
    precheckTy lx <*> 
    precheckTy rx 
precheckTy (Una sp op rx) = 
    Una sp op <$> 
    precheckTy rx 
precheckTy (Brack sp ex) = 
    Brack sp <$> 
    precheckTy ex 
precheckTy (TVoid) = 
    pure TVoid
precheckTy (Iden sp s) = environment <$> get >>= \en -> 
    if Data.Map.member s en
        then pure $ Iden sp s 
        else pure $ TVar sp s 
precheckTy _ = throwError "Found term in type"

precheckArg :: Arg SP -> Checker (Arg SP)
precheckArg (Arg n (Just ty)) = Arg n . Just <$> precheckTy ty 
precheckArg (Arg n Nothing) = pure (Arg n Nothing)

precheckTrm :: Exp SP -> Checker (Exp SP) 
precheckTrm (Lam sp args (Inline exp)) = 
    Lam sp <$> 
    mapM precheckArg args <*> 
    (Inline <$> precheckTrm exp)
precheckTrm (Lam sp args (Compound stmts)) = 
    Lam sp <$> 
    mapM precheckArg args <*> 
    (Compound <$> mapM precheckBlockStmt stmts)
precheckTrm (Bin sp op lx rx) = 
    Bin sp op <$> 
    precheckTrm lx <*> 
    precheckTrm rx 
precheckTrm (Una sp op rx) = Una sp op <$> precheckTrm rx
precheckTrm (Brack sp ex) = Brack sp <$> precheckTrm ex 
precheckTrm (TLam sp _) = throwError $ 
    pack (sourcePosPretty sp) <> " found TLam in term"
precheckTrm (TVar sp _) = throwError $ 
    pack (sourcePosPretty sp) <> " found TVar in term"
precheckTrm ex = pure ex 

precheckBlockStmt :: BlockStmt SP -> Checker (BlockStmt SP)
precheckBlockStmt (If sp ex stmts) = 
    If sp <$> 
    precheckTrm ex <*> 
    mapM precheckBlockStmt stmts
precheckBlockStmt (Else sp stmts) = 
    Else sp <$> 
    mapM precheckBlockStmt stmts
precheckBlockStmt (While sp ex stmts) = 
    While sp <$> 
    precheckTrm ex <*> 
    mapM precheckBlockStmt stmts 
precheckBlockStmt (DoWhile sp stmts ex) = 
    DoWhile sp <$> 
    mapM precheckBlockStmt stmts <*> 
    precheckTrm ex 
precheckBlockStmt (For sp init cond inc stmts) = 
    For sp <$> 
    precheckTrm init <*> 
    precheckTrm cond <*> 
    precheckTrm inc <*> 
    mapM precheckBlockStmt stmts
precheckBlockStmt (Ret sp (Just ex)) = 
    Ret sp . Just <$> precheckTrm ex
precheckBlockStmt (Let sp arg ex) = 
    Let sp <$> precheckArg arg <*> precheckTrm ex
precheckBlockStmt (Mat sp ex cs) = 
    Mat sp <$> precheckTrm ex <*> undefined 
precheckBlockStmt (ExpStmt ex) = 
    ExpStmt <$> precheckTrm ex  
precheckBlockStmt stmt = pure stmt 

precheckMemb :: Memb SP -> Checker (Memb SP) 
precheckMemb (Memb n ex) = Memb n <$> precheckTy ex

precheckFileStmt :: FileStmt SP -> Checker (FileStmt SP) 
precheckFileStmt (Sum sp n ns mems) = 
    Sum sp n ns <$> 
    mapM precheckMemb mems
precheckFileStmt (Rec sp n ns mems) =
    Rec sp n ns <$> 
    mapM precheckMemb mems
precheckFileStmt (Fun sp n args (Inline ex)) = 
    Fun sp n <$> 
    mapM precheckArg args <*> 
    (Inline <$> precheckTrm ex)
precheckFileStmt (Dec sp n ex) = 
    Dec sp n <$> 
    precheckTrm ex  
precheckFileStmt (Def sp n arg ex) =
    Def sp n <$> 
    precheckArg arg <*> 
    precheckTrm ex 
precheckFileStmt stmt = pure stmt 