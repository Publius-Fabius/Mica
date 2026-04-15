{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mica.Checker where 

import Mica.Cute
import Mica.Type
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.Map
import Data.Text
import Data.Maybe
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

instance Typed Note where 
    typeof (Note _ (Right t)) = t 
    typeof (Note _ (Left _)) = TBot

willReturn :: Maybe (Exp ()) -> Checker ()
willReturn rt = state (\st -> ((), st { returns = rt }))

withNewReturnCtx :: Checker a -> Checker a 
withNewReturnCtx ma = do 
    r <- gets returns
    willReturn Nothing
    a <- ma
    willReturn r
    pure a 
    
scopedCtx :: Checker a -> Checker a 
scopedCtx ma = do 
    c <- gets context 
    u <- gets uid  
    a <- ma 
    state $ \s -> (a, s{ context = c, uid = u })

scopedEnv :: Checker a -> Checker a 
scopedEnv ma = do 
    env <- gets environment 
    a <- ma 
    state (\st -> (a, st{ environment = env }))

lookupEnv :: Text -> Checker (Exp ()) 
lookupEnv k = do 
    env <- gets environment
    case Data.Map.lookup k env of 
        Just ex -> pure ex 
        Nothing -> throwError $ "type not found in environment for " <> k

insertEnv :: Text -> Exp () -> Checker (Exp ()) 
insertEnv k ex = state $ \st -> 
    (ex, st{ environment = Data.Map.insert k ex $ environment st })

insertCtx :: Text -> Exp () -> Checker (Exp ()) 
insertCtx k ex = state $ \st -> 
    (ex, st{ context = Data.Map.insert k ex (context st) })

inCtx :: Text -> Checker Bool 
inCtx k = gets (Data.Map.member k . context) 

nextUId :: Checker Int 
nextUId = state $ \st -> (uid st, st{ uid = uid st + 1 })

mapTVs :: (a -> Text -> Exp a) -> Exp a -> Exp a 
mapTVs f (TVar a v) = f a v
mapTVs f (Bin a op lx rx) = Bin a op (mapTVs f lx) (mapTVs f rx)
mapTVs f (Una a op ex) = Una a op $ mapTVs f ex
mapTVs f (Brack a ex) = Brack a $ mapTVs f ex
mapTVs _ ex = ex 

foldTVs :: (p -> Text -> x -> x) -> Exp p -> x -> x
foldTVs f (TVar p v) = f p v
foldTVs f (Bin _ _ lx rx) = foldTVs f rx . foldTVs f lx 
foldTVs f (Una _ _ ex) = foldTVs f ex 
foldTVs f (Brack _ ex) = foldTVs f ex 
foldTVs _ _ = id

problems :: Exp Note -> [Note]
problems = Data.Foldable.foldr folder []
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
            ctx <- gets context
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
findRoot tv = gets context >>= \ctx -> case Data.Map.lookup tv ctx of 
    Just (TVar _ tv') -> if tv /= tv'
        then findRoot tv'
        else pure $ TVar () tv
    Just ex -> pure ex
    Nothing -> pure $ TVar () tv

occursChk :: Text -> Exp () -> Checker ()
occursChk tv ex = foldTVs (\sp tv' m -> 
    if tv == tv' 
        then throwError $ "occurs checkExp " <> tv <> " = " <> tv'
        else void m) ex (pure ()) 

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
    Checker (Exp Note) -> 
    Checker (Exp Note)
annotate sp msg ma = 
    let fullMsg e = e <> "\n" <> msg 
        stubify = Stub . Note sp . Left . fullMsg
    in catchError ma (pure . stubify)

checkExp :: Exp SP -> Checker (Exp Note)
checkExp (Iden sp v) = 
    annotate sp ("type for identifier (" <> v <> ") not found") $ do 
        ty <- lookupEnv v >>= freshen
        pure $ Iden (Note sp (Right ty)) v

checkExp ex@(IntLit sp v) = 
    let ty = Iden () "IntLike" in 
    pure $ IntLit (Note sp (Right ty)) v

checkExp (DblLit sp v) = 
    let ty = Iden () "DblLike" in 
    pure $ DblLit (Note sp (Right ty)) v

checkExp (StrLit sp v) = 
    let ty = Iden () "String" in
    pure $ StrLit (Note sp (Right ty)) v

checkExp (CharLit sp v) = 
    let ty = Iden () "Char" in
    pure $ Iden (Note sp (Right ty)) (pack [v])

checkExp (Brack sp ex) = do 
    ex' <- checkExp ex 
    let ty = Brack () (typeof ex')
    pure $ Brack (Note sp (Right ty)) ex'

checkExp (Bin sp op lx rx) = 
    annotate sp ("binary operator (" <> op <> ") failed to type check") $ do
        opTy <- lookupEnv op
        lx' <- checkExp lx
        rx' <- checkExp rx
        tmpTy <- checkApp opTy (typeof lx')
        resTy <- checkApp tmpTy (typeof rx')
        pure $ Bin (Note sp (Right resTy)) op lx' rx'

checkExp (Una sp op ex) = 
    annotate sp ("unary operator (" <> op <> ") failed to type check") $ do
        opTy <- lookupEnv op
        ex' <- checkExp ex
        apTy <- checkApp opTy (typeof ex')
        pure $ Una (Note sp (Right apTy)) op ex'

checkExp (Lam sp args body) = scopedEnv $ do 
    args' <- mapM checkArg args 
    body' <- checkBody body
    let lamTy = Bin () "-->" (argSig (typeof <$> args')) (typeof body') 
    pure $ Lam (Note sp (Right lamTy)) args' body'

checkExp ex = throwError $ "could not type check expression " <> cute ex

argSig :: [Exp ()] -> Exp () 
argSig [] = TUnit
argSig [ty] = ty
argSig (a : bs) = Bin () "," a (argSig bs)

checkArg :: Arg SP -> Checker (Arg Note) 
checkArg (Arg (Name p n) Nothing) = do 
    t <- fresh () 
    insertEnv n t
    pure $ Arg (Name (Note p $ Right t) n) Nothing
checkArg (Arg (Name p n) (Just t)) = do 
    t' <- freshen t >>= checkExp 
    let tyNeat = void t' 
    insertEnv n tyNeat
    pure $ Arg (Name (Note p $ Right tyNeat) n) Nothing

checkBody :: Body SP -> Checker (Body Note)
checkBody (Compound sp stmts) = withNewReturnCtx $ do 
    stmts' <- mapM checkBlockStmt stmts
    mRetTy <- gets returns
    let retTy = fromMaybe TVoid mRetTy 
    pure $ Compound (Note sp (Right retTy)) stmts'
checkBody (Inline ex) = Inline <$> checkExp ex

checkBlockStmt :: BlockStmt SP -> Checker (BlockStmt Note)
checkBlockStmt (Ret sp (Just ex)) = gets returns >>= \case 
    Just rt -> do 
        ex' <- checkExp ex 
        rt' <- unify rt (typeof ex') 
        willReturn $ Just rt'
        pure $ Ret (Note sp $ Right TVoid) (Just ex')
    Nothing -> do
        ex' <- checkExp ex 
        willReturn $ Just $ typeof ex' 
        pure $ Ret (Note sp $ Right TVoid) (Just ex')

checkBlockStmt (Ret sp Nothing) = gets returns >>= \case 
    Just rt -> 
        let msg = pack (sourcePosPretty sp) <> 
                " returned void instead of " <> cute rt 
        in pure $ Ret (Note sp $ Left msg) Nothing
    Nothing -> 
        pure $ Ret (Note sp (Right TVoid)) Nothing

checkBlockStmt (If sp cond stmts) = do 
    cond' <- checkExp cond 
    unify (typeof cond') (Iden () "Bool")
    stmts' <- mapM checkBlockStmt stmts 
    pure $ If (Note sp (Right TVoid)) cond' stmts'

checkBlockStmt (While sp cond stmts) = do 
    cond' <- checkExp cond 
    unify (typeof cond') (Iden () "Bool")
    stmts' <- mapM checkBlockStmt stmts 
    pure $ While (Note sp (Right TVoid)) cond' stmts'
    
checkBlockStmt (DoWhile sp stmts cond) = do 
    cond' <- checkExp cond 
    unify (typeof cond') (Iden () "Bool") 
    stmts' <- mapM checkBlockStmt stmts 
    pure $ DoWhile (Note sp (Right TVoid)) stmts' cond'

checkBlockStmt (For sp init cond inc stmts) = do 
    cond' <- checkExp cond 
    unify (typeof cond') (Iden () "Bool") 
    init' <- checkExp init 
    inc' <- checkExp inc
    stmts' <- mapM checkBlockStmt stmts  
    pure $ For (Note sp (Right TVoid)) init' cond' inc' stmts'

checkBlockStmt (Else sp stmts) = 
    Else (Note sp (Right TVoid)) <$> mapM checkBlockStmt stmts 

checkBlockStmt (Let sp arg ex) = 
    Let (Note sp (Right TVoid)) <$> checkArg arg <*> checkExp ex

checkBlockStmt (ExpStmt ex) = ExpStmt <$> checkExp ex

checkBlockStmt (Mat sp cond cs) = do 
    --(cx, cty) <- checkExp cond 
    --unify cty $ Iden () "Bool"
    undefined

checkBlockStmt (Cont sp) = pure $ Cont $ Note sp $ Right TVoid
checkBlockStmt (Brk sp) = pure $ Brk $ Note sp $ Right TVoid

checkCase :: Case SP -> Checker (Case Note) 
checkCase = undefined 

checkFun :: FileStmt SP -> Checker (FileStmt SP)
checkFun (Fun p n as bs) = undefined 

checkFileStmt :: FileStmt SP -> Checker (FileStmt SP)
checkFileStmt (Imp a s) = undefined 
checkFileStmt (Inc a s) = undefined 
checkFileStmt (Sum a n args injs) = undefined 
checkFileStmt (Rec a n args mems) = undefined 
checkFileStmt (Fun a n args bdy) = undefined 
checkFileStmt (Dec a n ex) = undefined 

precheckTy :: Exp SP -> Checker (Exp SP) 
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
precheckTy TVoid = 
    pure TVoid
precheckTy (Iden sp s) = gets environment >>= \en -> 
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
precheckTrm (Lam lsp args (Compound csp stmts)) = 
    Lam lsp <$> 
    mapM precheckArg args <*> 
    (Compound csp <$> mapM precheckBlockStmt stmts)
precheckTrm (Bin sp op lx rx) = 
    Bin sp op <$> 
    precheckTrm lx <*> 
    precheckTrm rx 
precheckTrm (Una sp op rx) = Una sp op <$> precheckTrm rx
precheckTrm (Brack sp ex) = Brack sp <$> precheckTrm ex 
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

precheckInj :: Memb SP -> Checker (Inj SP) 
precheckInj (Memb n ex) = Inj n <$> precheckTy ex

precheckFileStmt :: FileStmt SP -> Checker (FileStmt SP) 
precheckFileStmt (Sum sp n ns mems) = undefined
precheckFileStmt (Rec sp n ns mems) = undefined
precheckFileStmt (Fun sp n args (Inline ex)) = 
    Fun sp n <$> 
    mapM precheckArg args <*> 
    (Inline <$> precheckTrm ex)
precheckFileStmt (Dec sp n ex) = 
    Dec sp n <$> 
    precheckTrm ex   
precheckFileStmt stmt = pure stmt 
