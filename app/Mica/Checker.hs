{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Mica.Checker where 

import Mica.Cute
import Mica.Type
import Data.Functor 
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.Map
import Data.Text as T
import Data.Maybe
import Data.Char
import Text.Megaparsec
import qualified Data.List as L

type SP = SourcePos 
type SymTbl a = Map Text a 

data ChkSt = ChkSt {
    uid :: Int,
    provenance :: SymTbl SP,
    registry :: SymTbl (SP, Meta),
    context :: SymTbl (Exp ()),
    environment :: SymTbl (SP, Exp ()),
    returns :: Maybe (Exp ())
} deriving (Show)

newChkSt = ChkSt { 
    uid = 0,
    provenance = Data.Map.empty,
    registry = Data.Map.empty,
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
    state $ \st -> (a, st{ environment = env })

scopedPro :: Checker a -> Checker a 
scopedPro ma = do 
    pro <- gets provenance 
    a <- ma 
    state $ \st -> (a, st{ provenance = pro })

lookupEnv :: Text -> Checker (Exp ()) 
lookupEnv t = do 
    env <- gets environment 
    case Data.Map.lookup t env of 
        Just (_,ex) -> pure ex 
        Nothing -> throwError $ 
            "symbol (" <> t <> ") not found in the environment" 

symbolsCollide :: SP -> SP -> Text -> Text 
symbolsCollide p1 p2 sym = 
    pack (sourcePosPretty p1) <> 
    "\nsymbol (" <> sym <> ") collides with another symbol introduced at:\n" <>
    pack (sourcePosPretty p2)

insertEnv :: SP -> Text -> Exp () -> Checker (Exp ()) 
insertEnv p1 t ex = do 
    env <- gets environment 
    case Data.Map.lookup t env of 
        Just (p2, _) -> throwError $ symbolsCollide p1 p2 t
        Nothing -> state $ \st -> (ex, st { environment = 
            Data.Map.insert t (p1, ex) (environment st) })

insertPro :: SP -> Text -> Checker SP 
insertPro p1 t = do 
    pro <- gets provenance 
    case Data.Map.lookup t pro of 
        Just p2 -> throwError $ symbolsCollide p1 p2 t
        Nothing -> state $ \st -> (p1, st { provenance = 
            Data.Map.insert t p1 (provenance st) })

insertCtx :: Text -> Exp () -> Checker (Exp ()) 
insertCtx k ex = state $ \st -> 
    (ex, st{ context = Data.Map.insert k ex (context st) })

insertReg :: SP -> Text -> Meta -> Checker () 
insertReg sp k fs = state $ \st -> 
    ((), st{ registry = Data.Map.insert k (sp, fs) (registry st) })

inCtx :: Text -> Checker Bool 
inCtx k = gets (Data.Map.member k . context) 

nextUId :: Checker Int 
nextUId = state $ \st -> (uid st, st{ uid = uid st + 1 })

mapVars :: ExpTransform m => (a -> Text -> Exp a) -> m a -> m a 
mapVars f = mapx mapper where 
    mapper (Var p v) = f p v 
    mapper x = x 

foldVars :: ExpTransform m => (p -> Text -> x -> x) -> m p -> x -> x
foldVars f = foldx folder where 
    folder (Var p v) s = f p v s 
    folder _ s = s 

mapIdens :: ExpTransform m => (a -> Text -> Exp a) -> m a -> m a 
mapIdens f = mapx mapper where 
    mapper (Iden p v) = f p v
    mapper x = x

foldIdens :: ExpTransform m => (p -> Text -> x -> x) -> m p -> x -> x
foldIdens f = foldx folder where 
    folder (Iden p v) s = f p v s 
    folder _ s = s 

problems :: Foldable m => m Note -> [Note]
problems = Data.Foldable.foldr folder [] where 
    folder (Note sp (Right ex)) b = b 
    folder n@(Note _ (Left _)) b = n : b 

notarize :: Functor f => Exp () -> f SP -> f Note
notarize ex = fmap (\sp -> Note sp (Right ex)) 

getVars :: Exp a -> SymTbl Text
getVars ex = foldVars folder ex Data.Map.empty where 
    folder _ tv = Data.Map.insert tv tv 

freshenVar :: SymTbl Text -> Text -> Checker Text 
freshenVar tvs tv =  
    inCtx tv >>= \case 
        True -> getNext 
        False -> pure tv 
    where 
        getNext = do 
            nxt <- nextUId 
            ctx <- gets context
            let tv' = tv <> pack (show nxt)
            if Data.Map.member tv' tvs || Data.Map.member tv' ctx
                then getNext 
                else pure tv'
 
freshenSymTbl :: SymTbl Text -> Checker (SymTbl Text) 
freshenSymTbl tvs = mapM (freshenVar tvs) tvs 

freshen :: Exp a -> Checker (Exp a)
freshen ex = do 
    tvs <- freshenSymTbl (getVars ex)
    forM_ tvs $ \tv -> insertCtx tv $ Var () T.empty  
    pure $ mapVars (apply tvs) ex
    where 
        apply tvs p tv = case Data.Map.lookup tv tvs of
            (Just tv') -> Var p tv' 
            Nothing -> Var p tv
 
fresh :: a -> Checker (Exp a) 
fresh a = freshen (Var a "x")

findRoot :: Text -> SymTbl (Exp ()) -> Exp ()
findRoot tv ctx = case Data.Map.lookup tv ctx of 
    Just (Var () tv') -> if tv' /= T.empty
        then findRoot tv' ctx
        else Var () tv
    Just ex -> ex
    Nothing -> Var () tv

occursChk :: Text -> Exp () -> Checker ()
occursChk tv ex = foldVars folder ex (pure ()) where 
    folder _ tv' m = if tv == tv' 
        then throwError ("occurs checkExp " <> tv <> " |- " <> cute ex)
        else m :: Checker () 

bind :: Text -> Exp () -> Checker (Exp ())
bind varName newTy = do
    ctx <- gets context 
    let simpTy = subst newTy ctx 
    case findRoot varName ctx of 
        (Var _ tv) -> do 
            occursChk tv simpTy 
            insertCtx tv simpTy
        root -> unify root simpTy

unify :: Exp () -> Exp () -> Checker (Exp ())
unify (Iden _ lv) (Iden _ rv) = if lv == rv 
    then pure (Iden () lv)
    else throwError $ "could not unify iden " <> lv <> " with " <> rv

unify lx (Var _ rv) = bind rv lx
unify (Var _ lv) rx = bind lv rx

unify (Bin _ lo ll lr) (Bin _ ro rl rr) = if lo == ro
    then Bin () ro <$> unify ll rl <*> unify lr rr 
    else throwError $ "could not unify operator " <> lo <> " with " <> ro

unify (Una _ lo lr) (Una _ ro rr) = if lo == ro 
    then Una () ro <$> unify lr rr 
    else throwError $ "could not unify operator " <> lo <> " with " <> ro

unify (Brack _ lx) (Brack _ rx) = unify lx rx

unify lx rx = throwError $ 
    "could not unify " <> cute lx <> " with " <> cute rx

subst :: Exp () -> SymTbl (Exp ()) -> Exp () 
subst ex ctx = mapVars mapper ex where
    mapper _ tv = case findRoot tv ctx of 
        Var p v -> Var p v
        nxt -> subst nxt ctx
    
checkApp :: Exp () -> Exp () -> Checker (Exp ())
checkApp (Bin _ "->" dom cod) vty = do 
    unify dom vty
    gets (subst cod . context) 
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

checkExp (Iden sp v) = annotate sp "" $ do 
    -- Lookup symbol and freshen into substitution context.
    ty <- lookupEnv v >>= freshen 
    pure $ Iden (Note sp (Right ty)) v

checkExp (Var sp v) = annotate sp "" $ do
    -- Lookup already fresh var. 
    ty <- lookupEnv v 
    pure $ Var (Note sp (Right ty)) v

checkExp (IntLit sp v) = 
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

skolemize :: [Text] -> Exp a -> Exp a 
skolemize ubis = mapIdens mapper 
    where 
        mapper p i = if i `L.elem` ubis 
            then Var p i
            else Iden p i

unboundIdens :: Exp SP -> Checker [(SP, Text)] 
unboundIdens ex = do 
    let idens = L.nubBy (\a b -> snd a == snd b) $ 
            foldIdens (\a t x -> (a, t) : x) ex [] 
    let folder x (p, t) = gets environment >>= \env -> 
            if Data.Map.member t env
                then pure x :: Checker [(SP, Text)]
                else pure $ (p, t) : x
    foldM folder [] idens 

checkArg :: Arg SP -> Checker (Arg Note) 
checkArg (Arg (Name p n) Nothing) = do 
    t <- fresh () 
    insertEnv p n t
    pure $ Arg (Name (Note p $ Right t) n) Nothing
checkArg (Arg (Name np n) (Just t)) = do 
    ubis <- unboundIdens t
    t' <- scopedEnv $ do 
        forM_ ubis $ \(ip, i) -> do 
            iTy <- fresh () 
            insertEnv ip i iTy 
        skolemize (snd <$> ubis) . void <$> checkExp t
    tyNeat <- freshen t' 
    insertEnv np n tyNeat 
    pure $ Arg (Name (Note np $ Right tyNeat) n) Nothing 

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

-- preBody :: Body SP -> Checker (Body SP) 
-- preBody (Inline ex) = Inline <$> preTrm ex 
-- preBody (Compound sp stmts) = Compound sp <$> mapM preBlockStmt stmts
--
-- startsWithLower :: Text -> Bool 
-- startsWithLower t = isLower $ Data.Text.head t 
--
-- preTy :: Exp SP -> Checker (Exp SP) 
-- preTy (Bin sp op lx rx) = Bin sp op <$> preTy lx <*> preTy rx 
-- preTy (Una sp op rx) = Una sp op <$> preTy rx 
-- preTy (Brack sp ex) = Brack sp <$> preTy ex 
-- preTy (Iden p s) = if startsWithLower s 
--     then pure $ Var p s 
--     else pure $ Iden p s 
-- preTy t = pure t

-- preArg :: Arg SP -> Checker (Arg SP)
-- preArg (Arg n mt) = Arg <$> 
--     preBindName n <*> 
--     maybe (pure Nothing) (fmap Just . preTy) mt 

-- preTrm :: Exp SP -> Checker (Exp SP) 
-- preTrm (Lam sp args body) = Lam sp <$> mapM preArg args <*> preBody body 
-- preTrm (Bin sp op lx rx) = Bin sp op <$> preTrm lx <*> preTrm rx 
-- preTrm (Una sp op rx) = Una sp op <$> preTrm rx
-- preTrm (Brack sp ex) = Brack sp <$> preTrm ex 
-- preTrm ex = pure ex 
--
alreadyDefined :: SP -> SP -> Text -> Text 
alreadyDefined sa sb n = 
    pack (sourcePosPretty sa) <> 
    "\nsymbol (" <> n <> ") was already defined at:\n" <>
    pack (sourcePosPretty sb) 

arityMismatch :: SP -> SP -> Text -> Text 
arityMismatch sa sb n =
    pack (sourcePosPretty sa) <> 
    "\narity of symbol (" <> n <> ") did not match declaration at:\n" <>
    pack (sourcePosPretty sb)

alreadyDeclared :: SP -> SP -> Text -> Text 
alreadyDeclared sa sb n = 
    pack (sourcePosPretty sa) <> 
    "\nsymbol (" <> n <> ") was already declared at:\n" <>
    pack (sourcePosPretty sb)

specifierMismatch :: SP -> SP -> Text -> Text 
specifierMismatch sa sb n = 
    pack (sourcePosPretty sa) <> 
    "\nspecifiers for symbol (" <> n <> ") don't match those declared at:\n" <>
    pack (sourcePosPretty sb)

checkRegForContainer :: SP -> Text -> Meta -> Checker () 
checkRegForContainer sp1 n m1 = do
    reg <- gets registry 
    case Data.Map.lookup n reg of 
        Just (sp2, m2) | matchesType m1 m2 -> void $ do 
            when (getArity m1 /= getArity m2) $ 
                throwError $ arityMismatch sp1 sp2 n
            when (isDef m1 && isDef m2) $
                throwError $ alreadyDefined sp1 sp2 n
            insertReg sp1 n m1
        Just (sp2, _) -> throwError $ alreadyDeclared sp1 sp2 n
        Nothing -> void $ do 
            insertPro sp1 n
            insertReg sp1 n m1
    where 
        matchesType IsSum {} IsSum {} = True 
        matchesType IsRec {} IsRec {} = True 
        matchesType _ _ = False 
        getArity (IsSum n _) = n 
        getArity (IsRec n _) = n 
        getArity _ = error "bad meta type passed to checkRegForContainer"
        isDef (IsSum _ b) = b 
        isDef (IsRec _ b) = b

checkRegForBinding :: SP -> Text -> Meta -> Checker () 
checkRegForBinding sp1 n m1 = do 
    reg <- gets registry 
    case Data.Map.lookup n reg of
        Just (sp2, m2) -> if matchesType m1 m2 
            then throwError $ alreadyDefined sp1 sp2 n 
            else void $ do 
                when (getSpes m1 /= getSpes m2) $ 
                    throwError $ specifierMismatch sp1 sp2 n 
                insertReg sp1 n m1
        Nothing -> void $ do 
            insertPro sp1 n
            insertReg sp1 n m1
    where 
        matchesType IsFun {} IsFun {} = True 
        matchesType IsTrm {} IsTrm {} = True
        matchesType _ _ = False

        getSpes (IsFun s) = s
        getSpes (IsTrm s) = s 
        getSpes (IsDec s)  = s
        getSpes _ = error "bad meta type passed to checkRegForBinding" 

liftVars :: ExpTransform m => SymTbl a -> m a -> m a
liftVars tbl = mapIdens $ \p v -> 
    if Data.Map.member v tbl
        then Iden p v
        else Var p v

preFileStmt :: FileStmt SP -> Checker (FileStmt SP) 
preFileStmt stmt@(Rec sp n ps ms) = do
    let ent = IsRec (L.length ps) (isJust ms) 
    checkRegForContainer sp (unname n) ent
    pro <- gets provenance
    pure $ Rec sp n ps (fmap (preMemb pro) <$> ms) 
    where 
        preMemb tbl (Memb mn mx) = Memb mn $ liftVars tbl mx
     
preFileStmt stmt@(Sum sp n ps ms) = do
    checkRegForContainer sp (unname n) $ IsSum (L.length ps) (isJust ms)
    pro <- gets provenance 
    pure $ Sum sp n ps (fmap (preInj pro) <$> ms) 
    where 
        preInj tbl (Inj mn mx) = Inj mn $ liftVars tbl mx

preFileStmt (Fun sps (Name p n) args body) = do
    let sps' = reduceSpecifiers sps
    checkRegForBinding p n $ IsFun sps' 
    pro <- gets provenance
    pure $ Fun sps' (Name p n) (liftVars pro <$> args) (liftVars pro body)

preFileStmt (Trm sps (Name p n) ex) = do
    let sps' = reduceSpecifiers sps
    checkRegForBinding p n $ IsTrm sps' 
    pro <- gets provenance 
    pure $ Trm sps' (Name p n) (liftVars pro ex)

preFileStmt (Dec sps (Name p n) ex) = do
    let sps' = reduceSpecifiers sps 
    checkRegForBinding p n $ IsDec sps' 
    pro <- gets provenance 
    pure $ Dec sps' (Name p n) (liftVars pro ex) 

preFileStmt stmt = pure stmt

--     Imp a Text | 
--     Inc a Text | 
--     Ext a (Name a) (Exp a) Text |
--     Lay a (Name a) (Layout a)
