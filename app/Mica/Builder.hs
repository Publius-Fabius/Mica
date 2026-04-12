{-# LANGUAGE OverloadedStrings #-}

module Mica.Builder where 

import Mica.Type 
import Control.Monad.State 
import Data.Text
import Data.Text.Lazy.Builder

data TranSt = TranSt { 
    result :: Builder, 
    indent :: Int, 
    tag :: Int } 
    deriving (Show)

type Transpiler a = State TranSt a

bWithNewTag :: Transpiler a -> Transpiler a 
bWithNewTag ma = do 
    cnt <- state (\st -> (tag st, st{ tag = 0 })) 
    a <- ma 
    state (\st -> (a, st{ tag = cnt }))

bIncTag :: Transpiler Int 
bIncTag = state (\st -> (tag st, st{ tag = tag st + 1 }))

bEmit :: Text -> Transpiler ()
bEmit txt = state (\st -> ((), st{ result = result st <> fromText txt }))

bArrowToList :: Exp a -> [Exp a]
bArrowToList (Bin _ "->" lx rx) = lx : bArrowToList rx 
bArrowToList rx = [rx]

bTy :: Exp a -> Transpiler () 
bTy = undefined 

bBlockStmt :: BlockStmt a -> Transpiler () 
bBlockStmt = undefined 

bSumEnum :: Memb a -> Transpiler ()
bSumEnum (Memb (Name _ name) _) = do 
    t <- bIncTag
    bEmit $ "#define " <> name <> "_tag " <> pack (show t) <> "\n"
    
bSumInjMemb :: Exp a -> Transpiler () 
bSumInjMemb ex = do
    cnt <- bIncTag
    bEmit "    "
    bTy ex 
    bEmit $ "member_" <> pack (show cnt) <> ";\n"

bSumInj :: Memb a -> Transpiler () 
bSumInj (Memb (Name _ name) ty) = bWithNewTag $ do
    bEmit $ "struct " <> name <> " {\n"
    mapM_ bSumInjMemb (bArrowToList ty)
    bEmit "}\n"

bFileStmt :: FileStmt a ->  Transpiler ()
bFileStmt (Inc _ path) = bEmit $ "#include \"" <> path <> "\"\n" 
bFileStmt (Imp _ path) = bEmit $ "#include \"" <> path <> "\"\n" 
bFileStmt (Sum _ (Name _ name) kind mems) = do
    bWithNewTag $ mapM_ bSumEnum mems
    mapM_ bSumInj mems
    bEmit $ "struct " <> name <> " {\n" 
    bEmit $ "    const int tag;\n"
    bEmit $ "    union {\n"
    mapM_ member mems 
    bEmit $ "    } variant;\n"
    bEmit "}\n"
    where 
        member (Memb (Name _ name) _) = 
            bEmit $ "        struct " <> name <> " member_" <> name <> ";\n"
bFileStmt (Rec _ (Name _ name) kind mems) = do 
    bEmit $ "struct " <> name <> " {\n"
    mapM_ member mems
    bEmit "}\n"
    where 
        member (Memb (Name _ name) ty) = do
            bEmit "    "
            bTy ty 
            bEmit $ " " <> name <> ";\n"
bFileStmt (Fun _ (Name _ name) args (Compound stmts)) = undefined


--     Fun a (Name a) [Arg a] (Body a) | 
--     Dec a (Name a) (Exp a) | 
--     Def a (Name a) (Arg a) (Exp a) 


