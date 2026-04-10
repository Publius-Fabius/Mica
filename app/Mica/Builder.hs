{-# LANGUAGE OverloadedStrings #-}

module Mica.Builder where 

import Mica.Type 
import Control.Monad.State 
import Data.Text.Lazy.Builder

data TranSt = TranSt { 
    result :: Builder, 
    indent :: Int, 
    count :: Int } 
    deriving (Show)

type Transpiler a = State 

bCountScope :: Transpiler a -> Transpiler a 
bCountScope ma = do 
    cnt <- count <$> get 
    a <- ma 
    state (\st -> (a, st{ count = cnt }))

bIncCount :: Transpiler Int 
bIncCount = state (\st -> (count st, st{ count = count st + 1 }))

bTy :: Exp a -> Transpiler () 
bTy = undefined 

bEmit :: Text -> Transpiler ()
bEmit txt = state (\st -> ((), st{ result = result st <> txt }))

bFlattenArrow :: Exp a -> [Exp a]
bFlattenArrow (Op _ "->" lx rx) = lx : members rx 
bFlattenArrow rx = rx

bBlockStmt :: BlockStmt a -> Transpiler () 
bBlockStmt = undefined 

bSumMembMemb :: Exp a -> Transpiler () 
bSumMembMemb ex = do
    cnt <- bIncCount
    bEmit "    "
    bTy ex 
    bEmit $ "member_" <> pack (show cnt) <> ";\n"

bSumMemb :: Memb a ->  Transpiler () 
bSumMemb (Memb (Name _ name) ty) = bCountScope $ do
    bEmit "struct " <> name <> " {\n"
    mapM_ bSumMembMemb (bFlattenArrow ty)
    bEmit "}\n"

bFileStmt :: FileStmt a ->  Transpiler ()
bFileStmt (Inc _ path) = bEmit $ "#include <" <> path <> ">\n" 
bFileStmt (Imp _ path) = bEmit $ "#include <" <> path <> ">\n" 
bFileStmt (Sum _ (Name _ name) mems) = do
    mapM_ bSumMemb mems
    bEmit $ "union " <> name <> " {\n" 
    mapM_ member mems 
    bEmit "}\n"
    where 
        member (Member (Name _ name) _) = 
            bEmit $ "   struct " <> name <> " " <> name <> "_;\n"
bFileStmt (Rec _ (Name _ name) mems) = do 
    bEmit $ "struct " <> name <> " {\n"
    mapM_ member mems
    bEmit "}\n"
    where 
        member (Member (Name _ name) ty) = do
            bEmit "    "
            bTy ty 
            bEmit $ " " <> name <> ";\n"
bFileStmt (Fun _ (Name _ name) args (Compound stmt)) = do

-- build lambda closures 


--     Fun a (Name a) [Arg a] (Body a) | 
--     Dec a (Name a) (Exp a) | 
--     Def a (Name a) (Arg a) (Exp a) 


