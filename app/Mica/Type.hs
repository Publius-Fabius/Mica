{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}

module Mica.Type where 

import Data.Text
import Mica.Cute

data Arg a = 
    Arg (Name a) (Maybe (Exp a)) 
    deriving (Show, Eq, Ord, Functor, Foldable) 

data Name a = 
    Name a Text 
    deriving (Show, Eq, Ord, Functor, Foldable)

data Exp a =
    TVar a Text |
    TLam a (Exp a) |
    Iden a Text | 
    Lam a [Arg a] (Body a) | 
    Bin a Text (Exp a) (Exp a) | 
    Una a Text (Exp a) | 
    IntLit a Integer | 
    DblLit a Double | 
    StrLit a Text | 
    CharLit a Char |
    Brack a (Exp a) |
    Stub a |
    TStar |
    TVoid | 
    TBot | 
    TUnit
    deriving (Show, Eq, Ord, Functor, Foldable)

data BlockStmt a = 
    If a (Exp a) [BlockStmt a] | 
    Else a [BlockStmt a] | 
    While a (Exp a) [BlockStmt a] | 
    DoWhile a [BlockStmt a] (Exp a) | 
    For a (Exp a) (Exp a) (Exp a) [BlockStmt a] | 
    Let a (Arg a) (Exp a) | 
    Ret a (Maybe (Exp a)) | 
    Mat a (Exp a) [Case a] | 
    Cont a | 
    Brk a | 
    ExpStmt (Exp a) 
    deriving (Show, Eq, Ord, Functor, Foldable)

data Case a = 
    Case (Name a) [Name a] [BlockStmt a]
    deriving (Show, Eq, Ord, Functor, Foldable)

data Body a = 
    Compound [BlockStmt a] |
    Inline (Exp a)
    deriving (Show, Eq, Ord, Functor, Foldable)

data Memb a = 
    Memb (Name a) (Exp a)
    deriving (Show, Eq, Ord, Functor)

data FileStmt a =
    Imp a Text |
    Inc a Text |
    Sum a (Name a) [Name a] [Memb a] | 
    Rec a (Name a) [Name a] [Memb a] | 
    Fun a (Name a) [Arg a] (Body a) | 
    Dec a (Name a) (Exp a) | 
    Def a (Name a) (Arg a) (Exp a) 
    deriving (Show, Eq, Ord, Functor)

instance Cute (Name a) where 
    cute (Name _ a) = a

instance Cute (Arg a) where 
    cute (Arg n (Just ty)) = "(" <> cute n <> ":" <> cute ty <> ")" 
    cute (Arg n Nothing) = cute n 

instance Cute (Exp a) where 
    cute (TVar _ v) = v
    cute (TLam _ ex) = cute ex 
    cute (Iden _ i) = i 
    cute (Bin _ op lx rx) = 
        "(" <> cute lx <> " " <> op <> " " <> cute rx <> ")"
    cute (Una _ op ex) = "(" <> op <> cute ex <> ")"
    cute (Brack _ ex) = "[" <> cute ex <> "]" 
    cute (Lam _ as b) = 
        "(\\ " <> Data.Text.unwords (cute <$> as) <> "->" <> cute b <> ")"
    cute (IntLit _ v) = pack $ show v
    cute (DblLit _ v) = pack $ show v
    cute (StrLit _ v) = pack $ show v
    cute (CharLit _ v) = pack $ show v
    cute TVoid = "Void"
    cute (Stub _) = "???"
    cute (TBot) = "_|_"

instance Cute (Body a) where 
    cute (Compound stmts) = 
        let stmts' = Data.Text.unwords (cute <$> stmts) 
        in "{" <> stmts' <> "}" 
    cute (Inline ex) = cute ex <> ";" 

instance Cute (Case a) where 
    cute (Case n as stmts) = 
        let cs' = Data.Text.unwords (cute <$> stmts) 
            as' = Data.Text.unwords (cute <$> as) 
        in cute n <> " " <> as' <> " => {" <> cs' <> "}"

instance Cute (BlockStmt a) where 
    cute (Mat _ pat cs) = 
        let cs' = Data.Text.unwords (cute <$> cs) in 
        "match(" <> cute pat <> ") {" <> cs' <> "}"
    cute (If _ cond stmts) = 
        let stmts' = Data.Text.unwords (cute <$> stmts) in
        "if (" <> cute cond <> ") {" <> stmts' <> "}"
    cute (Else _ stmts) = 
        let stmts' = Data.Text.unwords (cute <$> stmts) in
        "else {" <> stmts' <> "}"
    cute (While _ cond stmts) = 
        let stmts' = Data.Text.unwords (cute <$> stmts) in
        "while (" <> cute cond <> ") {" <> stmts' <> "}"
    cute (DoWhile _ stmts cond) = 
        let stmts' = Data.Text.unwords (cute <$> stmts) in
        "do {" <> stmts' <> "}" <> "while (" <> cute cond <> ")"
    cute (For _ ini cond inc stmts) = 
        let stmts' = Data.Text.unwords (cute <$> stmts) in
        "for (" <> cute ini <> "; " <> cute cond <> "; " <> cute inc <> ") {" 
        <> stmts' <> "}"
    cute (Let _ arg ex) = "let " <> cute arg <> " = " <> cute ex <> ";"
    cute (ExpStmt ex) = cute ex <> ";"
    cute (Ret _ (Just ex)) = "return " <> cute ex <> ";"
    cute (Ret _ Nothing) = "return;"
    cute (Cont _) = "continue;"
    cute (Brk _) = "break;"

instance Cute (Memb a) where 
    cute (Memb n ex) = cute n <> " : " <> cute ex <> ";"

instance Cute (FileStmt a) where 
    cute (Imp _ t) = "import " <> t <> ";" 
    cute (Inc _ t) = "include " <> t <> ";" 
    cute (Sum _ n ki mems) = 
        let mems' = Data.Text.unwords (cute <$> mems) 
            ki' = Data.Text.unwords (cute <$> ki) 
        in "typesum " <> cute n <> " " <> ki' <> " {" <> mems' <> "}"
    cute (Rec _ n ki mems) = 
        let mems' = Data.Text.unwords (cute <$> mems) 
            ki' = Data.Text.unwords (cute <$> ki) 
        in "record " <> cute n <> " " <> ki' <> " {" <> mems' <> "}"
    cute (Fun _ n as b) = 
        let as' = Data.Text.unwords (cute <$> as) 
        in "routine " <> cute n <> " " <> as' <> " = " <> cute b 
    cute (Dec _ n ex) = "declare " <> cute n <> " : " <> cute ex <> ";"
    cute (Def _ n a b) = "define " <> cute n <> "?????"

