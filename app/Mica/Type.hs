{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Mica.Type where 

import Data.Text

-- World's best systems programming language. 
-- The one true spiritual successor to C.

data Arg a = 
    Arg (Name a) (Maybe (Exp a)) 
    deriving (Show, Eq, Ord, Functor) 

data Name a = 
    Name a Text 
    deriving (Show, Eq, Ord, Functor)

data Exp a =
    TVar a Text |
    TLam a (Exp a) |
    Iden a Text | 
    Lam a [Arg a] (Body a) | 
    Bin a Text (Exp a) (Exp a) | 
    Una a Text (Exp a) | 
    Tri a (Exp a) (Exp a) (Exp a) | 
    IntLit a Integer | 
    DblLit a Double | 
    StrLit a Text | 
    CharLit a Char |
    Brack a (Exp a) |
    TVoid 
    deriving (Show, Eq, Ord, Functor)

data BlockStmt a =
    If a (Exp a) [BlockStmt a] | 
    Else a [BlockStmt a] | 
    While a (Exp a) [BlockStmt a] | 
    DoWhile a [BlockStmt a] (Exp a) |
    For a (BlockStmt a) (Exp a) (BlockStmt a) [BlockStmt a] |
    Ret a (Maybe (Exp a)) | 
    Cont a | 
    Brk a | 
    Let a (Arg a) (Exp a) | 
    Mat a (Exp a) [Case a] |
    Assign (Exp a) 
    deriving (Show, Eq, Ord, Functor)

data Case a = 
    Case (Name a) [Name a] [BlockStmt a]
    deriving (Show, Eq, Ord, Functor)

data Body a = 
    Compound [BlockStmt a] |
    Inline (Exp a)
    deriving (Show, Eq, Ord, Functor)

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

prettyExp :: Exp a -> Text 
prettyExp (TVar _ s) = s 
prettyExp (Iden _ s) = s 
prettyExp (Lam _ _ _) = undefined
prettyExp (Bin _ op lx rx) = prettyExp lx <> " " <> op <> " " <> prettyExp rx
prettyExp (Una _ op ex) = op <> prettyExp ex
prettyExp (Tri _ _ _ _) = undefined
prettyExp (IntLit _ v) = pack $ show v
prettyExp (DblLit _ v) = pack $ show v
prettyExp (StrLit _ v) = pack $ show v
prettyExp (CharLit _ v) = pack $ show v
prettyExp (Brack _ x) = "[" <> prettyExp x <> "]"
