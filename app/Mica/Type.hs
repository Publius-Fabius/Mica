{-# LANGUAGE DeriveFunctor #-}

module Mica.Type where 

-- World's best systems programming language. 
-- The one true spiritual successor to C.

data Reg a = 
    Heap a | 
    Stack a | 
    Local a | 
    Data a 
    deriving (Show, Eq, Ord, Functor) 

data Acc a = 
    Read a | 
    Write a 
    deriving (Show, Eq, Ord, Functor) 

data Arg a = 
    Arg (Name a) (Maybe (Exp a)) 
    deriving (Show, Eq, Ord, Functor) 

data Name a = 
    Name a String 
    deriving (Show, Eq, Ord, Functor)

data Exp a =
    TVar a String |
    TLam a (Reg a) (Acc a) (Exp a) | 
    TPtr a (Reg a) (Acc a) (Acc a) (Exp a) | 
    TArr a (Reg a) (Acc a) (Acc a) (Exp a) |
    TSli a (Reg a) (Acc a) (Acc a) (Exp a) |
    Lam a (Reg a) [Arg a] (Body a) | 
    Iden a String | 
    Bin a String (Exp a) (Exp a) | 
    Una a String (Exp a) | 
    Tri a (Exp a) (Exp a) (Exp a) | 
    IntLit a Integer | 
    DblLit a Double | 
    StrLit a String | 
    CharLit a Char |
    Brack a (Exp a) |
    Paren a (Exp a)
    deriving (Show, Eq, Ord, Functor)

data BlockStmt a =
    If a (Exp a) (BlockStmt a) | 
    Else a (BlockStmt a) | 
    While a (Exp a) (BlockStmt a) | 
    DoWhile a (BlockStmt a) (Exp a) |
    For a (BlockStmt a) (Exp a) (BlockStmt a) (BlockStmt a) |
    Ret a (Maybe (Exp a)) | 
    Cont a | 
    Brk a | 
    Let a (Arg a) (Exp a) | 
    Mat a (Exp a) [Case a] |
    Block a [BlockStmt a] |
    Assign (Exp a) 
    deriving (Show, Eq, Ord, Functor)

data Case a = 
    Case (Name a) [Name a] (BlockStmt a)
    deriving (Show, Eq, Ord, Functor)

data Body a = 
    Compound (BlockStmt a) |
    Inline (Exp a)
    deriving (Show, Eq, Ord, Functor)

data Memb a = 
    Memb (Name a) (Exp a)
    deriving (Show, Eq, Ord, Functor)

data FileStmt a =
    Imp a String |
    Inc a String |
    Sum a (Name a) [Name a] [Memb a] | 
    Rec a (Name a) [Name a] [Memb a] | 
    Jdg a (Name a) (Exp a) | 
    Map a (Name a) [Arg a] (Body a) | 
    Def a (Name a) (Acc a) (Acc a) (Exp a) (Maybe (Exp a)) 
    deriving (Show, Eq, Ord, Functor)
