{-# LANGUAGE DeriveFunctor #-}

module Mica.Type where 

-- World's best systems programming language. 
-- The one true spiritual successor to C.

data Exp a =
    TVar a String | 
    TLam a (Exp a) (Exp a) (Exp a) | 
    TPtr a (Exp a) (Exp a) (Exp a) | 
    TArr a (Exp a) (Exp a) (Exp a) |    
    Lam a (Exp a) (Exp a) (Exp a) (ExpStmt a) | 
    Iden a String |        
    Bin a String (Exp a) (Exp a) |
    Una a String (Exp a) | 
    Tri a (Exp a) (Exp a) (Exp a) | 
    IntLit a Integer | 
    DblLit a Double | 
    StrLit a String |
    Brack (Exp a) |
    Curly (Exp a)
    deriving (Show, Eq, Ord, Functor)

data BlockStmt a =
    If a (Exp a) (ExpStmt a) | 
    Else a (ExpStmt a) | 
    While a (Exp a) (ExpStmt a) | 
    DoWhile a (ExpStmt a) (Exp a) |
    For a (Exp a) (ExpStmt a) |
    Ret a (Maybe (Exp a)) | 
    Cont a | 
    Brk a | 
    Let a String (Exp a) | 
    Mat a (Exp a) [(String, Exp a, ExpStmt a)] |
    Set (Exp a) |
    BPre a String
    deriving (Show, Eq, Ord, Functor)

data ExpStmt a = 
    ExpStmtBlock [BlockStmt a] |
    ExpStmtExp (Exp a)
    deriving (Show, Eq, Ord, Functor)

data FileStmt a =
    Fun a String (Exp a) (ExpStmt a) | 
    Jdg a String (Exp a) | 
    Dat a String (Exp a) [(String, Exp a)] | 
    Stru a String (Exp a) [(String, Exp a)] | 
    Dec a (Maybe (Exp a)) String (Exp a) | 
    Def a String (Exp a) | 
    Imp a String | 
    FPre a String
    deriving (Show, Eq, Ord, Functor)
