{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE OverloadedStrings #-}

module Mica.Type where 

import Data.Set
import Data.Text
import qualified Data.List as L
import Mica.Cute

data Arg a = 
    Arg (Name a) (Maybe (Exp a)) 
    deriving (Show, Eq, Ord, Functor, Foldable) 

data Name a = 
    Name a Text 
    deriving (Show, Eq, Ord, Functor, Foldable)

data Exp a =
    Var a Text |
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
    Compound a [BlockStmt a] |
    Inline (Exp a)
    deriving (Show, Eq, Ord, Functor, Foldable)

data Memb a = Memb (Name a) (Exp a)
    deriving (Show, Eq, Ord, Functor)

data Inj a = Inj (Name a) (Exp a)
    deriving (Show, Eq, Ord, Functor)

data Spe = 
    Static | 
    ThreadLocal | 
    Mutable  | 
    NoReturn 
    deriving (Show, Eq, Ord)

data Layout a = 
    Scalar (Name a) (Exp a) |
    Group a (Name a) [Layout a] |
    Array a (Name a) Int [Layout a] 
    deriving (Show, Eq, Ord, Functor)

data FileStmt a =
    Imp a Text | 
    Inc a Text | 
    Rec a (Name a) [Name a] (Maybe [Memb a]) | 
    Sum a (Name a) [Name a] (Maybe [Inj a]) |
    Dec [Spe] (Name a) (Exp a) |
    Fun [Spe] (Name a) [Arg a] (Body a) | 
    Trm [Spe] (Name a) (Exp a) |
    Ext a (Name a) (Exp a) Text |
    Lay a (Name a) (Layout a)
    deriving (Show, Eq, Ord, Functor)

data Meta = 
    IsRec Int Bool | 
    IsSum Int Bool | 
    IsDec [Spe] | 
    IsFun [Spe] |
    IsTrm [Spe] 
    deriving (Show, Eq, Ord)

reduceSpecifiers :: [Spe] -> [Spe] 
reduceSpecifiers = L.map L.head . L.group . L.sort 

unname :: Name a -> Text 
unname (Name _ n) = n

class ExpTransform m where 
    mapx :: (Exp a -> Exp a) -> m a -> m a
    foldx :: (Exp a -> x -> x) -> m a -> x -> x 

instance ExpTransform BlockStmt where 
    mapx f (If p ex stmts) = If p (mapx f ex) (mapx f <$> stmts)
    mapx f (Else p stmts) = Else p (mapx f <$> stmts) 
    mapx f (While p ex stmts) = 
        While p (mapx f ex) (mapx f <$> stmts)
    mapx f (DoWhile p stmts ex) = 
        DoWhile p (mapx f <$> stmts) (mapx f ex) 
    mapx f (For p ex1 ex2 ex3 stmts) = 
        For p (mapx f ex1) (mapx f ex2) (mapx f ex3)
        (mapx f <$> stmts) 
    mapx f (Let p arg ex) = Let p (mapx f arg) (mapx f ex)
    mapx f (Ret p mex) = Ret p (mapx f <$> mex) 
    mapx f (Mat p ex cases) = Mat p (mapx f ex) (mapx f <$> cases) 
    mapx f (ExpStmt ex) = ExpStmt (mapx f ex)
    mapx _ (Cont a) = Cont a
    mapx _ (Brk a) = Brk a

    foldx f (If p ex stmts) x = L.foldr (foldx f) (foldx f ex x) stmts 
    foldx f (Else p stmts) x = L.foldr (foldx f) x stmts 
    foldx f (While p ex stmts) x = L.foldr (foldx f) (foldx f ex x) stmts 
    foldx f (DoWhile p stmts ex) x = L.foldr (foldx f) (foldx f ex x) stmts 
    foldx f (For p ex1 ex2 ex3 stmts) x =
        let x1 = foldx f ex1 x 
            x2 = foldx f ex2 x1 
            x3 = foldx f ex3 x2 
        in L.foldr (foldx f) x3 stmts 
    foldx f (Let p arg ex) x = foldx f ex $ foldx f arg x
    foldx f (Ret p (Just ex)) x = foldx f ex x 
    foldx f (Mat p ex cases)  x = L.foldr (foldx f) (foldx f ex x) cases
    foldx f (ExpStmt ex) x = foldx f ex x 
    foldx _ (Cont _) x = x
    foldx _ (Brk _) x = x

instance ExpTransform Case where 
    mapx f (Case n ns stmts) = Case n ns (mapx f <$> stmts) 
    foldx f (Case _ _ stmts) x = L.foldr (foldx f) x stmts 

instance ExpTransform Arg where 
    mapx f (Arg n mt) = Arg n $ mapx f <$> mt
    foldx f (Arg _ (Just ex)) x = foldx f ex x
    foldx _ _ x = x

instance ExpTransform Body where 
    mapx f (Inline ex) = Inline (mapx f ex)
    mapx f (Compound p stmts) = Compound p (mapx f <$> stmts) 
    foldx f (Inline ex) x = foldx f ex x
    foldx f (Compound _ stmts) x = L.foldr (foldx f) x stmts

instance ExpTransform Exp where 
    mapx f (Bin a op lx rx) = Bin a op (mapx f lx) (mapx f rx)
    mapx f (Una a op ex) = Una a op $ mapx f ex
    mapx f (Brack a ex) = Brack a $ mapx f ex
    mapx f (Lam a as b)  = Lam a (mapx f <$> as) (mapx f b) 
    mapx f leaf = f leaf 

    foldx f (Bin _ _ lx rx) x = foldx f rx (foldx f lx x)
    foldx f (Una _ _ ex) x = foldx f ex x
    foldx f (Brack _ ex) x = foldx f ex x 
    foldx f (Lam _ as b) x = foldx f b $ L.foldr (foldx f) x as
    foldx f leaf x = f leaf x

class Typed a where 
    typeof :: a -> Exp () 

instance Typed a => Typed (Exp a) where 
    typeof TStar = TStar
    typeof TUnit = TStar
    typeof (Var _ _) = TStar 
    typeof (Iden n _) = typeof n
    typeof (Lam n _ _) = typeof n
    typeof (Bin n _ _ _) = typeof n
    typeof (Una n _ _) = typeof n
    typeof (IntLit n _) = typeof n
    typeof (DblLit n _) = typeof n 
    typeof (StrLit n _) = typeof n 
    typeof (CharLit n _) = typeof n 
    typeof (Brack n _) = typeof n 
    typeof (Stub n) = typeof n
    typeof TVoid = TBot

instance Typed a => Typed (Body a) where 
    typeof (Compound n _) = typeof n 
    typeof (Inline ex) = typeof ex

instance Typed a => Typed (Name a) where 
    typeof (Name n _) = typeof n

instance Typed a => Typed (Arg a) where 
    typeof (Arg n _) = typeof n 

instance Cute (Name a) where 
    cute (Name _ a) = a

instance Cute (Arg a) where 
    cute (Arg n (Just ty)) = "(" <> cute n <> ":" <> cute ty <> ")" 
    cute (Arg n Nothing) = cute n 

instance Cute (Exp a) where 
    cute (Var _ v) = v
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
    cute TBot = "_|_"

instance Cute (Body a) where 
    cute (Compound _ stmts) = 
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

instance Cute (Inj a) where 
    cute (Inj n ex) = cute n <> " : " <> cute ex <> ";"

instance Cute (FileStmt a) where 
    cute (Imp _ t) = "import " <> t <> ";" 
    cute (Inc _ t) = "include " <> t <> ";" 
    cute (Sum _ n ki mems) = 
        let mems' = maybe "" (Data.Text.unwords . fmap cute) mems
            ki' = Data.Text.unwords $ cute <$> ki
        in "typesum " <> cute n <> " " <> ki' <> " {" <> mems' <> "}"
    cute (Rec _ n ps mems) = 
        let mems' = maybe "" (Data.Text.unwords . fmap cute) mems
            ps' = Data.Text.unwords $ cute <$> ps
        in "record " <> cute n <> " " <> ps' <> " {" <> mems' <> "}"
    cute (Fun _ n as b) = 
        let as' = Data.Text.unwords (cute <$> as) 
        in "routine " <> cute n <> " " <> as' <> " = " <> cute b 
    cute (Dec _ n ex) = "declare " <> cute n <> " : " <> cute ex <> ";"

