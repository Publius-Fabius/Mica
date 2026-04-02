module Mica.Type where 

-- World's best systems programming language. 
-- The one true spiritual successor to C.

data Region = 
    Global |                            --- Global "malloced" memory
    Local |                             --- Thread local memory
    Fenced |                            --- Safe mprotected memory
    Stack                               --- Stack based memory
    deriving (Show, Eq)
    
type TVar = Either String Expr          --- Either Var or Type

data Expr =
    TyLam Region [Expr] Expr Expr |     --- Region, Captures, Signature
    TyPtr Region TVar |                 --- Pointer Kind
    TyArr Region Expr |                 --- Array Type
    Lam Region Expr Expr FunBlock |     --- loc, captures, args, body
    Iden String |                       --- Identifier
    Bin String Expr Expr |              --- Binary Op ie. (+-%^&)
    Una String Expr |                   --- Unary Op ie. (!*&)
    Tri Expr Expr Expr |                --- Trinary Op ie. (x?y:z)
    LitInt Integer |                    --- Literal Integer
    LitDbl Double |                     --- Literal Double
    LitStr String                       --- Literal String
    deriving (Show, Eq)

type CaseStmt = (String, Expr, BlockStmt)

data BlockStmt =
    IfThen Expr BlockStmt |             --- If Then 
    Else Expr |                         --- Else
    While Expr BlockStmt |              --- While 
    For BlockStmt Expr BlockStmt |      --- For 
    Return (Maybe Expr) |               --- Return
    Continue |                          --- Continue
    Break |                             --- Break
    Assign Expr Expr |                  --- Assign Register|Ptr
    Let String Expr |                   --- Immutable Value
    Match Expr [CaseStmt] |             --- Pattern Matching
    Register String Expr                --- Mutable Primitive
    deriving (Show, Eq)

type FunBlock = Either [BlockStmt] Expr

data Spec = 
    ThreadLocal |
    Static |
    Extern
    deriving (Show, Eq)

type Constr = (String, Expr)
type Memb = (String, Expr)

data FileStmt =
    Fun String Expr FunBlock |         --- Function Stmt
    Jdg String Expr |                   --- Type Judgement
    Dat String [Constr] |               --- Type Union
    Stru String [Memb] |                --- Classic 'C' Struct
    Decl Spec String Expr |             --- Declare file level symbol
    Def String Expr |                   --- Assign value to symbol
    Import String                       --- Import Header
    deriving (Show, Eq)

