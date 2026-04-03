module Mica.Type where 

import Text.Megaparsec 

type SP = SourcePos

-- World's best systems programming language. 
-- The one true spiritual successor to C.

data Expr =
    TVar SP String |                    --- Type Variable
    TLam SP Expr Expr Expr |            --- TLambda: Region, Access, Type
    TPtr SP Expr Expr Expr |            --- TPointer: Region, Access, Type
    TArr SP Expr Expr |                 --- TArray: Region, Access, Type
    Lam SP Expr Expr Expr ExprStmt |    --- Lambda: Region, Access, Args, Body
    Iden SP String |                    --- Identifier
    Bin SP String Expr Expr |           --- Binary Op ie. (+-%^&)
    Una SP String Expr |                --- Unary Op ie. (!*&)
    Tri SP Expr Expr Expr |             --- Trinary Op ie. (x?y:z)
    IntLit SP Integer |                 --- Literal Integer
    DblLit SP Double |                  --- Literal Double
    StrLit SP String                    --- Literal String
    deriving (Show, Eq)

type CaseStmt = (String, Expr, ExprStmt)

data BlockStmt =
    If SP Expr ExprStmt |               --- If Then 
    Else SP ExprStmt |                  --- Else
    While SP Expr ExprStmt |            --- While 
    For SP BlockStmt Expr BlockStmt |   --- For 
    Ret SP (Maybe Expr) |               --- Return
    Cont SP |                           --- Continue
    Bre SP |                            --- Break
    Ass SP Expr Expr |                  --- Assign Register|Ptr
    Let SP String Expr |                --- Immutable Value
    Mat SP Expr [CaseStmt] |            --- Pattern Matching
    Reg SP String Expr                  --- Mutable Primitive
    deriving (Show, Eq)

type ExprStmt = Either [BlockStmt] Expr

data FileStmt =
    Fun SP String Expr ExprStmt |        --- Function Stmt
    Jdg SP String Expr |                --- Type Judgement
    Dat SP String [(String, Expr)] |    --- Tagged Type Union
    Stru SP String [(String, Expr)] |   --- Classic 'C' Struct
    Dec SP Expr String Expr |           --- Declare: Specifiers, Name, Type
    Def SP String Expr |                --- Define: Name, Value
    Imp SP String                       --- Import Header
    deriving (Show, Eq)
