
data Locality = 
    Global | 
    Local | 
    Safe |
    Stack 
    deriving (Show, Eq)
    
data Type = 
    TVar Ident |                    -- Int, u8, etc.
    TApp Type Type |                -- Ptr Local, etc.
    TArrow Type Type |              -- Domain -> Codomain
    TLam Locality [Type] Type |     -- The Closure Type
    deriving (Show, Eq)

