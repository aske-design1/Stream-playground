module Program.CST where


data BinaryOp = 
    Plus | 
    Minus | 
    LogicalOr | 
    LessThan 
    
    deriving (Show, Eq)

data UnaryOp = 
    Negate | 
    LogicalNot deriving (Show, Eq) 

data Member = Power | Active | Name 
    deriving (Show, Eq)

data Value = 
    VNum Int | 
    VStr String |
    VTime |
    Member Member
    
    deriving (Show, Eq)

data Expr = 
    Val Value | 
    BinOp BinaryOp Expr Expr | 
    UnOp UnaryOp Expr |
    MTLExpr MTLElement MTLBound Expr
    
    deriving (Show, Eq)

data MTLElement = Always | Eventually deriving (Show, Eq)
data MTLBound = None | Range Int Int deriving (Show, Eq)

data Property = Prop MTLElement MTLBound Expr | Comp Property Property

type Program = Property 