#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(i32),
    Var(String),
    Bool(bool),

    BinOp(Box<Expr>, Op, Box<Expr>),
    VarOp(Box<Expr>, Op, Box<Expr>),

    Let(Box<Expr>, Type, Box<Expr>),

    If(Box<Expr>, Block),
    IfElse(Box<Expr>, Block),
    While(Box<Expr>, Block),

    // FnCall(FnCall),
    // FnCall(FnHead, FnHead),
    Return(Box<Expr>),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Type {
    Int,
    Bool,
    Str,
    // Void,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    LogOp(LogOp),
    AriOp(AriOp),
    AssOp(AssOp),
    RelOp(RelOp),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FnHead {
    Name(String),
    Params(Vec<Box<Expr>>),
    Return(Type),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum AriOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum AssOp {
    Equ,
    PluEqu,
    SubEqu,
    DivEqu,
    MulEqu,
}
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LogOp {
    And,
    Or,
    // Not,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum RelOp {
    Eq,
    Neq,
    Leq,
    Geq,
    Les,
    Gre,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub content: Vec<Expr>,
}

impl Block {
    pub fn new(exprs: Vec<Expr>) -> Block {
        Block { content: exprs }
    }
}
