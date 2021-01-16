#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(i32),
    Var(String),
    Bool(bool),

    BinExpr(Box<Expr>, Op, Box<Expr>),
    VarExpr(Box<Expr>, Op, Box<Expr>),

    Let(Box<Expr>, Type, Box<Expr>),

    If(Box<Expr>, Vec<Expr>),
    IfElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    While(Box<Expr>, Vec<Expr>),

    FnCall(Box<Expr>, Vec<Expr>),
    Return(Box<Expr>),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Type {
    I32,
    Bool,
    Str,
    Void,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    LogOp(LogOp),
    AriOp(AriOp),
    AssOp(AssOp),
    RelOp(RelOp),
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
