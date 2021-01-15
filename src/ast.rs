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

// pub mod expr_tree {

//     #[derive(Debug, PartialEq, Eq, Clone, Hash)]
//     pub enum Expr {
//         Let(Box<Expr>, Box<Expr>, Box<Expr>),
//         Param(Box<Expr>, Box<Expr>),
//         If(Box<Expr>, Vec<Expr>),
//         Func(Box<Expr>, Vec<Expr>, Vec<Expr>),
//         FuncInput(Box<Expr>, Box<Expr>, Vec<Expr>),
//         While(Box<Expr>, Box<Expr>, Vec<Expr>),
//         Return(String, Box<Expr>),
//         Num(i32),
//         Bool(bool),
//         LogicOp(LogicOp),
//         ArithOp(ArithOp),
//         AssignOp(AssignOp),
//         RelOp(RelOp),
//         Type(Type),
//         Str(String),
//     }

//     #[derive(Debug, PartialEq, Eq, Clone, Hash)]
//     pub enum ArithOp {
//         Add,
//         Sub,
//         Mult,
//         Div,
//     }

//     #[derive(Debug, PartialEq, Eq, Clone, Hash)]
//     pub enum LogicOp {
//         And,
//         Or,
//         Not,
//     }

//     #[derive(Debug, PartialEq, Eq, Clone, Hash)]
//     pub enum AssignOp {
//         Equ,
//         PluEqu,
//         SubEqu,
//         DivEqu,
//     }

//     #[derive(Debug, PartialEq, Eq, Clone, Hash)]
//     pub enum RelOp {
//         EquEqu,
//         NotEqu,
//         LesEqu,
//         GreEqu,
//         Les,
//         Gre,
//     }

//     #[derive(Debug, PartialEq, Eq, Clone, Hash)]
//     pub enum Type {
//         Integer,
//         Bool,
//         Str,
//         Void,
//     }
// }

// pub mod content_tree {

//     #[derive(Debug, PartialEq, Eq, Clone, Hash)]
//     pub enum Content {
//         Num(i32),
//         ContentOp(ContentOp),
//         Bool(bool),
//         Str(String),
//         Return(String, Box<Content>),
//         Null,
//     }

//     #[derive(Debug, PartialEq, Eq, Clone, Hash)]
//     pub enum ContentOp {
//         Add,
//         Sub,
//         Mult,
//         Div,

//         And,
//         Or,
//         Not,

//         Equ,
//         PluEqu,
//         SubEqu,
//         DivEqu,

//         EquEqu,
//         NotEqu,
//         LesEqu,
//         GreEqu,
//         Les,
//         Gre,

//         Integer,
//         Bool,
//         Str,
//         Void,
//     }
// }
