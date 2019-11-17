pub mod expr_tree {

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Expr {
        Let(Box<Expr>, Box<Expr>, Box<Expr>),
        Param(Box<Expr>, Box<Expr>),
        If(Box<Expr>, Vec<Expr>),
        Func(Box<Expr>, Vec<Expr>, Vec<Expr>),
        FuncInput(Box<Expr>, Box<Expr>, Vec<Expr>),
        While(Box<Expr>, Box<Expr>, Vec<Expr>),
        Return(String, Box<Expr>),
        Num(i32),
        Bool(bool),
        LogicOp(LogicOp),
        ArithOp(ArithOp),
        AssignOp(AssignOp),
        RelOp(RelOp),
        Type(Type),
        Str(String),
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum ArithOp {
        Add,
        Sub,
        Mult,
        Div,
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum LogicOp {
        And,
        Or,
        Not,
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum AssignOp {
        Equ,
        PluEqu,
        SubEqu,
        DivEqu,
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum RelOp {
        EquEqu,
        NotEqu,
        LesEqu,
        GreEqu,
        Les,
        Gre,
    }

    #[derive(Debug, PartialEq, Eq, Clone)]
    pub enum Type {
        Integer,
        Bool,
        Str,
    }
}

pub mod content_tree {
    // use std::fmt;

    #[derive(Debug, PartialEq, Eq, Clone, Hash)]
    pub enum Content {
        Num(i32),
        ContentOp(ContentOp),
        Bool(bool),
        Str(String),
        // Tuple(String, Box<Content>),
        Return(String, Box<Content>),
        Null,
    }

    #[derive(Debug, PartialEq, Eq, Clone, Hash)]
    pub enum ContentOp {
        Add,
        Sub,
        Mult,
        Div,

        And,
        Or,
        Not,

        Equ,
        PluEqu,
        SubEqu,
        DivEqu,

        EquEqu,
        NotEqu,
        LesEqu,
        GreEqu,
        Les,
        Gre,

        Integer,
        Bool,
        Str,
    }

    // impl fmt::Display for Content {
    //     fn fmt(&self, f: &mut fmt::Formatter)  -> fmt::Result {
    //         match self {
    //             Content::Str(value) => write!(f, "{}", value),

    //         }
    //     }
    // }
}
