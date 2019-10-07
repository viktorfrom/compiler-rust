pub mod expr_tree {

    #[derive(Debug, PartialEq, Eq)]
    pub enum Expr {
        Node(Box<Expr>, Box<Expr>, Box<Expr>),
        Num(i32),
        Bool(bool),
        LogicOp(LogicOp),
        ArithOp(ArithOp),
        AssignOp(AssignOp),
        RelOp(RelOp),
        Type(Type),
        Str(String),
        Tuple(Box<Expr>, Box<Expr>),
        If(Box<Expr>, Vec<Expr>),
        Func(Box<Expr>, Vec<Expr>, Vec<Expr>),
        While(Box<Expr>, Box<Expr>, Vec<Expr>),
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum ArithOp {
        Add,
        Sub,
        Mult,
        Div,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum LogicOp {
        And,
        Or,
        Not,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum AssignOp {
        Equ,
        PluEqu,
        MinEqu,
        DivEqu,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum RelOp {
        EquEqu,
        NotEqu,
        LesEqu,
        GreEqu,
        Les,
        Gre,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub enum Type {
        Integer,
        Bool,
        Str,
    }
}
