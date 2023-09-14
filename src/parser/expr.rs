use std::fmt::Display;


pub(crate) enum Expr {
    Literal(Literal),
    Unary(OpUnary, Box<Expr>),
    Binary(Box<Expr>, OpBinary, Box<Expr>),
    Grouping(Box<Expr>),
}

pub(crate) enum Literal {
    Num(f64),
    Str(String),
    True,
    False,
    Nil,
}

pub(crate) enum OpUnary {
    Not, Neg
}

pub(crate) enum OpBinary {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add,
    Sub,
    Mul,
    Div,
}


impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expr::*;
        let str = match self {
            Grouping(expr)      => format!("({expr})"),
            Unary(op, expr)     => format!("({op} {expr})"),
            Binary(l, op, r)    => format!("({op} {l} {r})"),
            Literal(literal)    => literal.to_string(),
        };

        write!(f, "{str}")
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Literal::*;
        let str: String = match self {
            Str(str)    => str.into(),
            Num(num)    => num.to_string(),
            True        => "true".into(),
            False       => "false".into(),
            Nil         => "nil".into(),
        };

        write!(f, "{str}")
    }
}

impl Display for OpUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpUnary::*;
        let str = match self {
            Not => "not",
            Neg => "neg",
        };

        write!(f, "{str}")
    }
}

impl Display for OpBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use OpBinary::*;
        let str = match self {
            Equal           => "==",
            NotEqual        => "!=",
            Less            => "<",
            LessEqual       => "<=",
            Greater         => ">",
            GreaterEqual    => ">=",
            Add             => "+",
            Sub             => "-",
            Mul             => "*",
            Div             => "/",
        };

        write!(f, "{str}")
    }
}
