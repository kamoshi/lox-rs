type RecExpr = Box<Expr>;

pub(crate) enum Expr {
    Literal(Literal),
    Unary(OpUnary, RecExpr),
    Binary(RecExpr, OpBinary, RecExpr),
    Grouping(RecExpr),
}

pub(crate) enum Literal {
    Num(f64),
    Str(String),
    True,
    False,
    Nil,
}

pub(crate) enum OpUnary {
    Bang, Minus
}

pub(crate) enum OpBinary {
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::Grouping(expr) => format!("({})", expr.to_string()),
            Expr::Unary(op, expr) => format!("(op {})", expr.to_string()),
            Expr::Binary(lexpr, op, rexpr) => format!("(op {} {})", lexpr.to_string(), rexpr.to_string()),
            Expr::Literal(literal) => match literal {
                Literal::Str(str)   => str.to_owned(),
                Literal::Num(num)   => num.to_string(),
                Literal::True       => "true".to_owned(),
                Literal::False      => "false".to_owned(),
                Literal::Nil        => "nil".to_owned(),
            },
        }
    }
}
