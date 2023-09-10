type RecExpr = Box<Expr>;

pub(crate) enum Expr {
    Grouping(RecExpr),
    Unary(TokenUnary, RecExpr),
    Binary(TokenBinary, RecExpr, RecExpr),
    Literal(Literal),
}

pub(crate) enum TokenUnary {
    Bang, Minus
}

pub(crate) enum TokenBinary {
    Minus, Plus
}

pub(crate) enum Literal {
    Str(String),
    Num(f64),
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::Grouping(expr) => format!("({})", expr.to_string()),
            Expr::Unary(op, expr) => format!("(op {})", expr.to_string()),
            Expr::Binary(op, lexpr, rexpr) => format!("(op {} {})", lexpr.to_string(), rexpr.to_string()),
            Expr::Literal(literal) => match literal {
                Literal::Str(str) => str.to_owned(),
                Literal::Num(num) => num.to_string(),
            },
        }
    }
}
