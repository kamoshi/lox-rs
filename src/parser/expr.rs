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

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::Grouping(expr) => format!("({})", expr.to_string()),
            Expr::Unary(op, expr) => {
                let op = match op {
                    OpUnary::Not => "not".to_owned(),
                    OpUnary::Neg => "neg".to_owned(),
                };
                let expr = expr.to_string();
                format!("({op} {expr})")
            },
            Expr::Binary(l, op, r) => {
                let op = match op {
                    OpBinary::Equal         => "==".to_owned(),
                    OpBinary::NotEqual      => "!=".to_owned(),
                    OpBinary::Less          => "<".to_owned(),
                    OpBinary::LessEqual     => "<=".to_owned(),
                    OpBinary::Greater       => ">".to_owned(),
                    OpBinary::GreaterEqual  => ">=".to_owned(),
                    OpBinary::Add           => "+".to_owned(),
                    OpBinary::Sub           => "-".to_owned(),
                    OpBinary::Mul           => "*".to_owned(),
                    OpBinary::Div           => "/".to_owned(),
                };
                let (l, r) = (l.to_string(), r.to_string());
                format!("({op} {l} {r})")
            },
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
