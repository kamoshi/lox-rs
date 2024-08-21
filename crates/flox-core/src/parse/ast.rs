#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Let(Ident, Box<Expr>),
    Assign(Ident, Box<Expr>),
    Unary(String, Box<Expr>),
    Binary(Box<Expr>, String, Box<Expr>),
    Grouping(Box<Expr>),
    Variable(Ident),
    Call(Box<Expr>, Box<Expr>),
    Lambda(Ident, Box<Expr>),
    Constructor(usize, usize, Box<[String]>),
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Block(Box<[Expr]>),
    While(Box<Expr>, Box<Expr>),
    Return(Option<Box<Expr>>),
    Data(Ident, Box<[(Ident, Box<[String]>)]>),
    Match(Box<Expr>, Box<[(Expr, Expr, Box<[String]>)]>),
    Index(Box<Expr>, Ident),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Num(f64),
    Str(String),
    True,
    False,
    Nil,
}

#[derive(Debug, Clone)]
pub struct Ident(pub String);
