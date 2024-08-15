// #[derive(Debug, Clone)]
// pub enum Stmt {
//     Var(Ident, Option<Box<Expr>>),
//     Expression(Box<Expr>),
//     While(Box<Expr>, Box<Stmt>),
//     // Function(Ident, Vec<Ident>, Vec<Stmt>),
//     Return(Option<Box<Expr>>),
// }

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
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Block(Box<[Expr]>),
    While(Box<Expr>, Box<Expr>),
    Return(Option<Box<Expr>>),
    Variant(Ident, Vec<Ident>),
    Match(Box<Expr>, Box<[(Ident, Expr)]>)
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
