#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Var(Ident, Option<Box<Expr>>),
    Expression(Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    While(Box<Expr>, Box<Stmt>),
    Function(Ident, Vec<Ident>, Vec<Stmt>),
    Return(Option<Box<Expr>>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Assign(Ident, Box<Expr>),
    Unary(OpUnary, Box<Expr>),
    Binary(Box<Expr>, OpBinary, Box<Expr>),
    Grouping(Box<Expr>),
    Variable(Ident),
    Logic(Box<Expr>, OpLogic, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Lambda(Vec<Ident>, Vec<Stmt>),
    Array(Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum OpLogic {
    And, Or
}

#[derive(Debug, Clone)]
pub enum OpUnary {
    Not, Neg
}

#[derive(Debug, Clone)]
pub enum OpBinary {
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
