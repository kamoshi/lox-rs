#[derive(Debug)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Var(Ident, Option<Box<Expr>>),
    Print(Box<Expr>),
    Expression(Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Assign(Ident, Box<Expr>),
    Unary(OpUnary, Box<Expr>),
    Binary(Box<Expr>, OpBinary, Box<Expr>),
    Grouping(Box<Expr>),
    Variable(Ident),
    Logic(Box<Expr>, OpLogic, Box<Expr>),
}

#[derive(Debug)]
pub enum OpLogic {
    And, Or
}

#[derive(Debug)]
pub enum OpUnary {
    Not, Neg
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Literal {
    Num(f64),
    Str(String),
    True,
    False,
    Nil,
}

#[derive(Debug)]
pub struct Ident(pub String);
