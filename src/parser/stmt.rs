use super::expr::Expr;


pub(crate) enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
}
