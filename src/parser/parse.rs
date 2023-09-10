use crate::lexer::{token::Token, token_type::TokenType};
use super::expr::{Expr, OpBinary, Literal};


// expression  ::= equality ;
// equality    ::= comparison ( ( "!=" | "==" ) comparison )* ;
// comparison  ::= term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term        ::= factor ( ( "-" | "+" ) factor )* ;
// factor      ::= unary ( ( "/" | "*" ) unary )* ;
// unary       ::= ( "!" | "-" ) unary
//                 | primary ;
// primary     ::= NUMBER | STRING | "true" | "false" | "nil"
//                 | "(" expression ")" ;

fn parse(
    tokens: &[Token]
) {
    let (consumed, expr) = expression(tokens);
}

fn expression(
    tokens: &[Token]
) -> (usize, Box<Expr>) {
    equality(tokens)
}

fn equality(
    tokens: &[Token]
) -> (usize, Box<Expr>) {
    let (mut consumed, mut expr) = comparison(tokens);

    loop {
        let next = tokens.get(consumed);
    }

    (consumed, expr)
}


fn comparison(
    tokens: &[Token]
) -> (usize, Box<Expr>) {
    todo!()
}

fn term(
    tokens: &[Token]
) {
    todo!()
}

fn factor(
    tokens: &[Token]
) {
    todo!()
}

fn unary(
    tokens: &[Token]
) {
    todo!()
}

fn primary(
    tokens: &[Token]
) -> Box<Expr> {
    let next = &tokens[0];

    use Literal::*;
    let variant = match &next.r#type {
        TokenType::Num(num) => Num(*num),
        TokenType::Str(str) => Str(str.to_owned()),
        TokenType::True => True,
        TokenType::False => False,
        TokenType::Nil => Nil,
        TokenType::ParenL => todo!(),
        _ => unimplemented!(),
    };

    Box::new(Expr::Literal(variant))
}
