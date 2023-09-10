use crate::lexer::{token::Token, token_type::TokenType};
use super::expr::{Expr, OpBinary, Literal, OpUnary};


// expression  ::= equality ;
// equality    ::= comparison ( ( "!=" | "==" ) comparison )* ;
// comparison  ::= term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term        ::= factor ( ( "-" | "+" ) factor )* ;
// factor      ::= unary ( ( "/" | "*" ) unary )* ;
// unary       ::= ( "!" | "-" ) unary
//                 | primary ;
// primary     ::= NUMBER | STRING | "true" | "false" | "nil"
//                 | "(" expression ")" ;

pub(crate) fn parse(
    tokens: &[Token]
) -> Box<Expr> {
    let (_, expr) = expression(tokens);
    expr
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

    while matches(tokens.get(consumed), &[TokenType::BangEqual, TokenType::EqualEqual]) {
        let op = match tokens[consumed].r#type {
            TokenType::BangEqual    => OpBinary::NotEqual,
            TokenType::EqualEqual   => OpBinary::Equal,
            _ => unreachable!(),
        };

        let (next, next_expr) = comparison(&tokens[1+consumed..]);
        consumed += 1 + next;
        expr = Box::new(Expr::Binary(expr, op, next_expr))
    }

    (consumed, expr)
}


fn comparison(
    tokens: &[Token]
) -> (usize, Box<Expr>) {
    let (mut consumed, mut expr) = term(tokens);

    while matches(tokens.get(consumed), &[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
        let op = match tokens[consumed].r#type {
            TokenType::Greater      => OpBinary::Greater,
            TokenType::GreaterEqual => OpBinary::GreaterEqual,
            TokenType::Less         => OpBinary::Less,
            TokenType::LessEqual    => OpBinary::LessEqual,
            TokenType::Minus        => OpBinary::Sub,
            TokenType::Plus         => OpBinary::Add,
            _ => unreachable!(),
        };

        let (next, next_expr) = term(&tokens[1+consumed..]);
        consumed += 1 + next;
        expr = Box::new(Expr::Binary(expr, op, next_expr))
    }

    (consumed, expr)
}

fn term(
    tokens: &[Token]
) -> (usize, Box<Expr>) {
    let (mut consumed, mut expr) = factor(tokens);

    while matches(tokens.get(consumed), &[TokenType::Minus, TokenType::Plus]) {
        let op = match tokens[consumed].r#type {
            TokenType::Minus    => OpBinary::Sub,
            TokenType::Plus     => OpBinary::Add,
            _ => unreachable!(),
        };

        let (next, next_expr) = factor(&tokens[1+consumed..]);
        consumed += 1 + next;
        expr = Box::new(Expr::Binary(expr, op, next_expr))
    }

    (consumed, expr)
}

fn factor(
    tokens: &[Token]
) -> (usize, Box<Expr>) {
    let (mut consumed, mut expr) = unary(tokens);

    while matches(tokens.get(consumed), &[TokenType::Slash, TokenType::Star]) {
        let op = match tokens[consumed].r#type {
            TokenType::Slash    => OpBinary::Div,
            TokenType::Star     => OpBinary::Mul,
            _ => unreachable!(),
        };

        let (next, next_expr) = unary(&tokens[1+consumed..]);
        consumed += 1 + next;
        expr = Box::new(Expr::Binary(expr, op, next_expr))
    }

    (consumed, expr)
}

fn unary(
    tokens: &[Token]
) -> (usize, Box<Expr>) {
    let next = &tokens[0];
    match next.r#type {
        TokenType::Bang | TokenType::Minus => {
            let op = match next.r#type {
                TokenType::Bang     => OpUnary::Not,
                TokenType::Minus    => OpUnary::Neg,
                _ => unreachable!()
            };
            let (consumed, expr) = unary(&tokens[1..]);
            let expr = Box::new(Expr::Unary(op, expr));
            (1 + consumed, expr)
        },
        _ => primary(tokens),
    }
}

fn primary(
    tokens: &[Token]
) -> (usize, Box<Expr>) {
    let next = &tokens[0];

    use Literal::*;
    let (consumed, variant) = match &next.r#type {
        TokenType::Num(num) => (1, Box::new(Expr::Literal(Num(*num)))),
        TokenType::Str(str) => (1, Box::new(Expr::Literal(Str(str.to_owned())))),
        TokenType::True     => (1, Box::new(Expr::Literal(True))),
        TokenType::False    => (1, Box::new(Expr::Literal(False))),
        TokenType::Nil      => (1, Box::new(Expr::Literal(Nil))),
        TokenType::ParenL   => {
            let (consumed, expr) = expression(&tokens[1..]);
            // TODO: check for right paren
            let expr = Box::new(Expr::Grouping(expr));
            (2 + consumed, expr)
        },
        _ => unimplemented!(),
    };

    (consumed, variant)
}

fn matches(
    token: Option<&Token>,
    tts: &[TokenType],
) -> bool {
    token
        .map(|token| tts.iter().any(|tt| *tt == token.r#type))
        .unwrap_or(false)
}
