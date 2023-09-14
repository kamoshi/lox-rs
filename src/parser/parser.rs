use crate::lexer::{token::Token, token_type::TokenType};
use super::expr::{Expr, OpBinary, Literal, OpUnary};
use super::error::{Error, ErrorType};
use super::stmt::Stmt;


// program      → statement* EOF ;
//
// statement    → stmtExpr
//              | stmtPrnt ;
//
// stmtExpr     → expression ";" ;
// stmtPrnt     → "print" expression ";" ;
//
// expression   → equality ;
// equality     → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison   → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term         → factor ( ( "-" | "+" ) factor )* ;
// factor       → unary ( ( "/" | "*" ) unary )* ;
// unary        → ( "!" | "-" ) unary
//              | primary ;
// primary      → NUMBER | STRING | "true" | "false" | "nil"
//              | "(" expression ")" ;

pub(crate) fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, Error> {
    let mut statements = vec![];
    let mut curr = 0;

    while curr < tokens.len() {
        if tokens.get(curr).map(|t| t.ttype == TokenType::Eof).unwrap_or(false) { break };
        let (consumed, stmt) = statement(&tokens[curr..])?;

        statements.push(stmt);
        curr += consumed;
    }

    return Ok(statements);
}

pub(crate) fn parse_expr(tokens: &[Token]) -> Result<Expr, Error> {
    let (consumed, expr) = expression(tokens)?;
    match tokens.get(consumed).map(|t| &t.ttype) {
        Some(TokenType::Eof) => Ok(*expr),
        None => Ok(*expr),
        _ => Err(Error { ttype: ErrorType::ExprLeftover })
    }
}


fn statement(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    match tokens.get(0).map(|t| &t.ttype) {
        Some(TokenType::Print) => stmt_prnt(&tokens[1..]),
        _ => stmt_expr(tokens),
    }
}

fn stmt_expr(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    let (consumed, expr) = expression(tokens)?;

    match tokens.get(consumed).map(|t| &t.ttype) {
        Some(TokenType::Semicolon) => Ok((consumed + 1, Stmt::Expression(expr))),
        _ => Err(Error { ttype: ErrorType::MissingSemicolon }),
    }
}

fn stmt_prnt(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    let (consumed, expr) = expression(tokens)?;

    match tokens.get(consumed).map(|t| &t.ttype) {
        Some(TokenType::Semicolon) => Ok((consumed + 2, Stmt::Print(expr))),
        _ => Err(Error { ttype: ErrorType::MissingSemicolon }),
    }
}

fn expression(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    equality(tokens)
}

fn equality(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let (mut consumed, mut expr) = comparison(tokens)?;

    while matches(tokens.get(consumed), &[TokenType::BangEqual, TokenType::EqualEqual]) {
        let op = match tokens[consumed].ttype {
            TokenType::BangEqual    => OpBinary::NotEqual,
            TokenType::EqualEqual   => OpBinary::Equal,
            _ => unreachable!(),
        };

        let (next, next_expr) = comparison(&tokens[1+consumed..])?;
        consumed += 1 + next;
        expr = Box::new(Expr::Binary(expr, op, next_expr))
    }

    Ok((consumed, expr))
}


fn comparison(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let (mut consumed, mut expr) = term(tokens)?;

    while matches(tokens.get(consumed), &[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
        let op = match tokens[consumed].ttype {
            TokenType::Greater      => OpBinary::Greater,
            TokenType::GreaterEqual => OpBinary::GreaterEqual,
            TokenType::Less         => OpBinary::Less,
            TokenType::LessEqual    => OpBinary::LessEqual,
            _ => unreachable!(),
        };

        let (next, next_expr) = term(&tokens[1+consumed..])?;
        consumed += 1 + next;
        expr = Box::new(Expr::Binary(expr, op, next_expr))
    }

    Ok((consumed, expr))
}

fn term(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let (mut consumed, mut expr) = factor(tokens)?;

    while matches(tokens.get(consumed), &[TokenType::Minus, TokenType::Plus]) {
        let op = match tokens[consumed].ttype {
            TokenType::Minus    => OpBinary::Sub,
            TokenType::Plus     => OpBinary::Add,
            _ => unreachable!(),
        };

        let (next, next_expr) = factor(&tokens[1+consumed..])?;
        consumed += 1 + next;
        expr = Box::new(Expr::Binary(expr, op, next_expr))
    }

    Ok((consumed, expr))
}

fn factor(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let (mut consumed, mut expr) = unary(tokens)?;

    while matches(tokens.get(consumed), &[TokenType::Slash, TokenType::Star]) {
        let op = match tokens[consumed].ttype {
            TokenType::Slash    => OpBinary::Div,
            TokenType::Star     => OpBinary::Mul,
            _ => unreachable!(),
        };

        let (next, next_expr) = unary(&tokens[1+consumed..])?;
        consumed += 1 + next;
        expr = Box::new(Expr::Binary(expr, op, next_expr))
    }

    Ok((consumed, expr))
}

fn unary(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let next = &tokens[0];
    match next.ttype {
        TokenType::Bang | TokenType::Minus => {
            let op = match next.ttype {
                TokenType::Bang     => OpUnary::Not,
                TokenType::Minus    => OpUnary::Neg,
                _ => unreachable!()
            };
            let (consumed, expr) = unary(&tokens[1..])?;
            let expr = Box::new(Expr::Unary(op, expr));
            Ok((1 + consumed, expr))
        },
        _ => Ok(primary(tokens)?),
    }
}

fn primary(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let next = &tokens[0];

    use Literal::*;
    let (consumed, variant) = match &next.ttype {
        TokenType::Num(num) => (1, Box::new(Expr::Literal(Num(*num)))),
        TokenType::Str(str) => (1, Box::new(Expr::Literal(Str(str.to_owned())))),
        TokenType::True     => (1, Box::new(Expr::Literal(True))),
        TokenType::False    => (1, Box::new(Expr::Literal(False))),
        TokenType::Nil      => (1, Box::new(Expr::Literal(Nil))),
        TokenType::ParenL   => {
            let (consumed, expr) = expression(&tokens[1..])?;

            let expr = match tokens.get(1 + consumed).map(|t| &t.ttype) {
                Some(TokenType::ParenR) => Box::new(Expr::Grouping(expr)),
                _ => return Err(Error { ttype: ErrorType::MissingRightParen }),
            };

            (2 + consumed, expr)
        },
        tt => return Err(Error { ttype: ErrorType::InvalidToken(tt.to_owned()) }),
    };

    Ok((consumed, variant))
}

fn matches(token: Option<&Token>, tts: &[TokenType]) -> bool {
    token
        .map(|token| tts.iter().any(|tt| *tt == token.ttype))
        .unwrap_or(false)
}
