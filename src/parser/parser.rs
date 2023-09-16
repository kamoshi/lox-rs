use crate::lexer::{token::Token, token_type::TokenType};
use super::ast::{Stmt, Expr, OpBinary, Literal, OpUnary, Ident};
use super::error::{Error, ErrorType};


// program      → statement* EOF ;
//
// declaration  → declVar
//              | statement ;
//
// declVar      → "var" IDENTIFIER ( "=" expression )? ";" ;
//
// statement    → stmtExpr
//              | stmtPrnt ;
//
// stmtExpr     → expression ";" ;
// stmtPrnt     → "print" expression ";" ;
//
// expression   → assignment ;
// assignment   → IDENTIFIER "=" assignment
//              | equality ;
// equality     → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison   → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term         → factor ( ( "-" | "+" ) factor )* ;
// factor       → unary ( ( "/" | "*" ) unary )* ;
// unary        → ( "!" | "-" ) unary
//              | primary ;
// primary      → NUMBER | STRING | "true" | "false" | "nil"
//              | "(" expression ")"
//              | IDENTIFIER ;

pub fn parse<'src, 'a>(
    tokens: &'a [Token]
) -> Result<Vec<Stmt>, Error<'src>> where 'a: 'src {
    let mut statements = vec![];
    let mut curr = 0;

    while curr < tokens.len() {
        if tokens.get(curr).map(|t| t.ttype == TokenType::Eof).unwrap_or(false) { break };
        let (consumed, stmt) = declaration(&tokens[curr..])?;

        statements.push(stmt);
        curr += consumed;
    }

    return Ok(statements);
}

pub fn parse_expr<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<Expr, Error<'src>> where 'a: 'src {
    let (consumed, expr) = expression(tokens)?;
    match tokens.get(consumed).map(|t| &t.ttype) {
        Some(TokenType::Eof) | None => Ok(*expr),
        _ => {
            let next = &tokens[consumed];
            Err(Error {
                ttype: ErrorType::ExprLeftover,
                line_str: next.line_str,
                line: next.line,
                offset: next.offset,
                length: next.length,
            })
        }
    }
}

fn declaration<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    match tokens.get(0).map(|t| &t.ttype) {
        Some(TokenType::Var) => decl_var(tokens),
        _ => statement(tokens),
    }
}

fn decl_var<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    // first token is `var`
    const C_VAR: usize = 1;

    // consumes 1 token after `var`
    let ident = match tokens.get(C_VAR).map(|t| &t.ttype) {
        Some(TokenType::Ident(name)) => Ident(name.to_owned()),
        _ => {
            let prev = &tokens[0];
            return Err(Error {
                ttype: ErrorType::ExpectedIdent,
                line_str: prev.line_str,
                line: prev.line,
                offset: prev.offset + prev.length,
                length: 1,
            });
        },
    };

    // consumes 0 or optionally 1 + n tokens
    let (c_opt, expr) = match tokens.get(C_VAR + 1).map(|t| &t.ttype) {
        Some(TokenType::Equal) => {
            // get expr after equal
            let (c_expr, expr) = expression(&tokens[C_VAR + 2..])?;
            (1 + c_expr, Some(expr))
        },
        _ => (0, None),
    };

    // check semicolon
    let at = C_VAR + 1 + c_opt;
    let prev = &tokens[at - 1];
    let curr = tokens.get(at);
    consume(&prev, curr, TokenType::Semicolon, ErrorType::MissingSemicolon)
        .map(|_| (at + 1, Stmt::Var(ident, expr)))
}

fn statement<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    match tokens.get(0).map(|t| &t.ttype) {
        Some(TokenType::Print) => stmt_prnt(&tokens[1..]),
        _ => stmt_expr(tokens),
    }
}

fn stmt_expr<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    let (consumed, expr) = expression(tokens)?;

    match tokens.get(consumed).map(|t| &t.ttype) {
        Some(TokenType::Semicolon) => Ok((consumed + 1, Stmt::Expression(expr))),
        _ => {
            let last = &tokens[consumed - 1];
            Err(Error {
                ttype: ErrorType::MissingSemicolon,
                line_str: last.line_str,
                line: last.line,
                offset: last.offset + last.length,
                length: 1,
            })
        },
    }
}

fn stmt_prnt<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    let (consumed, expr) = expression(tokens)?;

    match tokens.get(consumed).map(|t| &t.ttype) {
        Some(TokenType::Semicolon) => Ok((consumed + 2, Stmt::Print(expr))),
        _ => {
            let last = &tokens[consumed - 1];
            Err(Error {
                ttype: ErrorType::MissingSemicolon,
                line_str: last.line_str,
                line: last.line,
                offset: last.offset + last.length,
                length: 1,
            })
        },
    }
}

fn expression<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
    assignment(tokens)
}

fn assignment<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
    let (c_expr, expr) = equality(tokens)?;

    match tokens.get(c_expr).map(|t| &t.ttype) {
        Some(TokenType::Equal) => {
            let (c_next, next) = assignment(&tokens[c_expr + 1..])?;

            match *expr {
                Expr::Variable(ident) => Ok((c_expr + 1 + c_next, Box::new(Expr::Assign(ident, next)))),
                _ => {
                    let equals = &tokens[c_expr];
                    Err(Error {
                        ttype: ErrorType::AssignmentTarget,
                        line_str: equals.line_str,
                        line: equals.line,
                        offset: equals.offset,
                        length: equals.length,
                    })
                },
            }
        },
        _ => Ok((c_expr, expr)),
    }
}

fn equality<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
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


fn comparison<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
    let (mut consumed, mut expr) = term(tokens)?;

    let signs = &[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual];
    while matches(tokens.get(consumed), signs) {
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

fn term<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
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

fn factor<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
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

fn unary<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
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

fn primary<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
    let next = &tokens[0];

    use TokenType as TT;
    use Literal::*;
    let (consumed, variant) = match &next.ttype {
        TT::Ident(name) => (1, Box::new(Expr::Variable(Ident(name.to_owned())))),
        TT::Num(num) => (1, Box::new(Expr::Literal(Num(*num)))),
        TT::Str(str) => (1, Box::new(Expr::Literal(Str(str.to_owned())))),
        TT::True     => (1, Box::new(Expr::Literal(True))),
        TT::False    => (1, Box::new(Expr::Literal(False))),
        TT::Nil      => (1, Box::new(Expr::Literal(Nil))),
        TT::ParenL   => {
            let (consumed, expr) = expression(&tokens[1..])?;

            let expr = match tokens.get(1 + consumed).map(|t| &t.ttype) {
                Some(TokenType::ParenR) => Box::new(Expr::Grouping(expr)),
                _ => {
                    let last = &tokens[consumed];
                    return Err(Error {
                        ttype: ErrorType::MissingRightParen,
                        line_str: last.line_str,
                        line: last.line,
                        offset: last.offset + last.length,
                        length: 1,
                    })
                },
            };

            (2 + consumed, expr)
        },
        tt => return Err(Error {
            ttype: ErrorType::InvalidToken(tt.to_owned()),
            line_str: next.line_str,
            line: next.line,
            offset: next.offset,
            length: next.length,
        }),
    };

    Ok((consumed, variant))
}

fn matches(token: Option<&Token>, tts: &[TokenType]) -> bool {
    token
        .map(|token| tts.iter().any(|tt| *tt == token.ttype))
        .unwrap_or(false)
}

fn consume<'src, 'a>(
    prev: &'a Token<'src>,
    curr: Option<&'a Token>,
    tt: TokenType,
    et: ErrorType,
) -> Result<(), Error<'src>> {
    match curr.map(|t| t.ttype == tt) {
        Some(true) => Ok(()),
        _ => Err(Error {
            ttype: et,
            line_str: prev.line_str,
            line: prev.line,
            offset: prev.offset + prev.length,
            length: 1,
        }),
    }
}
