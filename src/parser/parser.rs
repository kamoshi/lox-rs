use crate::lexer::{token::Token, token_type::TokenType};
use super::ast::{Stmt, Expr, OpBinary, Literal, OpUnary, Ident, OpLogic};
use super::error::{Error, ErrorType};


// program      → statement* EOF ;
//
// declaration  → declVar
//              | declFun
//              | statement ;
//
// declVar      → "var" IDENTIFIER ( "=" expression )? ";" ;
// declFun      → "fun" function ;
//
// function     → IDENTIFIER "(" parameters? ")" block ;
// parameters   → IDENTIFIER ( "," IDENTIFIER )* ;
//
// statement    → block
//              | stmtIf
//              | stmtWhile
//              | stmtFor
//              | stmtExpr ;
//
// block        → "{" declaration* "}" ;
// stmtIf       → "if" "(" expression ")" statement ( "else" statement )? ;
// stmtWhile    → "while" "(" expression ")" statement;
// stmtFor      → "for" "(" ( declVar | stmtExpr | ";" ) expression? ";" expression? ")" statement ;
// stmtExpr     → expression ";" ;
//
// expression   → assignment ;
// assignment   → IDENTIFIER "=" assignment
//              | logicOr ;
// logicOr      → logicAnd ( "or" logicAnd )* ;
// logicAnd     → equality ( "and" equality )* ;
// equality     → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison   → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term         → factor ( ( "-" | "+" ) factor )* ;
// factor       → unary ( ( "/" | "*" ) unary )* ;
// unary        → ( "!" | "-" ) unary
//              | call ;
// call         → primary ( "(" arguments? ")" )* ;
// primary      → NUMBER | STRING | "true" | "false" | "nil"
//              | "(" expression ")"
//              | IDENTIFIER ;
//
// arguments    → expression ( "," expression )* ;


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
        Some(TokenType::Fun) => decl_fun(tokens),
        _ => statement(tokens),
    }
}

fn decl_fun<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    function(tokens)
}

fn function<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    let mut ptr = 1;    // 1 = fun

    let ident = consume_ident(&tokens[ptr-1], tokens.get(ptr))?;
    ptr += 1;           // 1 = ident

    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::ParenL, ErrorType::MissingParenL)?;
    ptr += 1;           // 1 = (

    let (n, params) = parameters(&tokens[ptr-1..])?;
    ptr += n;

    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::ParenR, ErrorType::MissingParenR)?;
    ptr += 1;           // 1 = )

    let (n, block) = block(&tokens[ptr..])?;
    ptr += n;           // n = block

    Ok((ptr, Stmt::Function(ident, params, block)))
}

fn parameters<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Vec<Ident>), Error<'src>> where 'a: 'src {
    const PARAM_L: usize = 1;
    let mut ptr = PARAM_L;  // 1 = (
    let mut params = vec![];

    // if no params bail out early
    if matches(tokens.get(ptr), &[TokenType::ParenR]) { return Ok((0, params)) }

    loop {
        let ident = consume_ident(&tokens[ptr-1], tokens.get(ptr))?;
        params.push(ident);
        ptr += 1;       // 1 =? ident

        match matches(tokens.get(ptr), &[TokenType::Comma]) {
            true => ptr += 1, // 1 =? ,
            false => break,
        }
    };

    Ok((ptr - PARAM_L, params))
}

fn decl_var<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    // first token is `var`
    const VAR: usize = 1;

    // consumes 1 token after `var`
    let ident = match tokens.get(VAR).map(|t| &t.ttype) {
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
    let (c_opt, expr) = match tokens.get(VAR + 1).map(|t| &t.ttype) {
        Some(TokenType::Equal) => {
            // get expr after equal
            let (c_expr, expr) = expression(&tokens[VAR + 2..])?;
            (1 + c_expr, Some(expr))
        },
        _ => (0, None),
    };

    // check semicolon
    let at = VAR + 1 + c_opt;
    let prev = &tokens[at - 1];
    let curr = tokens.get(at);
    consume(&prev, curr, TokenType::Semicolon, ErrorType::MissingSemicolon)?;

    Ok((at + 1, Stmt::Var(ident, expr)))
}

fn statement<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    match tokens.get(0).map(|t| &t.ttype) {
        Some(TokenType::BraceL) => {
            let (n, block) = block(tokens)?;
            Ok((n, Stmt::Block(block)))
        },
        Some(TokenType::If)     => stmt_if(tokens),
        Some(TokenType::While)  => stmt_while(tokens),
        Some(TokenType::For)    => stmt_for(tokens),
        _ => stmt_expr(tokens),
    }
}

fn block<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Vec<Stmt>), Error<'src>> where 'a: 'src {
    let mut statements = Vec::new();
    let mut ptr = 1;

    while ptr < tokens.len() && !matches(tokens.get(ptr), &[TokenType::BraceR, TokenType::Eof]) {
        let (c, stmt) = declaration(&tokens[ptr..])?;

        statements.push(stmt);
        ptr += c;
    }

    // check closing brace
    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::BraceR, ErrorType::MissingBrace)?;

    Ok((ptr + 1, statements))
}

fn stmt_if<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    let mut ptr = 1;    // 1 = if
    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::ParenL, ErrorType::MissingParenL)?;
    ptr += 1;           // 1 = (
    let (n, cond) = expression(&tokens[ptr..])?;
    ptr += n;           // n = expr
    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::ParenR, ErrorType::MissingParenR)?;
    ptr += 1;           // 1 = )

    let (n, branch_t) = statement(&tokens[ptr..])?;
    ptr += n;           // n = true branch

    let branch_f = match matches(tokens.get(ptr), &[TokenType::Else]) {
        true => {
            ptr += 1;   // 1 =? else
            let (n, branch_f) = statement(&tokens[ptr..])?;
            ptr += n;   // n =? false branch
            Some(branch_f.into())
        },
        false => None,
    };

    Ok((ptr, Stmt::If(cond, branch_t.into(), branch_f)))
}

fn stmt_while<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    let mut ptr = 1;    // 1 = while
    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::ParenL, ErrorType::MissingParenL)?;
    ptr += 1;           // 1 = (
    let (n, cond) = expression(&tokens[ptr..])?;
    ptr += n;           // n = expr
    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::ParenR, ErrorType::MissingParenR)?;
    ptr += 1;           // 1 = )
    let (n, body) = statement(&tokens[ptr..])?;
    ptr += n;           // n = expr

    Ok((ptr, Stmt::While(cond, body.into())))
}

fn stmt_for<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Stmt), Error<'src>> where 'a: 'src {
    let mut ptr = 1;    // 1 = for
    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::ParenL, ErrorType::MissingParenL)?;
    ptr += 1;           // 1 = (

    let init = match tokens.get(ptr).map(|t| &t.ttype) {
        Some(TokenType::Semicolon) => {
            ptr += 1;   // 1 =? ;
            None
        },
        Some(TokenType::Var) => {
            let (n, init) = decl_var(&tokens[ptr..])?;
            ptr += n;   // n =? init
            Some(init)
        },
        _ => {
            let (n, init) = stmt_expr(&tokens[ptr..])?;
            ptr += n;   // n =? init
            Some(init)
        },
    };

    let cond = match tokens.get(ptr).map(|t| &t.ttype) {
        Some(TokenType::Semicolon) => Expr::Literal(Literal::True).into(),
        _ => {
            let (n, expr) = expression(&tokens[ptr..])?;
            ptr += n;   // n =? cond
            expr
        },
    };
    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::Semicolon, ErrorType::MissingSemicolon)?;
    ptr += 1;           // 1 = ;

    let inc = match tokens.get(ptr).map(|t| &t.ttype) {
        Some(TokenType::ParenR) => None,
        _ => {
            let (n, expr) = expression(&tokens[ptr..])?;
            ptr += n;   // n =? inc
            Some(expr)
        },
    };
    consume(&tokens[ptr-1], tokens.get(ptr), TokenType::ParenR, ErrorType::MissingParenR)?;
    ptr += 1;           // 1 = )

    let (n, body) = statement(&tokens[ptr..])?;
    ptr += n;           // n = body

    let body = match inc {
        Some(inc) => Stmt::Block(vec![body, Stmt::Expression(inc)]),
        None => body,
    };

    let body = Stmt::While(cond, body.into());

    let body = match init {
        Some(init) => Stmt::Block(vec![init, body]),
        None => body,
    };

    Ok((ptr, body))
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


fn expression<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
    assignment(tokens)
}

fn assignment<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
    let (c_expr, expr) = or(tokens)?;

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

fn or<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
    let mut ptr = 0;
    let (n, mut expr) = and(tokens)?;
    ptr += n;       // n = expr

    while matches(tokens.get(ptr), &[TokenType::Or]) {
        ptr += 1;   // 1 =* or
        let (n, r) = and(&tokens[ptr..])?;
        ptr += n;   // n =* expr
        expr = Box::new(Expr::Logic(expr, OpLogic::Or, r));
    };

    Ok((ptr, expr))
}

fn and<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
    let mut ptr = 0;
    let (n, mut expr) = equality(tokens)?;
    ptr += n;       // n = expr

    while matches(tokens.get(ptr), &[TokenType::And]) {
        ptr += 1;   // 1 =* and
        let (n, r) = equality(&tokens[ptr..])?;
        ptr += n;   // n =* expr
        expr = Box::new(Expr::Logic(expr, OpLogic::And, r));
    };

    Ok((ptr, expr))
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
        _ => Ok(call(tokens)?),
    }
}

fn call<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Box<Expr>), Error<'src>> where 'a: 'src {
    let mut ptr = 0;
    let (n, mut expr) = primary(tokens)?;
    ptr += n;       // n = expr

    while matches(tokens.get(ptr), &[TokenType::ParenL]) {
        ptr += 1;   // 1 = (
        let (n, args) = arguments(&tokens[ptr..])?;
        ptr += n;   // n = arguments
        consume(&tokens[ptr-1], tokens.get(ptr), TokenType::ParenR, ErrorType::MissingParenR)?;
        ptr += 1;   // 1 = )

        expr = Box::new(Expr::Call(expr, args));
    };

    Ok((ptr, expr))
}

fn arguments<'src, 'a>(
    tokens: &'a [Token<'src>]
) -> Result<(usize, Vec<Expr>), Error<'src>> where 'a: 'src {
    let mut ptr = 0;
    let mut args = vec![];

    // if no args bail out early
    if matches(tokens.get(ptr), &[TokenType::ParenR]) { return Ok((ptr, args)) }

    loop {
        let (n, expr) = expression(&tokens[ptr..])?;
        args.push(*expr);
        ptr += n;   // n =? expr

        match matches(tokens.get(ptr), &[TokenType::Comma]) {
            true => ptr += 1, // 1 =? ,
            false => break,
        }
    };

    Ok((ptr, args))
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
                        ttype: ErrorType::MissingParenR,
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

fn consume_ident<'src, 'a>(
    last: &'a Token<'src>,
    next: Option<&'a Token<'src>>,
) -> Result<Ident, Error<'src>> {
    match next.map(|t| &t.ttype) {
        Some(TokenType::Ident(name)) => Ok(Ident(name.clone())),
        _ => Err(Error {
            ttype: ErrorType::ExpectedIdent,
            line_str: last.line_str,
            line: last.line,
            offset: last.offset,
            length: 1,
        }),
    }
}
