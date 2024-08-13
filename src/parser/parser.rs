use crate::lexer::{token::Token, token_type::TokenType};
use super::ast::{Stmt, Expr, Literal, Ident};
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
//              | stmtReturn
//              | stmtExpr ;
//
// block        → "{" declaration* "}" ;
// stmtIf       → "if" "(" expression ")" statement ( "else" statement )? ;
// stmtWhile    → "while" "(" expression ")" statement;
// stmtFor      → "for" "(" ( declVar | stmtExpr | ";" ) expression? ";" expression? ")" statement ;
// stmtreturn   → "return" expression? ";" ;
// stmtExpr     → expression ";" ;
//
// expression   → assignment ;
// assignment   → IDENTIFIER "=" assignment
//              | exprPratt ;
// exprPratt    → call
//              | PRATT ;
//
// call         → primary ( primary )* ;
//
// arguments    → expression ( "," expression )* ;
//
// primary      → NUMBER | STRING | "nil"
//              | "true" | "false"
//              | "()" | "(" expression ")"
//              | lambda
//              | IDENTIFIER ;
//
// array        → "[" ( expression ( "," expression )* )? "]" ;
//
// lambda       → "fn" ( IDENTIFIER )+ "->" expression ;


pub fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, Error> {
    let mut ptr = 0;
    let mut top = vec![];

    while !matches!(tokens[ptr].ttype, TokenType::Eof) {
        let (n, decl) = declaration(&tokens[ptr..])?;
        ptr += n;       // n = decl

        top.push(decl);
    }

    Ok(top)
}

pub fn parse_expr(tokens: &[Token]) -> Result<Expr, Error> {
    let mut ptr = 0;

    let (n, expr) = expression(tokens)?;
    ptr += n;           // n = expr

    consume(tokens, ptr, TokenType::Eof, ErrorType::ExprLeftover)?;

    Ok(*expr)
}

fn declaration(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    match tokens.first().map(|t| &t.ttype) {
        Some(TokenType::Var) => decl_var(tokens),
        // Some(TokenType::Fun) => decl_fun(tokens),
        _ => statement(tokens),
    }
}

// fn decl_fun(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
//     function(tokens)
// }

// fn function(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
//     let mut ptr = 1;    // 1 = fun
//
//     let ident = consume_ident(tokens, ptr)?;
//     ptr += 1;           // 1 = ident
//
//     consume(tokens, ptr, TokenType::ParenL, ErrorType::MissingParenL)?;
//     ptr += 1;           // 1 = (
//
//     let (n, params) = parameters(&tokens[ptr-1..])?;
//     ptr += n;           // n = params
//
//     consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
//     ptr += 1;           // 1 = )
//
//     let (n, block) = block(&tokens[ptr..])?;
//     ptr += n;           // n = block
//
//     Ok((ptr, Stmt::Function(ident, params, block)))
// }

fn parameters(tokens: &[Token]) -> Result<(usize, Vec<Ident>), Error> {
    let mut ptr = 1;    // 1 = (
    let mut params = vec![];

    // if no params bail out early
    if matches(tokens.get(ptr), &[TokenType::ParenR]) { return Ok((0, params)) }

    loop {
        let ident = consume_ident(tokens, ptr)?;
        ptr += 1;       // 1 =? ident

        params.push(ident);
        if !matches(tokens.get(ptr), &[TokenType::Comma]) { break };
        ptr += 1;       // 1 =? ,
    };

    Ok((ptr-1, params))
}

fn decl_var(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    let mut ptr = 1;    // 1 = var

    let ident = consume_ident(tokens, ptr)?;
    ptr += 1;           // 1 = ident

    let expr = match tokens.get(ptr) {
        Some(Token { ttype: TokenType::Op(ref op), .. }) if op == "=" => {
            ptr += 1;       // 1 = =

            let (n, expr) = expression(&tokens[ptr..])?;
            ptr += n;       // n = expr

            Some(expr)
        },
        _ => None,
    };

    consume(tokens, ptr, TokenType::Semicolon, ErrorType::MissingSemicolon)?;
    ptr += 1;           // 1 = ;

    Ok((ptr, Stmt::Var(ident, expr)))
}

fn statement(
    tokens: &[Token]
) -> Result<(usize, Stmt), Error> {
    match tokens.first().map(|t| &t.ttype) {
        Some(TokenType::BraceL) => {
            let (n, block) = block(tokens)?;
            Ok((n, Stmt::Block(block)))
        },
        Some(TokenType::If)     => stmt_if(tokens),
        Some(TokenType::While)  => stmt_while(tokens),
        Some(TokenType::For)    => stmt_for(tokens),
        Some(TokenType::Return) => stmt_return(tokens),
        _ => stmt_expr(tokens),
    }
}

fn stmt_return(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    let mut ptr = 1;    // 1 = return

    let (n, expr) = match matches(tokens.get(ptr), &[TokenType::Semicolon]) {
        true => (0, None),
        false => {
            let (n, expr) = expression(&tokens[ptr..])?;
            (n, Some(expr))
        },
    };
    ptr += n;           // n =? expr

    consume(tokens, ptr, TokenType::Semicolon, ErrorType::MissingSemicolon)?;
    ptr += 1;           // 1 = ;

    Ok((ptr, Stmt::Return(expr)))
}

fn block(tokens: &[Token]) -> Result<(usize, Vec<Stmt>), Error> {
    let mut ptr = 1;    // 1 = {
    let mut block = vec![];

    while ptr < tokens.len() && !matches(tokens.get(ptr), &[TokenType::BraceR, TokenType::Eof]) {
        let (n, stmt) = declaration(&tokens[ptr..])?;
        ptr += n;

        block.push(stmt);
    }

    consume(tokens, ptr, TokenType::BraceR, ErrorType::MissingBrace)?;
    ptr += 1;           // 1 = }

    Ok((ptr, block))
}

fn stmt_if(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    let mut ptr = 1;    // 1 = if
    consume(tokens, ptr, TokenType::ParenL, ErrorType::MissingParenL)?;
    ptr += 1;           // 1 = (
    let (n, cond) = expression(&tokens[ptr..])?;
    ptr += n;           // n = expr
    consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
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

fn stmt_while(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    let mut ptr = 1;    // 1 = while
    consume(tokens, ptr, TokenType::ParenL, ErrorType::MissingParenL)?;
    ptr += 1;           // 1 = (
    let (n, cond) = expression(&tokens[ptr..])?;
    ptr += n;           // n = expr
    consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
    ptr += 1;           // 1 = )
    let (n, body) = statement(&tokens[ptr..])?;
    ptr += n;           // n = expr

    Ok((ptr, Stmt::While(cond, body.into())))
}

fn stmt_for(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    let mut ptr = 1;    // 1 = for
    consume(tokens, ptr, TokenType::ParenL, ErrorType::MissingParenL)?;
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
    consume(tokens, ptr, TokenType::Semicolon, ErrorType::MissingSemicolon)?;
    ptr += 1;           // 1 = ;

    let inc = match tokens.get(ptr).map(|t| &t.ttype) {
        Some(TokenType::ParenR) => None,
        _ => {
            let (n, expr) = expression(&tokens[ptr..])?;
            ptr += n;   // n =? inc
            Some(expr)
        },
    };
    consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
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

fn stmt_expr(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    let mut ptr = 0;

    let (n, expr) = expression(tokens)?;
    ptr += n;           // n = expr

    consume(tokens, ptr, TokenType::Semicolon, ErrorType::MissingSemicolon)?;
    ptr += 1;           // 1 = ;

    Ok((ptr, Stmt::Expression(expr)))
}


fn expression(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    assignment(tokens)
}

fn assignment(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, expr) = pratt(tokens, 0)?;
    ptr += n;           // n = expr

    match tokens.get(n).map(|t| &t.ttype) {
        Some(TokenType::Op(ref op)) if op == "="  => {
            let (c_next, next) = assignment(&tokens[n + 1..])?;

            match *expr {
                Expr::Variable(ident) => Ok((n + 1 + c_next, Box::new(Expr::Assign(ident, next)))),
                _ => {
                    let equals = &tokens[n];
                    Err(Error {
                        ttype: ErrorType::AssignmentTarget,
                        line: equals.line,
                        offset: equals.offset,
                        length: equals.length,
                    })
                },
            }
        },
        _ => Ok((ptr, expr)),
    }
}

/// callable arg1 arg2 ...
fn call(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = primary(tokens)?;
    ptr += n;       // n callable

    while let Ok((n, arg)) = primary(&tokens[ptr..]) {
        ptr += n;   // n arg
        expr = Box::new(Expr::Call(expr, arg));
    }

    Ok((ptr, expr))
}

fn arguments(tokens: &[Token]) -> Result<(usize, Vec<Expr>), Error> {
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

fn primary(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let next = &tokens[0];

    use TokenType as TT;
    use Literal::*;
    let (consumed, variant) = match &next.ttype {
        TT::Ident(name) => (1, Box::new(Expr::Variable(Ident(name.to_owned())))),
        TT::Num(num)    => (1, Box::new(Expr::Literal(Num(*num)))),
        TT::Str(str)    => (1, Box::new(Expr::Literal(Str(str.to_owned())))),
        TT::True        => (1, Box::new(Expr::Literal(True))),
        TT::False       => (1, Box::new(Expr::Literal(False))),
        TT::Nil         => (1, Box::new(Expr::Literal(Nil))),
        TT::Fun         => expr_lambda(tokens)?,
        TT::ParenL      => expr_parenthesized(tokens)?,
        TT::SquareL     => expr_array(tokens)?,
        tt => return Err(Error {
            ttype: ErrorType::InvalidToken(tt.to_owned()),
            line: next.line,
            offset: next.offset,
            length: next.length,
        }),
    };

    Ok((consumed, variant))
}

fn expr_parenthesized(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;        // 1 = (

    if let Some(Token { ttype: TokenType::ParenR, .. }) = tokens.get(ptr) {
        return Ok((2, Box::new(Expr::Literal(Literal::Nil))))
    }

    let (n, expr) = expression(&tokens[ptr..])?;
    ptr += n;               // n = expr

    if let Some(Token { ttype: TokenType::Comma, .. }) = tokens.get(ptr) {
        ptr += 1;
        let mut elements = vec![*expr];

        if let Some(Token { ttype: TokenType::ParenR, .. }) = tokens.get(ptr) {
            ptr += 1;
            return Ok((ptr, Box::new(Expr::Tuple(elements))))
        }

        // Parse subsequent expressions, separated by commas
        while let Some(_) = tokens.get(ptr) {
            let (n, next_expr) = expression(&tokens[ptr..])?;
            elements.push(*next_expr);
            ptr += n;

            // If there's a comma, continue to the next expression
            if let Some(Token { ttype: TokenType::Comma, .. }) = tokens.get(ptr) {
                ptr += 1;
            } else {
                break;
            }
        }

        // Expect a closing ')'
        consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
        ptr += 1;

        return Ok((ptr, Box::new(Expr::Tuple(elements))));
    }

    consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
    ptr += 1;               // 1 = )

    Ok((ptr, Box::new(Expr::Grouping(expr))))
}

fn expr_array(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;        // [

    let (n, arr) = arguments(&tokens[ptr..])?;
    ptr += n;

    consume(tokens, ptr, TokenType::SquareR, ErrorType::MissingParenR)?;
    ptr += 1;

    Ok((ptr, Expr::Array(arr).into()))
}


fn expr_lambda(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;        // 1 fn
    let mut args = vec![];

    loop {
        let expr = consume_ident(tokens, ptr)?;
        ptr += 1;           // 1 IDENTIFIER
        args.push(expr);

        match tokens.get(ptr) {
            Some(Token { ttype: TokenType::Op(ref op), .. }) if op == "->" => {
                ptr += 1;   // 1 ->
                break;
            },
            _ => continue,
        }
    }

    let (n, expr) = expression(&tokens[ptr..])?;
    ptr += n;               // n expr

    let mut args = args.into_iter().rev();
    let last = args.next().expect("Lambda should have at least one argument.");
    let func = args.fold(
        Expr::Lambda(last, expr),
        |acc, next| Expr::Lambda(next, acc.into())
    );

    Ok((ptr, func.into()))
}

fn matches(token: Option<&Token>, tts: &[TokenType]) -> bool {
    token
        .map(|token| tts.iter().any(|tt| *tt == token.ttype))
        .unwrap_or(false)
}

fn consume(
    tokens: &[Token],
    at: usize,
    tt: TokenType,
    et: ErrorType,
) -> Result<(), Error> {
    let prev = &tokens[at-1];
    match tokens.get(at).map(|t| t.ttype == tt) {
        Some(true) => Ok(()),
        _ => Err(Error {
            ttype: et,
            line: prev.line,
            offset: prev.offset + prev.length,
            length: 1,
        }),
    }
}

fn consume_ident(
    tokens: &[Token],
    at: usize,
) -> Result<Ident, Error> {
    let prev = &tokens[at-1];
    match tokens.get(at).map(|t| &t.ttype) {
        Some(TokenType::Ident(name)) => Ok(Ident(name.clone())),
        _ => Err(Error {
            ttype: ErrorType::ExpectedIdent,
            line: prev.line,
            offset: prev.offset + prev.length,
            length: 1,
        }),
    }
}

// logicOr      → logicAnd ( "||" logicAnd )* ;
// logicAnd     → equality ( "&&" equality )* ;
// equality     → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison   → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term         → factor ( ( "-" | "+" ) factor )* ;
// factor       → unary ( ( "/" | "*" ) unary )* ;
// unary        → ( "!" | "-" ) unary

fn precedence(str: &str, unary: bool) -> (i32, i32) {
    if unary { return (20, 20) };
    match str {
        "||" => (1, 1),
        "&&" => (2, 2),
        "!=" | "==" => (3, 3),
        ">" | ">=" | "<" | "<=" => (4, 4),
        "-" | "+" => (5, 5),
        "/" | "*" => (10, 10),
        _ => (0, 0),
    }
}

fn pratt(tokens: &[Token], min_p: i32) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let mut expr_l = match tokens[ptr] {
        Token { ttype: TokenType::Op(ref op), .. } => {
            let (_, bp_r) = precedence(op, true);
            ptr += 1;

            let (n, expr_r) = pratt(&tokens[ptr..], bp_r)?;
            ptr += n;

            Box::new(Expr::Unary(op.into(), expr_r))
        }
        _ => {
            let (n, expr_l) = call(tokens)?;
            ptr += n;
            expr_l
        },
    };

    while let Some(TokenType::Op(ref op)) = tokens.get(ptr).map(|t| &t.ttype) {
        let (bp_l, bp_r) = precedence(op, false);

        if bp_l < min_p {
            break;
        }

        ptr += 1;

        let (n, expr_r) = pratt(&tokens[ptr..], bp_r + 1)?;
        ptr += n;

        expr_l = Box::new(Expr::Binary(expr_l, op.into(), expr_r));
    };

    Ok((ptr, expr_l))
}
