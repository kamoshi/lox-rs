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
// arguments    → expression ( "," expression )* ;
//
// primary      → NUMBER | STRING | "true" | "false" | "nil"
//              | "(" expression ")"
//              | lambda
//              | IDENTIFIER ;
//
// array        → "[" ( expression ( "," expression )* )? "]" ;
//
// lambda       → "fun" "(" parameters? ")" block ;


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
        Some(TokenType::Fun) => decl_fun(tokens),
        _ => statement(tokens),
    }
}

fn decl_fun(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    function(tokens)
}

fn function(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
    let mut ptr = 1;    // 1 = fun

    let ident = consume_ident(tokens, ptr)?;
    ptr += 1;           // 1 = ident

    consume(tokens, ptr, TokenType::ParenL, ErrorType::MissingParenL)?;
    ptr += 1;           // 1 = (

    let (n, params) = parameters(&tokens[ptr-1..])?;
    ptr += n;           // n = params

    consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
    ptr += 1;           // 1 = )

    let (n, block) = block(&tokens[ptr..])?;
    ptr += n;           // n = block

    Ok((ptr, Stmt::Function(ident, params, block)))
}

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

    let expr = match tokens.get(ptr).map(|t| &t.ttype) {
        Some(TokenType::Equal) => {
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

    let (n, expr) = or(tokens)?;
    ptr += n;           // n = expr

    match tokens.get(n).map(|t| &t.ttype) {
        Some(TokenType::Equal) => {
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

fn or(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = and(tokens)?;
    ptr += n;           // n = expr

    while matches(tokens.get(ptr), &[TokenType::Or]) {
        ptr += 1;       // 1 =* or
        let (n, r) = and(&tokens[ptr..])?;
        ptr += n;       // n =* expr
        expr = Box::new(Expr::Logic(expr, OpLogic::Or, r));
    };

    Ok((ptr, expr))
}

fn and(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = equality(tokens)?;
    ptr += n;           // n = expr

    while matches(tokens.get(ptr), &[TokenType::And]) {
        ptr += 1;       // 1 =* and
        let (n, r) = equality(&tokens[ptr..])?;
        ptr += n;       // n =* expr
        expr = Box::new(Expr::Logic(expr, OpLogic::And, r));
    };

    Ok((ptr, expr))
}

fn equality(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = comparison(tokens)?;
    ptr += n;           // n = expr

    while matches(tokens.get(ptr), &[TokenType::BangEqual, TokenType::EqualEqual]) {
        let op = match tokens[ptr].ttype {
            TokenType::BangEqual    => OpBinary::NotEqual,
            TokenType::EqualEqual   => OpBinary::Equal,
            _ => unreachable!(),
        };
        ptr += 1;       // 1 = op

        let (n, next) = comparison(&tokens[ptr..])?;
        ptr += n;       // n = expr

        expr = Box::new(Expr::Binary(expr, op, next))
    }

    Ok((ptr, expr))
}


fn comparison(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = term(tokens)?;
    ptr += n;           // n = expr

    let signs = &[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual];
    while matches(tokens.get(ptr), signs) {
        let op = match tokens[ptr].ttype {
            TokenType::Greater      => OpBinary::Greater,
            TokenType::GreaterEqual => OpBinary::GreaterEqual,
            TokenType::Less         => OpBinary::Less,
            TokenType::LessEqual    => OpBinary::LessEqual,
            _ => unreachable!(),
        };
        ptr += 1;       // 1 = op

        let (n, next) = term(&tokens[ptr..])?;
        ptr += n;       // n = expr

        expr = Box::new(Expr::Binary(expr, op, next))
    }

    Ok((ptr, expr))
}

fn term(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = factor(tokens)?;
    ptr += n;           // n = expr

    while matches(tokens.get(ptr), &[TokenType::Minus, TokenType::Plus]) {
        let op = match tokens[ptr].ttype {
            TokenType::Minus    => OpBinary::Sub,
            TokenType::Plus     => OpBinary::Add,
            _ => unreachable!(),
        };
        ptr += 1;       // 1 = op

        let (n, next) = factor(&tokens[ptr..])?;
        ptr += n;       // n = expr

        expr = Box::new(Expr::Binary(expr, op, next))
    }

    Ok((ptr, expr))
}

fn factor(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = unary(tokens)?;
    ptr += n;           // n = expr

    while matches(tokens.get(ptr), &[TokenType::Slash, TokenType::Star]) {
        let op = match tokens[ptr].ttype {
            TokenType::Slash    => OpBinary::Div,
            TokenType::Star     => OpBinary::Mul,
            _ => unreachable!(),
        };
        ptr += 1;       // 1 = op

        let (n, next) = unary(&tokens[ptr..])?;
        ptr += n;       // n = expr

        expr = Box::new(Expr::Binary(expr, op, next))
    }

    Ok((ptr, expr))
}

fn unary(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    match matches(tokens.get(ptr), &[TokenType::Bang, TokenType::Minus]) {
        true => {
            let op = match tokens[ptr].ttype {
                TokenType::Bang     => OpUnary::Not,
                TokenType::Minus    => OpUnary::Neg,
                _ => unreachable!()
            };
            ptr += 1;   // 1 = op

            let (n, expr) = unary(&tokens[1..])?;
            ptr += n;   // n = expr

            let expr = Box::new(Expr::Unary(op, expr));
            Ok((ptr, expr))
        },
        false => call(tokens),
    }
}

fn call(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = primary(tokens)?;
    ptr += n;       // n = expr

    while matches(tokens.get(ptr), &[TokenType::ParenL]) {
        ptr += 1;   // 1 = (
        let (n, args) = arguments(&tokens[ptr..])?;
        ptr += n;   // n = arguments
        consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
        ptr += 1;   // 1 = )

        expr = Box::new(Expr::Call(expr, args));
    };

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
        TT::ParenL      => expr_group(tokens)?,
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

fn expr_group(tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;        // 1 = (

    let (n, expr) = expression(&tokens[ptr..])?;
    ptr += n;               // n = expr

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
    let mut ptr = 1;    // 1 = fun

    consume(tokens, ptr, TokenType::ParenL, ErrorType::MissingParenL)?;
    ptr += 1;           // 1 = (

    let (n, ident) = parameters(&tokens[ptr-1..])?;
    ptr += n;           // n = ident

    consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
    ptr += 1;           // 1 = )

    let (n, block) = block(&tokens[ptr..])?;
    ptr += n;           // n = block

    Ok((ptr, Expr::Lambda(ident, block).into()))
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
