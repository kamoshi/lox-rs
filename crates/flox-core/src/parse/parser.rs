use std::collections::HashMap;

use crate::lex::{token::Token, token_type::TokenType};
use super::ast::{Expr, Literal, Ident};
use super::error::{Error, ErrorType};


// program      → statement* EOF ;
//
// sequence     → (expression ";")* expression? ;
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
//              | stmtFor
//              | stmtReturn
//              | stmtExpr ;
//
// block        → "{" declaration* "}" ;
// stmtFor      → "for" "(" ( declVar | stmtExpr | ";" ) expression? ";" expression? ")" statement ;
// stmtExpr     → expression ";" ;
//
// expression   → IDENTIFIER "=" assignment
//              | exprLet
//              | exprIf
//              | exprWhile
//              | exprReturn
//              | exprPratt ;
//
// exprVariant  → "variant" IDENTIFIER ( "|" IDENTIFIER IDENTIFIER* )*
// exprMatch    → "match" IDENTIFIER ( "|" IDENTIFIER "->" expression)*
// exprLet      → "let" IDENTIFIER "=" expression ;
// exprIf       → "if" "(" expression ")" expression ( "else" expression )?
// exprWhile    → "while" "(" expression ")" expression;
// exprReturn   → "return" expression? ;
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


#[derive(Debug)]
pub struct Context {
    precedence: HashMap<String, (usize, usize)>,
}

impl Context {
    fn get_precedence(&self, op: &str) -> (usize, usize) {
        self.precedence.get(op).cloned().unwrap_or_default()
    }

    fn set_precedence(&mut self, op: String, p: (usize, usize)) {
        self.precedence.insert(op, p);
    }
}

impl Default for Context {
    fn default() -> Self {
        let p = [
            // logic
            ("||", (1, 2)),
            ("&&", (2, 3)),
            // equal
            ("!=", (3, 4)),
            ("==", (3, 4)),
            // compare
            (">", (4, 5)),
            (">=", (4, 5)),
            ("<", (4, 5)),
            ("<=", (4, 5)),
            // add / sub
            ("+", (5, 6)),
            ("-", (5, 6)),
            // mul / div
            ("*", (10, 11)),
            ("/", (10, 11)),
        ]
            .into_iter()
            .map(|(op, p)| (op.to_string(), p));

        Self {
            precedence: HashMap::from_iter(p),
        }
    }
}


pub fn parse(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, expr) = sequence(ctx, tokens)?;
    ptr += n;           // n = expr

    consume(tokens, ptr, TokenType::Eof, ErrorType::ExprLeftover)?;
    ptr += 1;

    Ok((ptr, expr))
}

/// expr1; expr2; expr3
fn sequence(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;
    let mut exprs = vec![];

    loop {
        match tokens.get(ptr) {
            Some(Token { ttype: TokenType::Eof, .. })       => break,
            Some(Token { ttype: TokenType::BraceR, .. })    => break,
            Some(Token { ttype: TokenType::Semicolon, .. }) => {
                ptr += 1;    // 1 ;
            },
            _ => {
                let (n, expr) = expression(ctx, &tokens[ptr..])?;
                ptr += n;    // n expr
                exprs.push(*expr);
            },
        }

    }

    Ok((ptr, Expr::Block(exprs.into()).into()))
}

// fn declaration(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
//     match tokens.first().map(|t| &t.ttype) {
//         Some(TokenType::Var) => decl_var(tokens),
//         // Some(TokenType::Fun) => decl_fun(tokens),
//         _ => statement(tokens),
//     }
// }

// fn decl_fun(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
//     function(tokens)
// }

fn parameters(ctx: &Context, tokens: &[Token]) -> Result<(usize, Vec<Ident>), Error> {
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

// fn statement(
//     tokens: &[Token]
// ) -> Result<(usize, Stmt), Error> {
//     match tokens.first().map(|t| &t.ttype) {
//         Some(TokenType::BraceL) => {
//             let (n, block) = block(tokens)?;
//             Ok((n, Stmt::Block(block)))
//         },
//         Some(TokenType::While)  => stmt_while(tokens),
//         Some(TokenType::For)    => stmt_for(tokens),
//         Some(TokenType::Return) => stmt_return(tokens),
//         _ => stmt_expr(tokens),
//     }
// }

fn expr_block(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;    // 1 = {

    let (n, seq) = sequence(ctx, &tokens[ptr..])?;
    ptr += n;

    consume(tokens, ptr, TokenType::BraceR, ErrorType::MissingBrace)?;
    ptr += 1;           // 1 = }

    Ok((ptr, seq))
}

// fn stmt_for(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
//     let mut ptr = 1;    // 1 = for
//     consume(tokens, ptr, TokenType::ParenL, ErrorType::MissingParenL)?;
//     ptr += 1;           // 1 = (
//
//     let init = match tokens.get(ptr).map(|t| &t.ttype) {
//         Some(TokenType::Semicolon) => {
//             ptr += 1;   // 1 =? ;
//             None
//         },
//         Some(TokenType::Var) => {
//             let (n, init) = decl_var(&tokens[ptr..])?;
//             ptr += n;   // n =? init
//             Some(init)
//         },
//         _ => {
//             let (n, init) = stmt_expr(&tokens[ptr..])?;
//             ptr += n;   // n =? init
//             Some(init)
//         },
//     };
//
//     let cond = match tokens.get(ptr).map(|t| &t.ttype) {
//         Some(TokenType::Semicolon) => Expr::Literal(Literal::True).into(),
//         _ => {
//             let (n, expr) = expression(&tokens[ptr..])?;
//             ptr += n;   // n =? cond
//             expr
//         },
//     };
//     consume(tokens, ptr, TokenType::Semicolon, ErrorType::MissingSemicolon)?;
//     ptr += 1;           // 1 = ;
//
//     let inc = match tokens.get(ptr).map(|t| &t.ttype) {
//         Some(TokenType::ParenR) => None,
//         _ => {
//             let (n, expr) = expression(&tokens[ptr..])?;
//             ptr += n;   // n =? inc
//             Some(expr)
//         },
//     };
//     consume(tokens, ptr, TokenType::ParenR, ErrorType::MissingParenR)?;
//     ptr += 1;           // 1 = )
//
//     let (n, body) = statement(&tokens[ptr..])?;
//     ptr += n;           // n = body
//
//     let body = match inc {
//         Some(inc) => Stmt::Block(vec![body, Stmt::Expression(inc)]),
//         None => body,
//     };
//
//     let body = Stmt::While(cond, body.into());
//
//     let body = match init {
//         Some(init) => Stmt::Block(vec![init, body]),
//         None => body,
//     };
//
//     Ok((ptr, body))
// }

// fn stmt_expr(tokens: &[Token]) -> Result<(usize, Stmt), Error> {
//     let mut ptr = 0;
//
//     let (n, expr) = expression(tokens)?;
//     ptr += n;           // n = expr
//
//     consume(tokens, ptr, TokenType::Semicolon, ErrorType::MissingSemicolon)?;
//     ptr += 1;           // 1 = ;
//
//     Ok((ptr, Stmt::Expression(expr)))
// }


fn expression(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    match tokens.first().map(|t| &t.ttype) {
        Some(TokenType::Data)    => expr_data(ctx, tokens),
        Some(TokenType::Match)   => expr_match(ctx, tokens),
        Some(TokenType::Let)     => expr_let(ctx, tokens),
        Some(TokenType::If)      => expr_if(ctx, tokens),
        Some(TokenType::While)   => expr_while(ctx, tokens),
        Some(TokenType::BraceL)  => expr_block(ctx, tokens),
        Some(TokenType::Return)  => expr_return(ctx, tokens),
        _ => expr_assign(ctx, tokens),
    }
}

fn expr_match(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;    // 1 "match"

    let (n, matched) = expression(ctx, &tokens[ptr..])?;
    ptr += n;           // n expression

    let mut items = vec![];
    while let Some(Token { ttype: TokenType::Pipe, .. }) = tokens.get(ptr) {
        ptr += 1;       // 1 "|"

        let (n, case) = expression(ctx, &tokens[ptr..])?;
        ptr += n;       // n expression

        let mut idents = vec![];
        while let Some(Token { ttype: TokenType::Ident(ref ident), .. }) = tokens.get(ptr) {
            ptr += 1;
            idents.push(ident.clone());
        }

        consume(tokens, ptr, TokenType::Arrow, ErrorType::ExprLeftover)?;
        ptr += 1;       // 1 "->"

        let (n, expr) = expression(ctx, &tokens[ptr..])?;
        ptr += n;       // n expression

        items.push((*case, *expr, idents.into()));
    }

    Ok((ptr, Expr::Match(matched, items.into()).into()))
}

fn expr_data(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;

    let name = consume_ident(tokens, ptr)?;
    ptr += 1;           // 1 = ident

    let mut items = vec![];
    while let Some(Token { ttype: TokenType::Pipe, .. }) = tokens.get(ptr) {
        ptr += 1;

        let item = consume_ident(tokens, ptr)?;
        ptr += 1;

        let mut fields = vec![];
        while let Some(Token { ttype: TokenType::Ident(field), .. }) = tokens.get(ptr) {
            ptr += 1;

            fields.push(field.to_owned())
        }

        items.push((item, fields.into()))
    }

    Ok((ptr, Expr::Data(name, items.into()).into()))
}

fn expr_let(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;    // 1 = var

    let ident = consume_ident(tokens, ptr)?;
    ptr += 1;           // 1 = ident

    consume(tokens, ptr, TokenType::Equal, ErrorType::MissingSemicolon)?;
    ptr += 1;           // 1 = ;

    let (n, expr) = expression(ctx, &tokens[ptr..])?;
    ptr += n;           // n = expr

    Ok((ptr, Expr::Let(ident, expr).into()))
}

fn expr_if(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;    // 1 = if

    let (n, cond) = expression(ctx, &tokens[ptr..])?;
    ptr += n;           // n = expr

    let (n, branch_t) = expression(ctx, &tokens[ptr..])?;
    ptr += n;           // n = true branch

    let branch_f = match matches(tokens.get(ptr), &[TokenType::Else]) {
        true => {
            ptr += 1;   // 1 =? else
            let (n, branch_f) = expression(ctx, &tokens[ptr..])?;
            ptr += n;   // n =? false branch
            Some(branch_f)
        },
        false => None,
    };

    Ok((ptr, Expr::If(cond, branch_t, branch_f).into()))
}

fn expr_while(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;    // 1 "while"
    //
    let (n, cond) = expression(ctx, &tokens[ptr..])?;
    ptr += n;           // n expr

    let (n, expr) = expression(ctx, &tokens[ptr..])?;
    ptr += n;           // n expr

    Ok((ptr, Expr::While(cond, expr).into()))
}

fn expr_return(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;    // 1 "return"

    let (n, expr) = match tokens.get(ptr) {
        Some(Token { ttype: TokenType::Semicolon, .. }) => (0, None),
        _ => {
            let (n, expr) = expression(ctx, &tokens[ptr..])?;
            (n, Some(expr))
        }
    };
    ptr += n;           // n =? expr

    Ok((ptr, Expr::Return(expr).into()))
}

fn expr_assign(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, expr) = pratt(ctx, tokens, 0)?;
    ptr += n;           // n = expr

    match tokens.get(n).map(|t| &t.ttype) {
        Some(TokenType::Equal) => {
            let (c_next, next) = expr_assign(ctx, &tokens[n + 1..])?;

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
fn call(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = index(ctx, tokens)?;
    ptr += n;       // n index

    while let Ok((n, arg)) = index(ctx, &tokens[ptr..]) {
        ptr += n;   // n arg
        expr = Box::new(Expr::Call(expr, arg));
    }

    Ok((ptr, expr))
}

fn index(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let (n, mut expr) = primary(ctx, tokens)?;
    ptr += n;       // n primary

    if let Some(Token { ttype: TokenType::Dot, .. }) = tokens.get(ptr) {
        ptr += 1;

        let next = consume_ident(tokens, ptr)?;
        ptr += 1;

        expr = Expr::Index(expr, next).into();
    }

    Ok((ptr, expr))
}

fn arguments(ctx: &Context, tokens: &[Token]) -> Result<(usize, Vec<Expr>), Error> {
    let mut ptr = 0;
    let mut args = vec![];

    // if no args bail out early
    if matches(tokens.get(ptr), &[TokenType::ParenR]) { return Ok((ptr, args)) }

    loop {
        let (n, expr) = expression(ctx, &tokens[ptr..])?;
        args.push(*expr);
        ptr += n;   // n =? expr

        match matches(tokens.get(ptr), &[TokenType::Comma]) {
            true => ptr += 1, // 1 =? ,
            false => break,
        }
    };

    Ok((ptr, args))
}

fn primary(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
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
        TT::Fun         => expr_lambda(ctx, tokens)?,
        TT::ParenL      => expr_parenthesized(ctx, tokens)?,
        TT::SquareL     => expr_array(ctx, tokens)?,
        tt => return Err(Error {
            ttype: ErrorType::InvalidToken(tt.to_owned()),
            line: next.line,
            offset: next.offset,
            length: next.length,
        }),
    };

    Ok((consumed, variant))
}

fn expr_parenthesized(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;        // 1 = (

    if let Some(Token { ttype: TokenType::ParenR, .. }) = tokens.get(ptr) {
        return Ok((2, Box::new(Expr::Literal(Literal::Nil))))
    }

    if let (
        Some(Token { ttype: TokenType::Op(ref op), .. }),
        Some(Token { ttype: TokenType::ParenR, .. })
    ) = (tokens.get(ptr), tokens.get(ptr+1)) {
        ptr += 2;
        return Ok((ptr, Expr::Variable(Ident(op.into())).into()))
    }

    let (n, expr) = expression(ctx, &tokens[ptr..])?;
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
            let (n, next_expr) = expression(ctx, &tokens[ptr..])?;
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

fn expr_array(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;        // [

    let (n, arr) = arguments(ctx, &tokens[ptr..])?;
    ptr += n;

    consume(tokens, ptr, TokenType::SquareR, ErrorType::MissingParenR)?;
    ptr += 1;

    Ok((ptr, Expr::Array(arr).into()))
}


fn expr_lambda(ctx: &Context, tokens: &[Token]) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 1;        // 1 fn
    let mut args = vec![];

    loop {
        let expr = consume_ident(tokens, ptr)?;
        ptr += 1;           // 1 IDENTIFIER
        args.push(expr);

        match tokens.get(ptr) {
            Some(Token { ttype: TokenType::Arrow, .. }) => {
                ptr += 1;   // 1 ->
                break;
            },
            _ => continue,
        }
    }

    let (n, expr) = expression(ctx, &tokens[ptr..])?;
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

fn pratt(ctx: &Context, tokens: &[Token], min_p: usize) -> Result<(usize, Box<Expr>), Error> {
    let mut ptr = 0;

    let mut expr_l = match tokens[ptr] {
        Token { ttype: TokenType::Op(ref op), .. } => {
            let (_, bp_r) = (100, 100); // hardcoded for now
            ptr += 1;

            let (n, expr_r) = pratt(ctx, &tokens[ptr..], bp_r)?;
            ptr += n;

            Box::new(Expr::Unary(op.into(), expr_r))
        }
        _ => {
            let (n, expr_l) = call(ctx, tokens)?;
            ptr += n;
            expr_l
        },
    };

    while let Some(TokenType::Op(ref op)) = tokens.get(ptr).map(|t| &t.ttype) {
        let (bp_l, bp_r) = ctx.get_precedence(op);

        if bp_l < min_p {
            break;
        }

        ptr += 1;

        let (n, expr_r) = pratt(ctx, &tokens[ptr..], bp_r)?;
        ptr += n;

        expr_l = Box::new(Expr::Binary(expr_l, op.into(), expr_r));
    };

    Ok((ptr, expr_l))
}
