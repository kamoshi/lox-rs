use std::cell::RefCell;

use flox_core::rt::env::Env;
use wasm_bindgen::prelude::*;
use flox_core::parse::parser::{parse, Context};
use flox_core::lex::tokenize;
use flox_core::rt::eval_expr;

#[wasm_bindgen]
pub fn eval_expression(code: &str) -> String {
    let tokens = match tokenize(code) {
        Ok(tokens) => tokens,
        Err(_) => return "fail".into(),
    };

    let (_, ast) = match parse(&Context::default(), &tokens) {
        Ok(ast) => ast,
        Err(_) => return "fail".into(),
    };

    let res = match eval_expr(RefCell::new(Env::new()).into(), &ast) {
        Ok(res) => res,
        Err(_) => return "fail".into(),
    };

    res.to_string()
}
