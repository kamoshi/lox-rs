use std::cell::RefCell;

use flox_core::error::FloxError;
use flox_core::rt::env::Env;
use wasm_bindgen::prelude::*;
use flox_core::parse::parser::{parse, Context};
use flox_core::lex::tokenize;
use flox_core::rt::eval_expr;

#[wasm_bindgen]
pub fn eval_expression(source: &str) -> String {
    let tokens = match tokenize(source) {
        Ok(tokens) => tokens,
        Err(err) => return err.report_rich(source),
    };

    let (_, ast) = match parse(&Context::default(), &tokens) {
        Ok(ast) => ast,
        Err(err) => return err.report_rich(source),
    };

    let res = match eval_expr(RefCell::new(Env::new()).into(), &ast) {
        Ok(res) => res,
        Err(err) => return err.report_rich(source),
    };

    res.to_string()
}
