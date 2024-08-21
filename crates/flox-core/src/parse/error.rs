use std::borrow::Cow;

use crate::lex::token_type::TokenType;
use crate::error::LoxError;

#[derive(Debug)]
pub enum ErrorType {
    MissingParenR,
    MissingParenL,
    InvalidToken(TokenType),
    MissingSemicolon,
    ExprLeftover,
    ExpectedIdent,
    AssignmentTarget,
    MissingBrace,
}

#[derive(Debug)]
pub struct Error {
    pub ttype: ErrorType,
    pub line: usize,
    pub offset: usize,
    pub length: usize,
}

impl LoxError for Error {
    fn report(&self) {
        let line = self.line + 1;
        let offset = self.offset + 1;
        let range = match self.length {
            1 => offset.to_string(),
            _ => format!("{offset}-{}", offset + self.length - 1),
        };

        use ErrorType::*;
        let message: Cow<str> = match &self.ttype {
            MissingParenL       => "Left parenthesis is expected".into(),
            MissingParenR       => "Right parenthesis is expected".into(),
            InvalidToken(token) => format!("Invalid token {token}").into(),
            MissingSemicolon    => "Missing semicolon after statement".into(),
            ExprLeftover        => "Invalid tokens after expression".into(),
            ExpectedIdent       => "Expected identifier".into(),
            AssignmentTarget    => "Invalid assignment target".into(),
            MissingBrace        => "Missing brace after block".into(),
        };

        eprintln!("Error diagnostics:\nL{line}:{range} {message}");
    }

    fn report_rich(&self, source: &str) {
        self.report();

        let line_str = source.lines().nth(self.line).unwrap();
        let marker = (0..self.offset).map(|_| ' ').collect::<String>();
        let arrows = (0..self.length).map(|_| '^').collect::<String>();
        eprintln!("{line_str}\n{marker}{arrows}");
    }
}
