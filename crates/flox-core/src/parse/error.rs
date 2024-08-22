use std::borrow::Cow;

use crate::lex::token_type::TokenType;
use crate::error::FloxError;

#[derive(Debug)]
pub enum ParserErrorKind {
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
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub line: usize,
    pub offset: usize,
    pub length: usize,
}

impl FloxError for ParserError {
    fn report(&self) -> String {
        let line = self.line + 1;
        let offset = self.offset + 1;
        let range = match self.length {
            1 => offset.to_string(),
            _ => format!("{offset}-{}", offset + self.length - 1),
        };

        use ParserErrorKind::*;
        let message: Cow<str> = match &self.kind {
            MissingParenL       => "Left parenthesis is expected".into(),
            MissingParenR       => "Right parenthesis is expected".into(),
            InvalidToken(token) => format!("Invalid token {token}").into(),
            MissingSemicolon    => "Missing semicolon after statement".into(),
            ExprLeftover        => "Invalid tokens after expression".into(),
            ExpectedIdent       => "Expected identifier".into(),
            AssignmentTarget    => "Invalid assignment target".into(),
            MissingBrace        => "Missing brace after block".into(),
        };

        format!("Error diagnostics:\nL{line}:{range} {message}")
    }

    fn report_rich(&self, source: &str) -> String {
        self.report();

        let line_str = source.lines().nth(self.line).unwrap();
        let marker = (0..self.offset).map(|_| ' ').collect::<String>();
        let arrows = (0..self.length).map(|_| '^').collect::<String>();
        format!("{line_str}\n{marker}{arrows}")
    }
}
