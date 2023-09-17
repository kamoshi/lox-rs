use std::borrow::Cow;
use crate::lexer::token_type::TokenType;
use crate::error::LoxError;


pub enum ErrorType {
    MissingParenR,
    InvalidToken(TokenType),
    MissingSemicolon,
    ExprLeftover,
    ExpectedIdent,
    AssignmentTarget,
    MissingBrace,
    IfMissingParenL,
}

pub struct Error<'src> {
    pub ttype: ErrorType,
    pub line_str: &'src str,
    pub line: usize,
    pub offset: usize,
    pub length: usize,
}

impl LoxError for Error<'_> {
    fn report(&self) {
        use ErrorType::*;
        let message: Cow<str> = match &self.ttype {
            MissingParenR       => "Missing right parenthesis".into(),
            InvalidToken(token) => format!("Invalid token {token}").into(),
            MissingSemicolon    => "Missing semicolon after statement".into(),
            ExprLeftover        => "Invalid tokens after expression".into(),
            ExpectedIdent       => "Expected identifier".into(),
            AssignmentTarget    => "Invalid assignment target".into(),
            MissingBrace        => "Missing brace after block".into(),
            IfMissingParenL     => "Left parenthesis is expected after if".into(),
        };

        let line_str = self.line_str;
        let marker = (0..self.offset).map(|_| ' ').collect::<String>();
        let arrows = (0..self.length).map(|_| '^').collect::<String>();
        let line = self.line + 1;
        let offset = self.offset + 1;
        let range = match self.length {
            1 => offset.to_string(),
            _ => format!("{offset}-{}", offset + self.length - 1),
        };
        eprintln!("Error diagnostics:\n{line_str}\n{marker}{arrows}\nL{line}:{range} {message}");
    }
}
