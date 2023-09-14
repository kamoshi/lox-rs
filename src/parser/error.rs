use crate::{error::LoxError, lexer::token_type::TokenType};


pub enum ErrorType {
    MissingRightParen,
    InvalidToken(TokenType),
    MissingSemicolon,
    ExprLeftover,
}

pub struct Error {
    pub ttype: ErrorType,
}

impl LoxError for Error {
    fn report(&self) {
        use ErrorType::*;
        let message = match &self.ttype {
            MissingRightParen => format!("Missing right parenthesis"),
            InvalidToken(token) => format!("Invalid token found {token}"),
            MissingSemicolon => format!("Missing semicolon after statement"),
            ExprLeftover => format!("Invalid tokens after expression"),
        };

        eprintln!("{message}");
    }
}
