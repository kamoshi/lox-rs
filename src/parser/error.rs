use crate::{error::LoxError, lexer::token_type::TokenType};


pub enum ErrorType {
    MissingRightParen,
    InvalidToken(TokenType),
    MissingSemicolon,
    ExprLeftover,
}

pub struct Error {
    pub ttype: ErrorType,
    pub line: Option<usize>,
}

impl LoxError for Error {
    fn report(&self) {
        use ErrorType::*;
        let message = match &self.ttype {
            MissingRightParen   => "Missing right parenthesis".into(),
            InvalidToken(token) => format!("Invalid token found {token}"),
            MissingSemicolon    => "Missing semicolon after statement".into(),
            ExprLeftover        => "Invalid tokens after expression".into(),
        };

        match self.line {
            Some(line)  => eprintln!("L{line}: {message}"),
            None        => eprintln!("{message}"),
        };
    }
}
