use crate::error::LoxError;


pub enum ErrorType {
    MissingRightParen,
}

pub struct Error {
    pub ttype: ErrorType,
}

impl LoxError for Error {
    fn report(&self) {
        let message = match self.ttype {
            ErrorType::MissingRightParen => format!("Missing right parenthesis"),
        };

        eprintln!("{message}");
    }
}
