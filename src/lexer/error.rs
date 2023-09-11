use crate::error::LoxError;


pub enum ErrorType {
    InvalidCharacter(char),
    UnterminatedString,
    MalformedNumber,
}

pub struct Error {
    pub ttype: ErrorType,
    pub line: usize,
    pub offset: usize,
}

impl LoxError for Error {
    fn report(&self) {
        let (line, offset) = (self.line, self.offset);

        use ErrorType::*;
        let message = match self.ttype {
            InvalidCharacter(char) => format!("{line}:{offset} Invalid character '{char}'"),
            UnterminatedString => format!("{line}:{offset} Unterminated string"),
            MalformedNumber => format!("{line}:{offset} Couldn't parse number"),
        };

        eprintln!("{}", message);
    }
}

