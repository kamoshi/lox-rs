use std::borrow::Cow;

use crate::error::LoxError;


pub enum ErrorType {
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
        let line = self.line + 1;
        let offset = self.offset + 1;

        use ErrorType::*;
        let message: Cow<str> = match self.ttype {
            UnterminatedString      => "Unterminated string".into(),
            MalformedNumber         => "Couldn't parse number".into(),
        };

        eprintln!("Error diagnostics:\nL{line}:{offset} {message}");
    }

    fn report_rich(&self, source: &str) {
        self.report();

        let line_str = source.lines().nth(self.line).unwrap();
        let marker = (0..self.offset).map(|_| ' ').collect::<String>();
        eprintln!("{line_str}\n{marker}^");
    }
}
