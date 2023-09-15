use std::fmt::Display;
use super::token_type::TokenType;


#[derive(Debug)]
pub struct Token {
    pub ttype: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub offset: usize,
    pub length: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token_type = &self.ttype;
        let lexeme = &self.lexeme;
        let line = self.line;
        let start = self.offset;
        let end = self.offset + self.length;
        write!(f, "({token_type} '{lexeme}' {line}:{start}-{end})")
    }
}
