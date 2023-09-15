use std::fmt::Display;
use super::token_type::TokenType;


#[derive(Debug)]
pub struct Token<'src> {
    pub ttype: TokenType,
    pub line_str: &'src str,
    pub line: usize,
    pub offset: usize,
    pub length: usize,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token_type = &self.ttype;
        let line = self.line;
        let start = self.offset;
        let end = self.offset + self.length;
        write!(f, "({token_type} {line}:{start}-{end})")
    }
}
