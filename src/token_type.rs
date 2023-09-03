use std::{fmt::Display, borrow::Cow};

#[derive(Debug)]
pub enum TokenType {

    // Single-character tokens
    ParenL, ParenR, BraceL, BraceR,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals
    Ident, Str(String), Num(f64),

    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,

    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenType::*;
        let str = match self {
            ParenL          => "paren-l",
            ParenR          => "paren-r",
            BraceL          => "brace-l",
            BraceR          => "brace-r",
            Comma           => "comma",
            Dot             => "dot",
            Minus           => "minus",
            Plus            => "plus",
            Semicolon       => "semicolon",
            Slash           => "slash",
            Star            => "star",
            Bang            => "bang",
            BangEqual       => "bang-equal",
            Equal           => "equal",
            EqualEqual      => "equal-equal",
            Greater         => "greater",
            GreaterEqual    => "greater-equal",
            Less            => "less",
            LessEqual       => "less-equal",
            Ident           => "identifier",
            Str(_)          => "string",
            Num(_)          => "number ",
            And             => "and",
            Class           => "class",
            Else            => "else",
            False           => "false",
            Fun             => "fun",
            For             => "for",
            If              => "if",
            Nil             => "nil",
            Or              => "or",
            Print           => "print",
            Return          => "return",
            Super           => "super",
            This            => "this",
            True            => "true",
            Var             => "var",
            While           => "while",
            Eof             => "EOF",
        };
        write!(f, "{}", str)
    }
}
