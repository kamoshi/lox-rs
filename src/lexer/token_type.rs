use std::{fmt::Display, borrow::Cow};


#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {

    // Single-character tokens
    ParenL, ParenR, BraceL, BraceR, SquareL, SquareR,
    Comma, Dot, Semicolon, Slash, Star,

    // One or two character tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Op(String),

    // Literals
    Ident(String), Str(String), Num(f64),

    // Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Return, Super, This, True, Var, While,
    Typeclass, Instance, Variant,

    Infixr, Infixl,

    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenType::*;
        let str: Cow<str> = match self {
            ParenL          => "paren-l".into(),
            ParenR          => "paren-r".into(),
            BraceL          => "brace-l".into(),
            BraceR          => "brace-r".into(),
            SquareL         => "square-l".into(),
            SquareR         => "square-r".into(),
            Comma           => "comma".into(),
            Dot             => "dot".into(),
            Minus           => "minus".into(),
            Plus            => "plus".into(),
            Semicolon       => "semicolon".into(),
            Slash           => "slash".into(),
            Star            => "star".into(),
            Bang            => "bang".into(),
            BangEqual       => "bang-equal".into(),
            Equal           => "equal".into(),
            EqualEqual      => "equal-equal".into(),
            Greater         => "greater".into(),
            GreaterEqual    => "greater-equal".into(),
            Less            => "less".into(),
            LessEqual       => "less-equal".into(),
            Ident(ident)    => format!("ident <{ident}>").into(),
            Str(str)        => format!("str <{str}>").into(),
            Num(num)        => format!("num <{num}>").into(),
            And             => "and".into(),
            Class           => "class".into(),
            Else            => "else".into(),
            False           => "false".into(),
            Fun             => "fun".into(),
            For             => "for".into(),
            If              => "if".into(),
            Nil             => "nil".into(),
            Or              => "or".into(),
            Return          => "return".into(),
            Super           => "super".into(),
            This            => "this".into(),
            True            => "true".into(),
            Var             => "var".into(),
            While           => "while".into(),
            Eof             => "EOF".into(),
            Typeclass       => "typeclass".into(),
            Instance        => "instance".into(),
            Variant         => "variant".into(),

            Op(op) => format!("op <{op}>").into(),
            Infixl => "infixl".into(),
            Infixr => "infixr".into(),
        };

        write!(f, "{str}")
    }
}
