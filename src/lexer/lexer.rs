use crate::lexer::{token::Token, token_type::TokenType};
use super::error::{Error, ErrorType};


pub fn tokenize(source: &str) -> Result<Vec<Token>, Error> {
    let mut tokens = vec![];

    for (line, line_str) in source.lines().enumerate() {
        let chars = line_str.chars().collect::<Vec<_>>();
        let mut offset = 0;

        while offset < chars.len() {
            let (length, token) = match read_token(&chars[offset..]) {
                Ok(lexed) => lexed,
                Err(ttype) => return Err(Error { ttype, line_str, line, offset }),
            };

            if let Some(ttype) = token {
                let lexeme = String::from_iter(&chars[offset..offset+length]);
                tokens.push(Token { ttype, lexeme, line, offset, length });
            }

            offset += length;
        }
    }

    tokens.push(Token {
        ttype: TokenType::Eof,
        lexeme: String::new(),
        line: tokens.last().map(|t| t.line + 1).unwrap_or(1),
        offset: 0,
        length: 0
    });

    Ok(tokens)
}

fn read_token(
    chars: &[char]
) -> Result<(usize, Option<TokenType>), ErrorType> {
    let curr = chars[0];
    let next = chars.get(1);
    match curr {
        // single char
        '(' | ')' | '{' | '}' | ',' | '.' | '-' | '+' | ';' | '*' => {
            let ttype = match curr {
                '(' => TokenType::ParenL,
                ')' => TokenType::ParenR,
                '{' => TokenType::BraceL,
                '}' => TokenType::BraceR,
                ',' => TokenType::Comma,
                '.' => TokenType::Dot,
                '-' => TokenType::Minus,
                '+' => TokenType::Plus,
                ';' => TokenType::Semicolon,
                '*' => TokenType::Star,
                _ => unreachable!(),
            };
            Ok((1, Some(ttype)))
        }
        // comparison
        '!' => match next {
            Some('=') => Ok((2, Some(TokenType::BangEqual))),
            _ => Ok((1, Some(TokenType::Bang))),
        },
        '=' => match next {
            Some('=') => Ok((2, Some(TokenType::EqualEqual))),
            _ => Ok((1, Some(TokenType::Equal))),
        },
        '<' => match next {
            Some('=') => Ok((2, Some(TokenType::LessEqual))),
            _ => Ok((1, Some(TokenType::Less))),
        },
        '>' => match next {
            Some('=') => Ok((2, Some(TokenType::GreaterEqual))),
            _ => Ok((1, Some(TokenType::Greater))),
        },
        // div | comment
        '/' => match next {
            Some('/') => {
                let consumed = chars.iter()
                    .position(|&c| c == '\n')
                    .unwrap_or(chars.len());
                Ok((consumed, None))
            },
            _ => Ok((1, Some(TokenType::Slash))),
        }
        // whitespace
        ' ' | '\r' | '\t' => Ok((1, None)),
        // strings
        '"' => {
            let consumed = chars[1..].iter()
                .position(|&c| c == '"')
                .map(|n| n + 2);
            let consumed = match consumed {
                Some(n) => n,
                None => return Err(ErrorType::UnterminatedString),
            };
            let literal = String::from_iter(&chars[1..consumed-1]);
            Ok((consumed, Some(TokenType::Str(literal))))
        },
        // numbers
        '0'..='9' => {
            let maybe_dot = chars.iter()
                .position(|c| !c.is_numeric())
                .unwrap_or(chars.len());

            let peek = (chars.get(maybe_dot), chars.get(maybe_dot+1));
            let end = match peek {
                (Some('.'), Some('0'..='9')) => {
                    let end = chars[maybe_dot+1..].iter()
                        .position(|c| !c.is_numeric())
                        .unwrap_or(chars.len());
                    maybe_dot + 1 + end
                },
                _ => maybe_dot,
            };

            let literal = String::from_iter(&chars[..end]).parse::<f64>();
            let literal = match literal {
                Ok(n) => n,
                Err(_) => return Err(ErrorType::MalformedNumber),
            };
            Ok((end, Some(TokenType::Num(literal))))
        },
        // identifiers
        'a'..='z' | 'A'..='Z' | '_' => {
            let consumed = chars.iter()
                .position(|&c| !c.is_alphanumeric() && c != '_')
                .unwrap_or(chars.len());
            let token_type = match String::from_iter(&chars[..consumed]).as_str() {
                "and"   => TokenType::And,
                "class" => TokenType::Class,
                "else"  => TokenType::Else,
                "false" => TokenType::False,
                "for"   => TokenType::For,
                "fun"   => TokenType::Fun,
                "if"    => TokenType::If,
                "nil"   => TokenType::Nil,
                "or"    => TokenType::Or,
                "print" => TokenType::Print,
                "return"=> TokenType::Return,
                "super" => TokenType::Super,
                "this"  => TokenType::This,
                "true"  => TokenType::True,
                "var"   => TokenType::Var,
                "while" => TokenType::While,
                other   => TokenType::Ident(String::from(other)),
            };
            Ok((consumed, Some(token_type)))
        },
        // other
        _ => Err(ErrorType::InvalidCharacter(curr)),
    }
}
