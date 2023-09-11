use crate::lexer::{token::Token, token_type::TokenType};
use super::error::{Error, ErrorType};


pub(crate) fn scan_tokens(source: &str) -> Result<Vec<Token>, Error> {
    let chars: Vec<char> = source.chars().collect();
    let mut tokens: Vec<Token> = vec![];
    let mut current = 0;
    let mut line = 1;
    let mut offset = 0;

    while current < chars.len() {
        let (length, new_line, token) = match scan_token(&chars[current..]) {
            Ok(scanned) => scanned,
            Err(ttype) => return Err(Error { ttype, line, offset }),
        };

        if let Some(token) = token {
            tokens.push(Token {
                ttype: token,
                lexeme: String::from_iter(&chars[current..current+length]),
                line,
                offset,
                length,
            });
        }

        current += length;
        match new_line {
            true => {
                line += 1;
                offset = 0;
            },
            false => {
                offset += length;
            },
        }
    }

    tokens.push(Token {
        ttype: TokenType::Eof,
        lexeme: String::new(),
        line,
        offset,
        length: 0
    });

    Ok(tokens)
}

fn scan_token(
    chars: &[char]
) -> Result<(usize, bool, Option<TokenType>), ErrorType> {
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
            Ok((1, false, Some(ttype)))
        }
        // comparison
        '!' => match next {
            Some('=') => Ok((2, false, Some(TokenType::BangEqual))),
            _ => Ok((1, false, Some(TokenType::Bang))),
        },
        '=' => match next {
            Some('=') => Ok((2, false, Some(TokenType::EqualEqual))),
            _ => Ok((1, false, Some(TokenType::Equal))),
        },
        '<' => match next {
            Some('=') => Ok((2, false, Some(TokenType::LessEqual))),
            _ => Ok((1, false, Some(TokenType::Less))),
        },
        '>' => match next {
            Some('=') => Ok((2, false, Some(TokenType::GreaterEqual))),
            _ => Ok((1, false, Some(TokenType::Greater))),
        },
        // div | comment
        '/' => match next {
            Some('/') => {
                let consumed = chars.iter()
                    .position(|&c| c == '\n')
                    .unwrap_or(chars.len());
                Ok((consumed, false, None))
            },
            _ => Ok((1, false, Some(TokenType::Slash))),
        }
        // whitespace
        ' ' | '\r' | '\t' => Ok((1, false, None)),
        // newline
        '\n' => Ok((1, true, None)),
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
            Ok((consumed, false, Some(TokenType::Str(literal))))
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
            Ok((end, false, Some(TokenType::Num(literal))))
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
            Ok((consumed, false, Some(token_type)))
        },
        // other
        _ => Err(ErrorType::InvalidCharacter(curr)),
    }
}
