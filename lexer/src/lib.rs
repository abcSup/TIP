use miette::{Diagnostic, LabeledSpan};
use std::vec;
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
#[error("Unexpected EOF")]
pub struct UnexpectedEof;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<'de> {
    pub origin: &'de str,
    pub offset: usize,
    pub kind: TokenKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    If,
    Else,
    While,
    Return,
    Var,
    Input,
    Output,
    Alloc,
    Null,
    Error,
    Ident,
    Number(u64),
    LParen,
    RParen,
    LCurly,
    RCurly,
    Semicolon,
    Colon,
    Dot,
    Comma,
    Equal,
    EqualEqual,
    Greater,
    Plus,
    Minus,
    Star,
    Ampersand,
    Slash,
}

pub struct Lexer<'de> {
    pub whole: &'de str,
    pub rest: &'de str,
    pub offset: usize,
    peeked: Option<Token<'de>>,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            offset: 0,
            peeked: None,
        }
    }

    pub fn expect(&mut self, kind: TokenKind) -> Result<Token<'de>, miette::Error> {
        match self.next() {
            Some(token) if token.kind == kind => Ok(token),
            Some(token) => Err(miette::miette!(
                labels = vec![LabeledSpan::at(
                    token.offset..token.offset + token.origin.len(),
                    "here"
                )],
                help = format!("Expected {:?}, but found {:?}", kind, token.kind),
                "Unexpected Token"
            )
            .with_source_code(self.whole.to_string())),
            None => Err(UnexpectedEof.into()),
        }
    }

    pub fn peek(&mut self) -> Option<&Token<'de>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }
        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Token<'de>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }

        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_onwards = self.rest;

            let cur_origin = &self.rest[..c.len_utf8()];
            let cur_offset = self.offset;

            self.rest = chars.as_str();
            self.offset += c.len_utf8();

            let wrap_cur = move |kind: TokenKind| {
                Some(Token {
                    kind,
                    offset: cur_offset,
                    origin: cur_origin,
                })
            };

            match c {
                '(' => return wrap_cur(TokenKind::LParen),
                ')' => return wrap_cur(TokenKind::RParen),
                '{' => return wrap_cur(TokenKind::LCurly),
                '}' => return wrap_cur(TokenKind::RCurly),
                ';' => return wrap_cur(TokenKind::Semicolon),
                ':' => return wrap_cur(TokenKind::Colon),
                '.' => return wrap_cur(TokenKind::Dot),
                ',' => return wrap_cur(TokenKind::Comma),
                '>' => return wrap_cur(TokenKind::Greater),
                '+' => return wrap_cur(TokenKind::Plus),
                '-' => return wrap_cur(TokenKind::Minus),
                '*' => return wrap_cur(TokenKind::Star),
                '&' => return wrap_cur(TokenKind::Ampersand),
                '=' => {
                    if self.rest.starts_with("=") {
                        let span = &c_onwards[..c.len_utf8() + 1];
                        self.rest = &self.rest[1..];
                        self.offset += 1;

                        return Some(Token {
                            kind: TokenKind::EqualEqual,
                            offset: cur_offset,
                            origin: span,
                        });
                    }
                    return wrap_cur(TokenKind::Equal)
                }
                '/' => {
                    if self.rest.starts_with("/") {
                        // Skip comments
                        let end_offset = self.rest.find('\n').unwrap();

                        self.rest = &self.rest[end_offset..];
                        self.offset += end_offset - c.len_utf8();

                        continue;
                    } else if self.rest.starts_with("*") {
                        // Skip block comments
                        // TODO: 2 is not the actual length in UTF8
                        let end_offset = self.rest.find("*/").unwrap() + 2;

                        self.rest = &self.rest[end_offset..];
                        self.offset += end_offset - 2;

                        continue;
                    }
                    return wrap_cur(TokenKind::Slash);
                }
                '0'..='9' => {
                    let end_offset = c_onwards.find(|c: char| !c.is_ascii_digit()).unwrap();
                    let literal = &c_onwards[..end_offset];

                    let skip = literal.len() - c.len_utf8();
                    self.rest = &self.rest[skip..];
                    self.offset += skip;

                    let number = match literal.parse() {
                        Ok(n) => n,
                        Err(_) => panic!("Invalid number literal: {}", literal),
                    };

                    return Some(Token {
                        kind: TokenKind::Number(number),
                        offset: cur_offset,
                        origin: literal,
                    });
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let end_offset = c_onwards
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap();
                    let literal = &c_onwards[..end_offset];

                    let skip = literal.len() - c.len_utf8();
                    self.rest = &self.rest[skip..];
                    self.offset += skip;

                    let kind = match literal {
                        "if" => TokenKind::If,
                        "else" => TokenKind::Else,
                        "while" => TokenKind::While,
                        "return" => TokenKind::Return,
                        "var" => TokenKind::Var,
                        "input" => TokenKind::Input,
                        "output" => TokenKind::Output,
                        "alloc" => TokenKind::Alloc,
                        "null" => TokenKind::Null,
                        "error" => TokenKind::Error,
                        _ => TokenKind::Ident,
                    };

                    return Some(Token {
                        kind,
                        offset: cur_offset,
                        origin: literal,
                    });
                }
                c if c.is_whitespace() => continue,
                _ => {
                    eprintln!("Error: unknown character '{c}'");
                    panic!()
                }
            }
        }
    }
}
