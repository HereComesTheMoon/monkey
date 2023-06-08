use std::fmt::Display;
use crate::tokens::{Token, TokenType};
use crate::parser::Expr;

#[derive(Debug)]
pub struct Error(pub(crate) (String, Token));

#[derive(Debug)]
pub struct ErrorLocation {
    pub source: String,
    pub err: Error,
    pub line: usize,
    pub col: usize,
}

impl From<(TokenType, Token)> for Error {
    fn from(value: (TokenType, Token)) -> Self {
        Error((value.0.to_string(), value.1))
    }
}

impl From<Error> for Expr {
    fn from(value: Error) -> Self {
        Expr::Error(Error((value.0.0, value.0.1)))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ERROR[{},{}]", self.0.0, self.0.1)
    }
}


pub fn locate_error(source: &str, err: Error) -> ErrorLocation {
    // let pos = err.0.1.pos;
    // let len = err.0.1.len;
    // let t = Tokenizer::new(source[pos..pos+len].into()).next().typ;
    // assert_eq!(t, err.0.1.typ);
    let mut line = 0;
    let mut col = 0;
    for (k, c) in source.char_indices() {
        col += 1;
        if c == '\n' {
            line += 1;
            col = 0;
        }
        if err.0.1.pos <= k {
            break
        }
    }
    ErrorLocation { source: source.to_owned(), err, line, col }
}

pub fn format_error(err: ErrorLocation) -> String {
    let ErrorLocation { source, err, line, col } = err;
    let Error((expected, token)) = err;

    let left = source[..token.pos].trim_end().rfind('\n').unwrap_or(0);
    let right = source[token.pos..].find('\n').unwrap_or(source.len());
    let context = &source[left..left+right+2];
    format!("Error found in line {line}, column {col}. Error: Expected {expected}, got {token}! Context: \n\n{context}\n")
}

