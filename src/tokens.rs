use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub pos: usize,
    pub len: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Slash,

    // Comparators
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    BangEqual,
    Bang,
    EqualEqual,
    Equal,

    // Keywords
    And,
    Else,
    False,
    Fun,
    If,
    Let,
    Or,
    Return,
    True,

    // Number
    Number(i64),

    // String
    String(String),

    // Identifier
    Identifier(String),

    // Error
    Error,

    // EoF
    EoF,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let TokenType::Number(num) = &self {
            return write!(f, "{}", num)
        }

        write!(f, "{}", match self {
            TokenType::LeftParen     => "(",
            TokenType::RightParen    => ")",
            TokenType::LeftBrace     => "{",
            TokenType::RightBrace    => "}",
            TokenType::Comma         => ",",
            TokenType::Dot           => ".",
            TokenType::Minus         => "-",
            TokenType::Plus          => "+",
            TokenType::Semicolon     => ";",
            TokenType::Star          => "*",
            TokenType::Slash         => "/",
            TokenType::GreaterEqual  => ">=",
            TokenType::Greater       => ">",
            TokenType::LessEqual     => "<=",
            TokenType::Less          => "<",
            TokenType::BangEqual     => "!=",
            TokenType::Bang          => "!",
            TokenType::EqualEqual    => "==",
            TokenType::Equal         => "=",
            TokenType::And           => "and",
            TokenType::Else          => "else",
            TokenType::False         => "false",
            TokenType::Fun           => "fn",
            TokenType::If            => "if",
            TokenType::Let           => "let",
            TokenType::Or            => "or",
            TokenType::Return        => "return",
            TokenType::True          => "true",
            TokenType::String(s)     => s,
            TokenType::Number(_)     => unreachable!(),
            TokenType::Identifier(d) => d,
            TokenType::Error         => "ERROR",
            TokenType::EoF           => "EoF",
        })
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.typ)
    }
}

