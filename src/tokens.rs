use std::cmp::Ordering;

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
    Minus,
    Plus,
    Slash,
    Star,

    // Number
    Number,
    // EOF, // unnecessary token?
}

impl TokenType {
    fn precedence(&self) -> u8 {
        #[rustfmt::skip]
        return match &self {
            TokenType::LeftParen  => 4,
            TokenType::RightParen => 0,
            TokenType::Minus      => 2,
            TokenType::Plus       => 2,
            TokenType::Slash      => 3,
            TokenType::Star       => 3,
            TokenType::Number     => 9,
        };
    }
}

impl PartialOrd for TokenType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.precedence().cmp(&other.precedence()))
    }
}

impl Ord for TokenType {
    fn cmp(&self, other: &Self) -> Ordering {
        TokenType::partial_cmp(&self, other).unwrap()
    }
}

