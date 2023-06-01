#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Token {
    pub typ: TokenType,
    pub pos: usize,
    pub len: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

    // Identifier
    Identifier,
    // EOF, // unnecessary token?
}


