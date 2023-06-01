mod tokenizer;
mod tokens;
use std::env;

use std::fmt::Display;
use std::fs;
use std::io;
use std::io::{BufRead, Write};
use std::process;
use tokenizer::Tokenizer;
use tokens::{Token, TokenType};

fn main() {
    println!("Hello, world!");
    let args: Vec<_> = env::args().into_iter().skip(1).collect();
    println!("{:?}", args);
    match args.len() {
        0 => run_prompt(),
        1 => run_file(&args[0]),
        _ => {
            println!("Bad arguments! {:?}", args);
            process::exit(64);
        }
    }
}

fn run_file(file: &str) {
    let content = fs::read_to_string(file).unwrap();
    println!("Interpreting: {}\n", content);
    run(content);
}

fn run_prompt() {
    let stdin = io::stdin();

    print!("> ");
    io::stdout().flush().unwrap();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        run(line);
        print!("\n> ");
        io::stdout().flush().unwrap();
    }
}

fn run(source: String) -> Expr {
    // let sc = Scanner::new(source);
    let mut parser = Parser::new(source);
    parser.parse()
}

struct Parser {
    source: String,
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(source: String) -> Self {
        let tokens = Tokenizer::new(&source).get_tokens();
        Parser {
            source,
            tokens,
            pos: 0,
        }
    }
}

impl Parser {
    pub fn parse(&mut self) -> Expr {
        self.parse_bp(0)
    }

    fn parse_bp(&mut self, min_bp: u8) -> Expr {
        println!("{:?} {:?}", self.pos, self.tokens[self.pos]);
        let mut lhs = match self.tokens[self.pos].typ {
            TokenType::Number(num) => Expr::Literal(num), // Add Minus and unary expressions
            _ => panic!("Oh no???"),
        };

        self.pos += 1;

        loop {
            if self.tokens.len() <= self.pos {
                break
            }
            println!("{:?} {:?}", self.pos, self.tokens[self.pos]);
            let op = match &self.tokens[self.pos].typ {
                TokenType::Minus => BinaryType::Minus,
                TokenType::Plus  => BinaryType::Plus,
                TokenType::Slash => BinaryType::Slash,
                TokenType::Star  => BinaryType::Star,
                // TokenType::LeftParen  => todo!(),
                // TokenType::RightParen => todo!(),
                // TokenType::Minus      => todo!(),
                // TokenType::Plus       => todo!(),
                // TokenType::Slash      => todo!(),
                // TokenType::Star       => todo!(),
                // TokenType::Number     => todo!(),
                t                        => todo!("{t:?}"),
            };

            let (l_bp, r_bp) = op.binding_power();

            if l_bp < min_bp {
                break
            }

            self.pos += 1;

            let rhs = self.parse_bp(r_bp);

            lhs = Expr::Binary(BinaryExpr {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            });
        }

        lhs
    }

    fn parse_number(&self) -> Expr {
        let t = &self.tokens[self.pos];
        let int = self.source[t.pos..t.pos+t.len].parse();
        if let Ok(int) = int {
            Expr::Literal(int)
        } else {
            println!("Oh no!");
            Expr::Error
        }
    }
}


enum Expr {
    Literal(i64),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Grouping(Box<Expr>),
    Error,
}

struct BinaryExpr {
    left: Box<Expr>,
    op: BinaryType,
    right: Box<Expr>,
}

enum BinaryType {
    Minus,
    Plus,
    Slash,
    Star,
}

struct UnaryExpr {
    op: UnaryType,
    val: Box<Expr>,
}

enum UnaryType {
    Minus,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(c)  => write!(f, "{}", c),
            Expr::Binary(bin) => write!(f, "({})", bin),
            Expr::Grouping(_) => todo!(),
            Expr::Error       => todo!(),
            Expr::Unary(val)  => write!(f, "{}", val),
        }
    }
}

impl Display for BinaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self.op {
            BinaryType::Minus => '-',
            BinaryType::Plus  => '+',
            BinaryType::Slash => '/',
            BinaryType::Star  => '*',
        };

        write!(f, "{} {} {}", op, self.left, self.right)
    }
}

impl Display for UnaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self.op {
            UnaryType::Minus => '-',
        };

        write!(f, "{}{}", op, self.val)
    }
}

impl BinaryType {
    fn binding_power(&self) -> (u8, u8) {
        match &self {
            BinaryType::Minus => (1, 2),
            BinaryType::Plus  => (1, 2),
            BinaryType::Slash => (3, 4),
            BinaryType::Star  => (3, 4),
        }
    }
}


impl Expr {
    pub fn eval(&self) -> i64 {
        match self {
            Expr::Literal(num) => *num,
            Expr::Binary(b)    => b.eval(),
            Expr::Grouping(b)  => b.as_ref().eval(),
            Expr::Error        => todo!(),
            Expr::Unary(val)   => val.eval(),
        }
    }
}

impl BinaryExpr {
    pub fn eval(&self) -> i64 {
        let left = self.left.eval();
        let right = self.right.eval();
        match self.op {
            BinaryType::Minus => left - right,
            BinaryType::Plus  => left + right,
            BinaryType::Slash => left / right,
            BinaryType::Star  => left * right,
        }
    }
}

impl UnaryExpr {
    pub fn eval(&self) -> i64 {
        match self.op {
            UnaryType::Minus => - self.val.eval(),
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;

    fn test_parser(s: &str, res: &str, val: i64) {
        println!("TESTING: Parsing \"{}\"", s);
        let mut parser = Parser::new(s.into());

        print!("TOKENS: ");
        for token in parser.tokens.iter() {
            print!("{} ", token);
        }
        println!("");
        println!("PARSING...");

        let parsed = parser.parse();

        println!("EVALUATING...");
        let output = parsed.eval();
        assert_eq!(parsed.to_string(), res);
        assert_eq!(val, output);
    }

    #[test]
    fn grouping() {
        test_parser("(1)"        , "(1)" , 1); 
        test_parser("(1 - 1)"    , "((- 1 1))" , 0); 
        test_parser("1 - (1 - 1)", "(- 1 ((- 1 1)))" , 1); 
    }

    #[test]
    fn binary_operator_precedence() {
        test_parser("1", "1", 1);

        test_parser("1 + 2 * 3", "(+ 1 (* 2 3))", 7);

        test_parser("1 * 2 + 3", "(+ (* 1 2) 3)", 5);

        test_parser("1 + 2 + 3", "(+ (+ 1 2) 3)", 6);

        test_parser("1 - 1 - 1", "(- (- 1 1) 1)", -1);
    }}
