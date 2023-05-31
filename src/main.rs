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

fn run(source: String) -> i64 {
    // let sc = Scanner::new(source);
    let mut parser = Parser::new(source);
    parser.parse()
}

fn run_pratt(source: String) -> Expr {
    let mut pratt = Pratt::new(source);
    pratt.parse()
}


struct Pratt {
    source: String,
    tokens: Vec<Token>,
    pos: usize,
}

impl Pratt {
    pub fn new(source: String) -> Self {
        let tokens = Tokenizer::new(&source).get_infix_tokens();
        Pratt {
            source,
            tokens,
            pos: 0,
        }
    }
}

impl Pratt {
    pub fn parse(&mut self) -> Expr {
        self.parse_bp(0)
    }

    fn parse_bp(&mut self, min_bp: u8) -> Expr {
        println!("{:?} {:?}", self.pos, self.tokens[self.pos]);
        let mut lhs = match self.tokens[self.pos].typ {
            TokenType::Number => self.parse_number(),
            _ => panic!("Oh no???"),
        };

        self.pos += 1;

        loop {
            if self.tokens.len() <= self.pos {
                break
            }
            println!("{:?} {:?}", self.pos, self.tokens[self.pos]);
            let op = match self.tokens[self.pos].typ {
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

    fn parse_number(&mut self) -> Expr {
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


struct Parser {
    source: String,
    tokens: Vec<Token>, // Prefix order
    pos: usize,
}

impl Parser {
    pub fn new(source: String) -> Self {
        // let tokens = infix_to_prefix(Lexer::new(&source));
        let tokens = Tokenizer::new(&source).get_tokens();
        Parser {
            source,
            tokens,
            pos: 0,
        }
    }

    pub fn parse(&mut self) -> i64 {
        let exp = self.parse_expr();
        exp.unwrap().eval()
    }

    fn parse_expr(&mut self) -> Option<Expr> {
        // println!("Token: {} — {:?}", self.pos, self.tokens[self.pos]);
        Some(match self.tokens[self.pos].typ {
            TokenType::LeftParen  => self.parse_group(),
            TokenType::RightParen => unreachable!(),
            TokenType::Minus      => self.parse_bin_expr(),
            TokenType::Plus       => self.parse_bin_expr(),
            TokenType::Slash      => self.parse_bin_expr(),
            TokenType::Star       => self.parse_bin_expr(),
            TokenType::Number     => self.parse_number(),
        })
    }

    fn parse_bin_expr(&mut self) -> Expr {
        let op = match self.tokens[self.pos].typ {
            TokenType::Minus => BinaryType::Minus,
            TokenType::Plus  => BinaryType::Plus,
            TokenType::Slash => BinaryType::Slash,
            TokenType::Star  => BinaryType::Star,
            _                => unreachable!(),
        };
        self.pos += 1;
        let left = Box::new(self.parse_expr().unwrap());
        let right = Box::new(self.parse_expr().unwrap());
        Expr::Binary(BinaryExpr { left , op , right })
    }

    fn parse_number(&mut self) -> Expr {
        let t = &self.tokens[self.pos];
        let int = self.source[t.pos..t.pos+t.len].parse();
        self.pos += 1;
        if let Ok(int) = int {
            Expr::Literal(int)
        } else {
            println!("Oh no!");
            Expr::Error
        }
    }

    fn parse_group(&mut self) -> Expr {
        assert!(matches!(self.tokens[self.pos].typ, TokenType::LeftParen));
        self.pos += 1;
        let inner = self.parse_expr();
        println!("{:?}", self.tokens[self.pos]);
        assert!(matches!(self.tokens[self.pos].typ, TokenType::RightParen));
        self.pos += 1;
        Expr::Grouping(Box::new(inner.unwrap()))
    }
}

enum Expr {
    Literal(i64),
    Binary(BinaryExpr),
    Grouping(Box<Expr>),
    Error,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(c)  => write!(f, "{}", c),
            Expr::Binary(bin) => write!(f, "({})", bin),
            Expr::Grouping(_) => todo!(),
            Expr::Error       => todo!(),
        }
    }
}

struct BinaryExpr {
    left: Box<Expr>,
    op: BinaryType,
    right: Box<Expr>,
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

enum BinaryType {
    Minus,
    Plus,
    Slash,
    Star,
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


#[cfg(test)]
mod test {
    use super::*;

    fn tester(s: &str, expect: i64) {
        let res = run(s.to_string());
        assert_eq!(res, expect);
    }

    #[test]
    fn test_group_1() {
        tester("1 - 1 - 1", -1);
    }
    
    #[test]
    fn test_group_2() {
        tester("(1)", 1);
    }
    
    #[test]
    fn test_group_3() {
        tester("(1 - 1)", 0);
    }

    #[test]
    fn test_group_4() {
        tester("1 - (1 - 1)", 1);
    }


    fn test_pratt(s: &str, res: &str, val: i64) {
        let s = run_pratt(s.into());
        let output = s.eval();
        assert_eq!(s.to_string(), res);
        assert_eq!(val, output);
    }

    #[test]
    fn pratt_tests() {
        test_pratt("1", "1", 1);

        test_pratt("1 + 2 * 3", "(+ 1 (* 2 3))", 7);

        test_pratt("1 * 2 + 3", "(+ (* 1 2) 3)", 5);

        test_pratt("1 + 2 + 3", "(+ (+ 1 2) 3)", 6);

        test_pratt("1 - 1 - 1", "(- (- 1 1) 1)", -1);


        // let s = run_pratt("a + b * c * d + e".into());
        // assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");
    }}
