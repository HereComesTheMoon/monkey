mod scanner;
use std::env;

use std::fs;
use std::io;
use std::io::{BufRead, Write};
use std::process;
use scanner::{Scanner, Token, TokenType, infix_to_prefix};

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

struct Parser {
    source: String,
    tokens: Vec<Token>, // Prefix order
    pos: usize,
}

impl Parser {
    pub fn new(source: String) -> Self {
        Parser {
            source: source.clone(),
            tokens: infix_to_prefix(Scanner::new(source)),
            pos: 0,
        }
    }

    pub fn parse(&mut self) -> i64 {
        let exp = self.parse_expr();
        exp.unwrap().eval()
        // for (k, token) in self.tokens.iter().enumerate() {
        //     println!("Pos: {}, {:?}", k, token);
        // }
        // println!("Tokens: {:?}", self.tokens);
        // while self.pos < self.tokens.len() {
        //     let new_last = self.parse_expr();
        //     assert!(new_last.is_some());
        //     assert!(self.last.is_none());
        //     self.last = new_last;
        // }
        // let res = self.parse_expr();
        // println!("The result is: {}", res.eval())
        // self.last.as_ref().unwrap().eval()
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
    use super::run;

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
}