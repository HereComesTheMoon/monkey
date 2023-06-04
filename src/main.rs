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
        2 => {
                let content = fs::read_to_string(&args[0]).unwrap();
                let parser = Parser::new(content);
                Program::new(parser).unwrap();
            }
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

#[derive(Debug)]
struct Program {
    statements: Vec<Statement>,
}

#[derive(Debug)]
enum Statement {
    Let(LetStatement),
    Expr(ExprStatement),
}

#[derive(Debug)]
struct LetStatement {
    name: String,
    val: Expr,
}

#[derive(Debug)]
struct ExprStatement {
    val: Expr,
}

impl Program {
    pub fn new(mut parser: Parser) -> Result<Self, ExpectedError> {
        let mut statements = vec![];
        loop {
            let token = parser.tokenizer.peek();
            if token.typ == TokenType::EoF { break }
            let res = match parser.tokenizer.peek().typ {
                TokenType::Let => parser.parse_let(),
                _              => parser.parse_expr_statement(),
            }?;

            statements.push(res);
        }
        Ok(Program { statements })
    }
}


struct Parser {
    tokenizer: Tokenizer,
}

impl Parser {
    pub fn new(source: String) -> Self {
        let tokenizer = Tokenizer::new(source);
        Parser {
            tokenizer,
        }
    }

    pub fn parse_let(&mut self) -> Result<Statement, ExpectedError> {
        self.tokenizer.assert_next(TokenType::Let)?;

        let name = if let Token { typ: TokenType::Identifier(name), .. } = self.tokenizer.peek() {
            name.clone()
        } else {
            return Err(ExpectedError(("Identifier".to_owned(), self.tokenizer.next().unwrap().clone())))
        };

        self.tokenizer.assert_next(TokenType::Equal)?;

        let val = self.parse();

        self.tokenizer.assert_next(TokenType::Semicolon)?;

        Ok(Statement::Let(LetStatement { name, val }))
    }

    pub fn parse_expr_statement(&mut self) -> Result<Statement, ExpectedError> {
        let val = self.parse();

        self.tokenizer.assert_next(TokenType::Semicolon)?;

        Ok(Statement::Expr(ExprStatement { val }))
    }
}

impl Parser {
    pub fn parse(&mut self) -> Expr {
        self.parse_bp(0)
    }

    fn parse_bp(&mut self, min_bp: u8) -> Expr {
        let token = self.tokenizer.next().expect("Parsing after EoF!");
        println!("{:?}", token);
        let mut lhs = match token.typ {
            TokenType::Number(num)     => Expr::Integer(num),
            TokenType::False           => Expr::Bool(false),
            TokenType::True            => Expr::Bool(true),
            TokenType::String(val)     => Expr::String(val.clone()),
            TokenType::Identifier(val) => Expr::Identifier(val.clone()),
            TokenType::Minus           => Expr::Unary(self.parse_unary(token.typ)),
            TokenType::Bang            => Expr::Unary(self.parse_unary(token.typ)),
            TokenType::LeftParen       => Expr::Grouping(self.parse_grouping()),
            _                          => panic!("Oh no???"),
        };

        loop {
            let token = self.tokenizer.peek();
            if token.typ == TokenType::EoF { break }
            println!("{:?}", token);
            let op = match token.typ {
                TokenType::Minus        => BinaryType::Minus,
                TokenType::Plus         => BinaryType::Plus,
                TokenType::Slash        => BinaryType::Slash,
                TokenType::Star         => BinaryType::Star,
                TokenType::RightParen   => break,
                TokenType::Semicolon    => break,
                TokenType::GreaterEqual => BinaryType::GreaterEqual,
                TokenType::Greater      => BinaryType::Greater,
                TokenType::LessEqual    => BinaryType::LessEqual,
                TokenType::Less         => BinaryType::Less,
                TokenType::BangEqual    => BinaryType::BangEqual,
                TokenType::EqualEqual   => BinaryType::EqualEqual,
                TokenType::And          => BinaryType::And,
                TokenType::Or           => BinaryType::Or,
                t                       => todo!("{t:?}"),
            };

            let (l_bp, r_bp) = op.binding_power();

            if l_bp < min_bp {
                break
            }

            self.tokenizer.next();

            let rhs = self.parse_bp(r_bp);

            lhs = Expr::Binary(BinaryExpr {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            });
        }

        lhs
    }

    fn parse_grouping(&mut self) -> Box<Expr> {
        let res = Box::new(self.parse_bp(0));
        self.tokenizer.assert_next(TokenType::RightParen).unwrap();
        res
    }

    fn parse_unary(&mut self, typ: TokenType) -> UnaryExpr {
        let op = match typ {
            TokenType::Minus => UnaryType::Minus,
            TokenType::Bang => UnaryType::Bang,
            _ => todo!(),
        };

        let (_, r_bp) = op.binding_power();

        let val = Box::new(self.parse_bp(r_bp));

        UnaryExpr { op, val }
    }
}


#[derive(Debug)]
enum Expr {
    Integer(i64),
    Bool(bool),
    String(String),
    Identifier(String),
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Grouping(Box<Expr>),
    Error,
}

#[derive(Debug)]
struct BinaryExpr {
    left: Box<Expr>,
    op: BinaryType,
    right: Box<Expr>,
}

#[derive(Debug)]
enum BinaryType {
    Minus,
    Plus,
    Slash,
    Star,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    BangEqual,
    EqualEqual,
    And,
    Or,
}

#[derive(Debug)]
struct UnaryExpr {
    op: UnaryType,
    val: Box<Expr>,
}

#[derive(Debug)]
enum UnaryType {
    Minus,
    Bang,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Integer(val)    => write!(f, "{}", val),
            Expr::Bool(val)       => write!(f, "{}", val),
            Expr::Binary(val)     => write!(f, "({})", val),
            Expr::Grouping(val)   => write!(f, "({})", val),
            Expr::Error           => todo!(),
            Expr::Unary(val)      => write!(f, "{}", val),
            Expr::String(val)     => write!(f, "{}", val),
            Expr::Identifier(val) => write!(f, "{}", val),
        }
    }
}

impl Display for BinaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self.op {
            BinaryType::Minus        => "-",
            BinaryType::Plus         => "+",
            BinaryType::Slash        => "/",
            BinaryType::Star         => "*",
            BinaryType::GreaterEqual => ">=",
            BinaryType::Greater      => ">",
            BinaryType::LessEqual    => "<=",
            BinaryType::Less         => "<",
            BinaryType::BangEqual    => "!=",
            BinaryType::EqualEqual   => "==",
            BinaryType::And          => "and",
            BinaryType::Or           => "or",
        };

        write!(f, "{} {} {}", op, self.left, self.right)
    }
}

impl Display for UnaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self.op {
            UnaryType::Minus => '-',
            UnaryType::Bang  => '!',
        };

        write!(f, "{}{}", op, self.val)
    }
}

impl BinaryType {
    fn binding_power(&self) -> (u8, u8) {
        match &self {
            BinaryType::Minus        => (7, 8),
            BinaryType::Plus         => (7, 8),
            BinaryType::Slash        => (9, 10),
            BinaryType::Star         => (9, 10),
            BinaryType::GreaterEqual => (5, 6),
            BinaryType::Greater      => (5, 6),
            BinaryType::LessEqual    => (5, 6),
            BinaryType::Less         => (5, 6),
            BinaryType::BangEqual    => (3, 4),
            BinaryType::EqualEqual   => (3, 4),
            BinaryType::And          => (1, 2),
            BinaryType::Or           => (1, 2),
        }
    }
}

impl UnaryType {
    fn binding_power(&self) -> ((), u8) {
        match &self {
            UnaryType::Minus => ((), 11),
            UnaryType::Bang => ((), 11),
        }
    }
}


impl Expr {
    pub fn eval(&self) -> i64 {
        match self {
            Expr::Integer(num) => *num,
            Expr::Binary(b)    => b.eval(),
            Expr::Grouping(b)  => b.as_ref().eval(),
            Expr::Error        => todo!(),
            Expr::Unary(val)   => val.eval(),
            Expr::String(_) => todo!(),
            Expr::Identifier(_) => todo!(),
            Expr::Bool(val)    => *val as i64,
        }
    }
}

impl BinaryExpr {
    pub fn eval(&self) -> i64 {
        let left = self.left.eval();
        let right = self.right.eval();
        match self.op {
            BinaryType::Minus        => left - right,
            BinaryType::Plus         => left + right,
            BinaryType::Slash        => left / right,
            BinaryType::Star         => left * right,
            BinaryType::GreaterEqual => (left >= right).into(),
            BinaryType::Greater      => (left > right).into(),
            BinaryType::LessEqual    => (left <= right).into(),
            BinaryType::Less         => (left < right).into(),
            BinaryType::BangEqual    => (left != right).into(),
            BinaryType::EqualEqual   => (left == right).into(),
            BinaryType::And          => (left != 0 && right != 0).into(),
            BinaryType::Or           => (left != 0 || right != 0).into(),
        }
    }
}

impl UnaryExpr {
    pub fn eval(&self) -> i64 {
        match self.op {
            UnaryType::Minus => - self.val.eval(),
            UnaryType::Bang => !self.val.eval(),
        }
    }
}


#[derive(Debug)]
struct ExpectedError((String, Token));

impl From<(TokenType, Token)> for ExpectedError {
    fn from(value: (TokenType, Token)) -> Self {
        ExpectedError((value.0.to_string(), value.1))
    }
}

#[cfg(test)]
mod test {
    use super::*;


    fn test_eval(s: &str, res: &str, val: i64) {
        println!("TESTING: Parsing \"{}\"", s);
        let mut parser = Parser::new(s.into());

        print!("TOKENS: ");
        for token in parser.tokenizer.clone() {
            print!("{} ", token);
        }
        println!("");
        println!("PARSING...");

        let parsed = parser.parse();

        println!("EVALUATING...");
        let output = parsed.eval();
        assert_eq!(val, output);
        assert_eq!(parsed.to_string(), res);
    }

    fn test_parser(s: &str, res: &str) {
        println!("TESTING: Parsing \"{}\"", s);
        let mut parser = Parser::new(s.into());

        print!("TOKENS: ");
        for token in parser.tokenizer.clone() {
            print!("{} ", token);
        }
        println!("");
        println!("PARSING...");

        let parsed = parser.parse();

        println!("EVALUATING...");
        assert_eq!(parsed.to_string(), res);
    }

    #[test]
    fn grouping() {
        test_eval("(1)"        , "(1)" , 1); 
        test_eval("(1 - 1)"    , "((- 1 1))" , 0); 
        test_eval("1 - (1 - 1)", "(- 1 ((- 1 1)))" , 1); 
        test_eval("((1))", "((1))" , 1); 
    }

    #[test]
    fn grouping2() {
        test_eval("1 - (1) - 1"        , "(- (- 1 (1)) 1)" , -1); 
        test_eval("1 - (1 - (1))"        , "(- 1 ((- 1 (1))))" , 1); 
        test_eval("((1) - 1)"        , "((- (1) 1))" , 0); 
        test_eval("1 - ((1) - 1)"        , "(- 1 ((- (1) 1)))" , 1); 
    }

    #[test]
    fn binary_operator_precedence() {
        test_eval("1", "1", 1);
        test_eval("1 + 2 * 3", "(+ 1 (* 2 3))", 7);
        test_eval("1 * 2 + 3", "(+ (* 1 2) 3)", 5);
        test_eval("1 + 2 + 3", "(+ (+ 1 2) 3)", 6);
        test_eval("1 - 1 - 1", "(- (- 1 1) 1)", -1);
    }

    #[test]
    fn program_test() {
        let source = "let sneed = \"Seed and Feed\";\nlet abc = 1 + 2;".into();
        let parser = Parser::new(source);
        let program = Program::new(parser);
        println!("{:?}", program);
    }

    #[test]
    fn unary() {
        test_eval("-1", "-1", -1);
        test_eval("-----1", "-----1", -1);
        test_eval("----1", "----1", 1);
        test_eval("1 + -1", "(+ 1 -1)", 0);
        test_eval("- 1 + - 2", "(+ -1 -2)", -3);
    }

    #[test]
    fn bool() {
        test_eval("true", "true", 1);
        test_eval("false", "false", 0);
        test_parser("!true", "!true");
        test_parser("!!!true", "!!!true");
        test_parser("!!!!true", "!!!!true");
        test_parser("true and true", "(and true true)");
        test_parser("true or false", "(or true false)");
        test_parser("true or false == true", "(or true (== false true))");
        test_parser("3 < 5 == true", "(== (< 3 5) true)");
        test_parser("3 <= 5 == true", "(== (<= 3 5) true)");
        test_parser("3 > 5 == false", "(== (> 3 5) false)");
        test_parser("3 >= 5 == true", "(== (>= 3 5) true)");
        test_parser("3 != 5 == true", "(== (!= 3 5) true)");
        test_parser("!false == true", "(== !false true)");
        test_parser("3 < 5 and 4 == 4 == true", "(and (< 3 5) (== (== 4 4) true))");
    }
}


