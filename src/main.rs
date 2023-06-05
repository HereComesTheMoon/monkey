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

fn run(source: String) -> Result<Expr, ExpectedError> {
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

    pub fn parse_statement(&mut self) -> Result<Statement, ExpectedError> {
        let token = self.tokenizer.peek();
        let res = match self.tokenizer.peek().typ {
            TokenType::Let => self.parse_let(),
            _              => self.parse_expr_statement(),
        };

        res
    }

    pub fn parse_let(&mut self) -> Result<Statement, ExpectedError> {
        self.tokenizer.assert_next(TokenType::Let)?;

        let name = if let Token { typ: TokenType::Identifier(name), .. } = self.tokenizer.peek() {
            self.tokenizer.next();
            name.clone()
        } else {
            return Err(ExpectedError(("Identifier".to_owned(), self.tokenizer.next().clone())))
        };

        self.tokenizer.assert_next(TokenType::Equal)?;

        let val = self.parse()?;

        self.tokenizer.assert_next(TokenType::Semicolon)?;

        Ok(Statement::Let(LetStatement { name, val }))
    }

    pub fn parse_expr_statement(&mut self) -> Result<Statement, ExpectedError> {
        // let val = self.parse().is_err()?;
        let val = self.parse()?;

        self.tokenizer.assert_next(TokenType::Semicolon)?;

        Ok(Statement::Expr(ExprStatement { val }))
    }
}

impl Parser {
    pub fn parse(&mut self) -> Result<Expr, ExpectedError> {
        self.parse_bp(0)
    }

    fn parse_bp(&mut self, min_bp: u8) -> Result<Expr, ExpectedError> {
        let token = self.tokenizer.next();
        println!("Start: {:?}", token);
        let mut lhs = match token.typ {
            TokenType::Bang            => Expr::Unary(self.parse_unary(token.typ)?),
            TokenType::EoF             => return Err(ExpectedError(("Some token.".into(), token))),
            TokenType::Error           => return Err(ExpectedError(("Lexing error! Most likely unmatched \".".into(), token))),
            TokenType::False           => Expr::Bool(false),
            TokenType::Identifier(val) => Expr::Identifier(val.clone()),
            TokenType::If              => self.parse_if()?,
            TokenType::LeftBrace       => self.parse_block()?,
            TokenType::LeftParen       => self.parse_grouping()?,
            TokenType::Minus           => Expr::Unary(self.parse_unary(token.typ)?),
            TokenType::Number(num)     => Expr::Integer(num),
            TokenType::String(val)     => Expr::String(val.clone()),
            TokenType::True            => Expr::Bool(true),
            _                          => return Err(ExpectedError(("Start of expression.".into(), token))),
        };

        loop {
            let token = self.tokenizer.peek();
            println!("Loop: {:?}", token);
            let op = match token.typ {
                TokenType::And          => BinaryType::And,
                TokenType::BangEqual    => BinaryType::BangEqual,
                TokenType::Else         => break,
                TokenType::EoF          => break,
                TokenType::EqualEqual   => BinaryType::EqualEqual,
                TokenType::Greater      => BinaryType::Greater,
                TokenType::GreaterEqual => BinaryType::GreaterEqual,
                TokenType::Less         => BinaryType::Less,
                TokenType::LessEqual    => BinaryType::LessEqual,
                TokenType::Minus        => BinaryType::Minus,
                TokenType::Plus         => BinaryType::Plus,
                TokenType::RightParen   => break,
                TokenType::RightBrace   => break,
                TokenType::Semicolon    => break,
                TokenType::Slash        => BinaryType::Slash,
                TokenType::Star         => BinaryType::Star,
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
                right: Box::new(rhs?),
            });
        }

        Ok(lhs)
    }

    fn parse_block(&mut self) -> Result<Expr, ExpectedError> {
        // self.tokenizer.assert_next(TokenType::LeftBrace)?;
        let mut statements = vec![];
        loop {
            if let TokenType::RightBrace = self.tokenizer.peek().typ {
                break;
            }
            let next_statement = self.parse_statement()?;
            statements.push(next_statement);
        }
        self.tokenizer.assert_next(TokenType::RightBrace)?;
        Ok(Expr::Block(statements))
    }

    fn parse_if(&mut self) -> Result<Expr, ExpectedError> {
        self.tokenizer.assert_next(TokenType::LeftParen)?;
        let cond = Box::new(self.parse_bp(0)?);
        self.tokenizer.assert_next(TokenType::RightParen)?;
        self.tokenizer.assert_next(TokenType::LeftBrace)?;
        let cons = Box::new(self.parse_block()?);

        let alt = if let TokenType::Else = self.tokenizer.peek().typ {
            self.tokenizer.assert_next(TokenType::Else)?;
            self.tokenizer.assert_next(TokenType::LeftBrace)?;
            Some(Box::new(self.parse_block()?))
        } else { None };

        Ok(Expr::If(IfExpr { cond, cons, alt }))
        
    }

    fn parse_grouping(&mut self) -> Result<Expr, ExpectedError> {
        let res = Box::new(self.parse_bp(0)?);
        self.tokenizer.assert_next(TokenType::RightParen)?;
        Ok(Expr::Grouping(res))
    }

    fn parse_unary(&mut self, typ: TokenType) -> Result<UnaryExpr, ExpectedError> {
        let op = match typ {
            TokenType::Minus => UnaryType::Minus,
            TokenType::Bang => UnaryType::Bang,
            _ => todo!(),
        };

        let (_, r_bp) = op.binding_power();

        let val = Box::new(self.parse_bp(r_bp)?);

        Ok(UnaryExpr { op, val })
    }
}


#[derive(Debug)]
enum Expr {
    Binary(BinaryExpr),
    Block(Vec<Statement>),
    Bool(bool),
    Error(ExpectedError),
    Grouping(Box<Expr>),
    Identifier(String),
    If(IfExpr),
    Integer(i64),
    String(String),
    Unary(UnaryExpr),
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
struct BlockExpr {
    statements: Vec<Statement>,
}

#[derive(Debug)]
struct IfExpr {
    cond: Box<Expr>,
    cons: Box<Expr>,
    alt: Option<Box<Expr>>,
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

impl Expr {
    pub fn is_err(self) -> Result<Expr, ExpectedError> {
        match self {
            Expr::Error(err) => Err(err),
            _ => Ok(self),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(val)  => write!(f, "{}", val),
            Statement::Expr(val) => write!(f, "{}", val),
        }
    }
}


impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.val)
    }
}


impl Display for ExprStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{};", self.val)
    }
}


impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Integer(val)    => write!(f, "{}", val),
            Expr::Bool(val)       => write!(f, "{}", val),
            Expr::Binary(val)     => write!(f, "({})", val),
            Expr::Grouping(val)   => write!(f, "({})", val),
            Expr::Error(err)      => write!(f, "({})", err),
            Expr::Unary(val)      => write!(f, "{}", val),
            Expr::String(val)     => write!(f, "{}", val),
            Expr::Identifier(val) => write!(f, "{}", val),
            Expr::Block(val)      => {
                let mut s = String::new();
                s.extend(
                    val.iter().map(|stmt| format!("{}", stmt))
                );
                write!(f, "{{{}}}", s)
            },
            Expr::If(val)         => write!(f, "{}", val),
        }
    }
}

impl Display for BlockExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        s.extend(
            self.statements.iter().map(|stmt| format!("{}", stmt))
        );

        write!(f, "{{{}}}", s)
    }
}

impl Display for IfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.alt.is_some() {
            write!(f, "if {} {} else {}", self.cond, self.cons, *self.alt.as_ref().unwrap())
        } else {
            write!(f, "if ({}) {{ {} }}", self.cond, self.cons)
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
            Expr::Integer(num)  => *num,
            Expr::Binary(b)     => b.eval(),
            Expr::Grouping(b)   => b.as_ref().eval(),
            Expr::Error(err)    => panic!(),
            Expr::Unary(val)    => val.eval(),
            Expr::String(_)     => todo!(),
            Expr::Identifier(_) => todo!(),
            Expr::Bool(val)     => *val as i64,
            Expr::Block(_)      => todo!(),
            Expr::If(_)         => todo!(),
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

impl From<ExpectedError> for Expr {
    fn from(value: ExpectedError) -> Self {
        Expr::Error(ExpectedError((value.0.0, value.0.1)))
    }
}

impl Display for ExpectedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ERROR[{},{}]", self.0.0, self.0.1)
    }
}

#[cfg(test)]
mod test {
    use super::*;


    fn test_program(source: &str) {
        let parser = Parser::new(source.into());
        let program = Program::new(parser);
        if program.is_err() {
            println!("{}", source);
            println!("{:?}", program);
        }
        assert!(program.is_ok());
    }

    fn test_program_error(source: &str) {
        let parser = Parser::new(source.into());
        let program = Program::new(parser);
        if program.is_ok() {
            println!("SOURCE CODE: {}", source);
            println!("{:?}", program);
        }
        assert!(program.is_err());
    }

    fn test_eval(s: &str, res: &str, val: i64) -> Result<(), ExpectedError> {
        println!("TESTING: Parsing \"{}\"", s);
        let mut parser = Parser::new(s.into());

        print!("TOKENS: ");
        for token in parser.tokenizer.clone() {
            print!("{} ", token);
        }
        println!("");
        println!("PARSING...");

        let parsed = parser.parse()?;

        println!("EVALUATING...");
        let output = parsed.eval();
        assert_eq!(val, output);
        assert_eq!(parsed.to_string(), res);
        Ok(())
    }

    fn parse_expr_to_s_expr(s: &str, res: &str) -> Result<(), ExpectedError> {
        println!("TESTING: Parsing \"{}\"", s);
        let mut parser = Parser::new(s.into());

        print!("TOKENS: ");
        for token in parser.tokenizer.clone() {
            print!("{} ", token);
        }
        println!("");
        println!("PARSING...");

        let parsed = parser.parse()?;

        println!("EVALUATING...");
        println!("{:#?}", parsed);
        assert_eq!(parsed.to_string(), res);
        Ok(())
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
        parse_expr_to_s_expr("!true", "!true");
        parse_expr_to_s_expr("!!!true", "!!!true");
        parse_expr_to_s_expr("!!!!true", "!!!!true");
        parse_expr_to_s_expr("true and true", "(and true true)");
        parse_expr_to_s_expr("true or false", "(or true false)");
        parse_expr_to_s_expr("true or false == true", "(or true (== false true))");
        parse_expr_to_s_expr("3 < 5 == true", "(== (< 3 5) true)");
        parse_expr_to_s_expr("3 <= 5 == true", "(== (<= 3 5) true)");
        parse_expr_to_s_expr("3 > 5 == false", "(== (> 3 5) false)");
        parse_expr_to_s_expr("3 >= 5 == true", "(== (>= 3 5) true)");
        parse_expr_to_s_expr("3 != 5 == true", "(== (!= 3 5) true)");
        parse_expr_to_s_expr("!false == true", "(== !false true)");
        parse_expr_to_s_expr("3 < 5 and 4 == 4 == true", "(and (< 3 5) (== (== 4 4) true))");
    }

    #[test]
    fn program_test() {
        test_program("let sneed = \"Seed and Feed\";\nlet abc = 1 + 2;");
        test_program("");
        test_program("foo;");
        test_program("foo + bar == baz;");
        test_program("((foo + bar == baz));");
    }

    #[test]
    fn bad_program_test() {
        test_program_error("1");
        test_program_error("let sneed = \"Seed and Feed\";\nlet abc = 1 + 2");
        test_program_error("foo");
        test_program_error("\"aaaa");
        test_program_error("(");
        test_program_error(")");
        test_program_error("((())");
        test_program_error("\"\\");
        parse_expr_to_s_expr("(1", "(ERROR[),EoF])");
    }

    #[test]
    fn if_test() {
        test_program("if (3 < 5) {\n\tlet a = 7;\n} else {\n\t1+2;\n};");
        test_program("if (3 < 5) {\n\tlet a = 7;\n};");
        parse_expr_to_s_expr("if (x < y) {\nlet foo = z;\n7;\n} else {};", "if (< x y) {let foo = z;7;} else {}");
    }

    #[test]
    fn block_test() {
        test_program("{\na + b;\nlet banana = \"foo\";\n};");
        test_program("{};");
        test_program("{{};};");
    }
}


