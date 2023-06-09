use std::fmt::Display;
use std::println;
use crate::tokenizer::Tokenizer;
use crate::tokens::{Token, TokenType};
use crate::errors::Error;

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Expr(ExprStatement),
    Return(ReturnStatement),
}

#[derive(Debug)]
pub struct LetStatement {
    name: String,
    val: Expr,
}

#[derive(Debug)]
pub struct ExprStatement {
    pub val: Expr,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub val: Expr,
}

pub struct Parser {
    tokenizer: Tokenizer,
}

impl Parser {
    pub fn new(source: String) -> Self {
        let tokenizer = Tokenizer::new(source);
        Parser {
            tokenizer,
        }
    }

    pub fn get_ast(mut self) -> Result<Vec<Statement>, (Vec<Statement>, Error)> {
        let mut statements = vec![];
        loop {
            if self.tokenizer.peek().typ == TokenType::EoF { break }
            let res = self.parse_statement();
            match res {
                Ok(stmt) => statements.push(stmt),
                Err(err) => return Err(( statements, err )),
            }
        }
        Ok(statements)
    }
        
    fn parse_statement(&mut self) -> Result<Statement, Error> {
        match self.tokenizer.peek().typ {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _              => self.parse_expr_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, Error> {
        self.tokenizer.assert_next(TokenType::Let)?;

        let name = if let Token { typ: TokenType::Identifier(name), .. } = self.tokenizer.peek() {
            self.tokenizer.next();
            name
        } else {
            return Err(Error(("Identifier".to_owned(), self.tokenizer.next())))
        };

        self.tokenizer.assert_next(TokenType::Equal)?;

        let val = self.parse(0)?;

        self.tokenizer.assert_next(TokenType::Semicolon)?;

        Ok(Statement::Let(LetStatement { name, val }))
    }

    fn parse_expr_statement(&mut self) -> Result<Statement, Error> {
        let val = self.parse(0)?;

        self.tokenizer.assert_next(TokenType::Semicolon)?;

        Ok(Statement::Expr(ExprStatement { val }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, Error> {
        self.tokenizer.assert_next(TokenType::Return)?;
        let val = self.parse(0)?;
        self.tokenizer.assert_next(TokenType::Semicolon)?;

        Ok(Statement::Return(ReturnStatement { val }))
    }
}

impl Parser {
    fn parse(&mut self, min_bp: u8) -> Result<Expr, Error> {
        let token = self.tokenizer.next();
        println!("Start: {:?}", token);
        let mut lhs = match token.typ {
            TokenType::Bang            => Expr::Unary(self.parse_unary(token.typ)?),
            TokenType::EoF             => return Err(Error(("Expression".into(), token))),
            TokenType::Error           => return Err(Error(("Expression".into(), token))),
            TokenType::False           => Expr::Bool(false),
            TokenType::Fun             => self.parse_func()?,
            TokenType::Identifier(val) => Expr::Identifier(val),
            TokenType::If              => self.parse_if()?,
            TokenType::LeftBrace       => self.parse_block()?,
            TokenType::LeftParen       => self.parse_grouping()?,
            TokenType::Minus           => Expr::Unary(self.parse_unary(token.typ)?),
            TokenType::Number(num)     => Expr::Integer(num),
            TokenType::String(val)     => Expr::String(val),
            TokenType::True            => Expr::Bool(true),
            _                          => return Err(Error(("Expression".into(), token))),
        };

        if self.tokenizer.peek().typ == TokenType::LeftParen {
            lhs = self.parse_func_call(lhs)?;
        }

        loop {
            let token = self.tokenizer.peek();
            println!("Loop: {:?}", token);
            let op = match token.typ {
                TokenType::And          => BinaryType::And,
                TokenType::BangEqual    => BinaryType::BangEqual,
                TokenType::Comma        => break,
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
                _                       => return Err(Error(("End of expression or binary operator".into(), token)))
            };

            let (l_bp, r_bp) = op.binding_power();

            if l_bp < min_bp {
                break
            }

            self.tokenizer.next();

            let rhs = self.parse(r_bp);

            lhs = Expr::Binary(BinaryExpr {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs?),
            });
        }

        Ok(lhs)
    }

    fn parse_block(&mut self) -> Result<Expr, Error> {
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
        Ok(Expr::Block(BlockExpr { statements }))
    }

    fn parse_func(&mut self) -> Result<Expr, Error> {
        self.tokenizer.assert_next(TokenType::LeftParen)?;
        let mut pars = vec![];
        loop {
            if self.tokenizer.peek().typ == TokenType::RightParen {
                break
            }
            let token = self.tokenizer.next();
            match token.typ {
                TokenType::Identifier(par) => pars.push(par),
                _                          => return Err(Error(("Parameter".to_owned(), token)))
            }
            if self.tokenizer.peek().typ == TokenType::Comma {
                self.tokenizer.next();
            } 
        }
        self.tokenizer.assert_next(TokenType::RightParen)?;
        self.tokenizer.assert_next(TokenType::LeftBrace)?;

        let body = Box::new(self.parse_block()?);
        // self.tokenizer.assert_next(TokenType::Semicolon)?;

        Ok(Expr::Function(FunctionLiteral { pars, body }))
    }

    fn parse_func_call(&mut self, func: Expr) -> Result<Expr, Error> {
        self.tokenizer.assert_next(TokenType::LeftParen)?;
        let mut args = vec![];
        loop {
            if self.tokenizer.peek().typ == TokenType::RightParen {
                self.tokenizer.next();
                break;
            }
            let arg = self.parse(0)?;
            args.push(arg);
            if self.tokenizer.peek().typ == TokenType::Comma {
                self.tokenizer.next();
            }
        }
        let func = Box::new(func);
        Ok(Expr::FunctionCall(FunctionCall { func, args }))
    }

    fn parse_if(&mut self) -> Result<Expr, Error> {
        self.tokenizer.assert_next(TokenType::LeftParen)?;
        let cond = Box::new(self.parse(0)?);
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

    fn parse_grouping(&mut self) -> Result<Expr, Error> {
        let res = Box::new(self.parse(0)?);
        self.tokenizer.assert_next(TokenType::RightParen)?;
        Ok(Expr::Grouping(res))
    }

    fn parse_unary(&mut self, typ: TokenType) -> Result<UnaryExpr, Error> {
        let op = match typ {
            TokenType::Minus => UnaryType::Minus,
            TokenType::Bang => UnaryType::Bang,
            _ => todo!(),
        };

        let (_, r_bp) = op.binding_power();

        let val = Box::new(self.parse(r_bp)?);

        Ok(UnaryExpr { op, val })
    }
}


#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Block(BlockExpr),
    Bool(bool),
    Error(Error),
    Function(FunctionLiteral),
    Grouping(Box<Expr>),
    Identifier(String),
    If(IfExpr),
    Integer(i64),
    String(String),
    Unary(UnaryExpr),
    FunctionCall(FunctionCall),
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub op: BinaryType,
    pub right: Box<Expr>,
}

#[derive(Debug)]
pub enum BinaryType {
    And,
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Minus,
    Or,
    Plus,
    Slash,
    Star,
}

#[derive(Debug)]
pub struct BlockExpr {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct FunctionLiteral {
    pars: Vec<String>,
    body: Box<Expr>,
}

#[derive(Debug)]
pub struct FunctionCall {
    func: Box<Expr>,
    args: Vec<Expr>,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub cons: Box<Expr>,
    pub alt: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnaryType,
    pub val: Box<Expr>,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryType {
    Minus,
    Bang,
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(val)    => write!(f, "{}", val),
            Statement::Expr(val)   => write!(f, "{}", val),
            Statement::Return(val) => write!(f, "{}", val),
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

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.val)
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
                    val.statements.iter().map(|stmt| format!("{}", stmt))
                );
                write!(f, "{{{}}}", s)
            },
            Expr::If(val)         => write!(f, "{}", val),
            Expr::Function(val)   => {
                let mut s = String::new();
                if let Some(par) = val.pars.first() {
                    s.push_str(&par.to_string());
                }
                s.extend(
                    val.pars.iter().skip(1).map(|stmt| format!(", {}", stmt))
                );
                write!(f, "func({}) {}", s, val.body)
            },
            Expr::FunctionCall(val)   => {
                let mut s = String::new();
                if let Some(arg) = val.args.first() {
                    s.push_str(&format!("{}", arg));
                }
                s.extend(
                    val.args.iter().skip(1).map(|arg| format!(", {}", arg))
                );
                write!(f, "{}({})", val.func, s)
            },
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

#[cfg(test)]
mod test {
    use std::println;
    use super::*;

    fn test_program(source: &str) {
        let parser = Parser::new(source.into());
        let program = parser.get_ast();
        if program.is_err() {
            println!("{}", source);
            println!("{:?}", program);
        }
        assert!(program.is_ok());
    }

    fn test_program_error(source: &str) {
        let parser = Parser::new(source.into());
        let program = parser.get_ast();
        // let program = Program::new(parser);
        if program.is_ok() {
            println!("SOURCE CODE: {}", source);
            println!("{:?}", program);
        }
        assert!(program.is_err());
    }

    fn test_expr_error(s: &str, res: &str) {
        println!("TESTING: Parsing \"{}\"", s);
        let mut parser = Parser::new(s.into());

        print!("TOKENS: ");
        for token in parser.tokenizer.clone() {
            print!("{} ", token);
        }
        println!("");
        println!("PARSING...");

        let parsed = parser.parse(0).unwrap_err();

        assert_eq!(parsed.to_string(), res);
    }

    fn parse_expr_to_s_expr(s: &str, res: &str)  {
        println!("TESTING: Parsing \"{}\"", s);
        let mut parser = Parser::new(s.into());

        print!("TOKENS: ");
        for token in parser.tokenizer.clone() {
            print!("{} ", token);
        }
        println!("");
        println!("PARSING...");

        let parsed = parser.parse(0).unwrap();

        println!("EVALUATING...");
        println!("{:#?}", parsed);
        assert_eq!(parsed.to_string(), res);
    }

    #[test]
    fn grouping() {
        parse_expr_to_s_expr("(1)"        , "(1)"); 
        parse_expr_to_s_expr("(1 - 1)"    , "((- 1 1))"); 
        parse_expr_to_s_expr("1 - (1 - 1)", "(- 1 ((- 1 1)))"); 
        parse_expr_to_s_expr("((1))", "((1))"); 
    }

    #[test]
    fn grouping2() {
        parse_expr_to_s_expr("1 - (1) - 1",   "(- (- 1 (1)) 1)"); 
        parse_expr_to_s_expr("1 - (1 - (1))", "(- 1 ((- 1 (1))))"); 
        parse_expr_to_s_expr("((1) - 1)",     "((- (1) 1))"); 
        parse_expr_to_s_expr("1 - ((1) - 1)", "(- 1 ((- (1) 1)))"); 
    }

    #[test]
    fn binary_operator_precedence() {
        parse_expr_to_s_expr("1", "1");
        parse_expr_to_s_expr("1 + 2 * 3", "(+ 1 (* 2 3))");
        parse_expr_to_s_expr("1 * 2 + 3", "(+ (* 1 2) 3)");
        parse_expr_to_s_expr("1 + 2 + 3", "(+ (+ 1 2) 3)");
        parse_expr_to_s_expr("1 - 1 - 1", "(- (- 1 1) 1)");
    }

    #[test]
    fn unary() {
        parse_expr_to_s_expr("-1", "-1");
        parse_expr_to_s_expr("-----1", "-----1");
        parse_expr_to_s_expr("----1", "----1");
        parse_expr_to_s_expr("1 + -1", "(+ 1 -1)");
        parse_expr_to_s_expr("- 1 + - 2", "(+ -1 -2)");
    }

    #[test]
    fn bool() {
        parse_expr_to_s_expr("true", "true");
        parse_expr_to_s_expr("false", "false");
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
        test_program("return 1;");
        test_program("return { return 1; };");

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
    }

    #[test]
    fn bad_expr_test() {
        test_expr_error("let sneed = \"Seed and Feed\";\nlet abc = 1 + 2", "ERROR[Expression,let]");
        test_expr_error("return foo;", "ERROR[Expression,return]");
        test_expr_error("\"aaaa", "ERROR[Expression,LEXING ERROR]");
        test_expr_error("(", "ERROR[Expression,EoF]");
        test_expr_error(")", "ERROR[Expression,)]");
        test_expr_error("((())", "ERROR[Expression,)]");
        test_expr_error("\"\\", "ERROR[Expression,LEXING ERROR]");
        test_expr_error("(1", "ERROR[),EoF]");

        test_expr_error("if", "ERROR[(,EoF]");
        test_expr_error("if ()", "ERROR[Expression,)]");
        test_expr_error("if (1 < 2)", "ERROR[{,EoF]");
        test_expr_error("if (1 < 2) {", "ERROR[Expression,EoF]");
        test_expr_error("if (1 < 2) {} else {", "ERROR[Expression,EoF]");

        test_expr_error("fn", "ERROR[(,EoF]");
        test_expr_error("fn()", "ERROR[{,EoF]");
        test_expr_error("fn() {", "ERROR[Expression,EoF]");
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

    #[test]
    fn func_test() {
        parse_expr_to_s_expr("fn(x, y) { x + y; };", "func(x, y) {(+ x y);}");
        parse_expr_to_s_expr("fn(x, y) {};"        , "func(x, y) {}");
        parse_expr_to_s_expr("fn(x, y,) {};"       , "func(x, y) {}");
        parse_expr_to_s_expr("fn() {};"            , "func() {}");
        parse_expr_to_s_expr("fn() { 5; };"        , "func() {5;}");
    }

    #[test]
    fn call_expr() {
        parse_expr_to_s_expr("foo(x, y)", "foo(x, y)");
        parse_expr_to_s_expr("foo(x, x + x)", "foo(x, (+ x x))");
        parse_expr_to_s_expr("foo((x + (x)))", "foo(((+ x (x))))");
        parse_expr_to_s_expr("foo()", "foo()");

        parse_expr_to_s_expr("fn(x) {}(1)", "func(x) {}(1)");
    }
}
