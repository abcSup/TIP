use std::default;

use lexer::Lexer;
use miette::LabeledSpan;

pub use lexer::{Token, TokenKind};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub functions: Vec<Function>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub declarations: Vec<String>,
    pub block: Vec<Statement>,
    pub return_stmt: Box<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Assignment(Box<Expr>, Box<Expr>),
    Output(Box<Expr>),
    Error(Box<Expr>),
    If {
        condition: Box<Expr>,
        yes: Box<Statement>,
        no: Option<Box<Statement>>,
    },
    While {
        condition: Box<Expr>,
        body: Box<Statement>,
    },
    Block(Vec<Statement>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Unary(Op, Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Atom(Atom),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Atom {
    Number(u64),
    Ident(String),
    Record {
        names: Vec<String>,
        values: Vec<Expr>,
    },
    Null,
    Input,
}

pub struct Parser<'de> {
    lexer: Lexer<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Parser {
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(&mut self) -> Result<Program, miette::Error> {
        self.parse_program()
    }

    // Prog -> Fun ... Fun
    fn parse_program(&mut self) -> Result<Program, miette::Error> {
        let mut functions = Vec::new();
        loop {
            functions.push(self.parse_function()?);
            if self.lexer.peek().is_none() {
                break;
            }
        }
        Ok(Program { functions })
    }

    // Fun -> Id ( parameter_list ) {
    //     var_block?
    //     Stm
    //     return Exp;
    //   }
    fn parse_function(&mut self) -> Result<Function, miette::Error> {
        let name = self.lexer.expect(TokenKind::Ident)?;

        self.lexer.expect(TokenKind::LParen)?;

        let parameters = self.parse_parameter_list()?;

        self.lexer.expect(TokenKind::RParen)?;

        self.lexer.expect(TokenKind::LCurly)?;

        let declarations = self.parse_var_block()?;

        let block = self.parse_block_within()?;

        self.lexer.expect(TokenKind::Return)?;

        let return_stmt = Box::new(self.parse_expression(0)?);

        self.lexer.expect(TokenKind::Semicolon)?;

        self.lexer.expect(TokenKind::RCurly)?;

        Ok(Function {
            name: name.origin.to_owned(),
            parameters,
            declarations,
            block,
            return_stmt,
        })
    }

    // Id, ..., Id
    fn parse_parameter_list(&mut self) -> Result<Vec<String>, miette::Error> {
        let mut parameters = Vec::new();
        if matches!(
            self.lexer.peek(),
            Some(Token {
                kind: TokenKind::Ident,
                ..
            })
        ) {
            loop {
                let ident = self.lexer.expect(TokenKind::Ident)?;
                parameters.push(ident.origin.to_owned());

                if !matches!(
                    self.lexer.peek(),
                    Some(Token {
                        kind: TokenKind::Comma,
                        ..
                    })
                ) {
                    break;
                }
                self.lexer.expect(TokenKind::Comma)?;
            }
        }

        Ok(parameters)
    }

    // VarBlock ->
    //  | VarBlock VarBlock
    //  | var Id (, Id)+;
    fn parse_var_block(&mut self) -> Result<Vec<String>, miette::Error> {
        let mut variables = Vec::new();
        loop {
            if !matches!(
                self.lexer.peek(),
                Some(Token {
                    kind: TokenKind::Var,
                    ..
                })
            ) {
                break;
            }

            self.lexer.expect(TokenKind::Var)?;

            loop {
                let ident = self.lexer.expect(TokenKind::Ident)?;
                variables.push(ident.origin.to_owned());

                if !matches!(
                    self.lexer.peek(),
                    Some(Token {
                        kind: TokenKind::Comma,
                        ..
                    })
                ) {
                    break;
                }
                self.lexer.expect(TokenKind::Comma)?;
            }

            self.lexer.expect(TokenKind::Semicolon)?;
        }

        Ok(variables)
    }

    fn parse_block(&mut self) -> Result<Statement, miette::Error> {
        self.lexer.expect(TokenKind::LCurly)?;

        let body = self.parse_block_within()?;

        self.lexer.expect(TokenKind::RCurly)?;

        Ok(Statement::Block(body))
    }

    fn parse_block_within(&mut self) -> Result<Vec<Statement>, miette::Error> {
        let mut statements = Vec::new();
        loop {
            let stmt = self.parse_statement();
            match stmt {
                Ok(stmt) => statements.push(stmt),
                Err(_) => match self.lexer.peek() {
                    Some(Token {
                        kind: TokenKind::LCurly,
                        ..
                    }) => {
                        statements.push(self.parse_block()?);
                    }
                    Some(Token {
                        kind: TokenKind::Return | TokenKind::RCurly,
                        ..
                    }) => break,
                    Some(token) => {
                        return Err(miette::miette!(
                            labels = vec![LabeledSpan::at(
                                token.offset..token.offset + token.origin.len(),
                                "here"
                            )],
                            help = format!("Expected a Return or RCurly, found {:?}", token.kind),
                            "Unexpected token"
                        )
                        .with_source_code(self.lexer.whole.to_string()));
                    }
                    None => {
                        return Err(miette::miette!("Unexpected EOF"));
                    }
                },
            };
        }

        Ok(statements)
    }

    // Stm -> Id = Exp;
    //  | output Exp;
    //  | error Exp;
    //  | Stm Stm
    //  | if (Exp) { Stm } [else { Stm }]?
    //  | while (Exp) { Stm }
    //  | *Exp = Exp;
    //  | Id.Id = Exp;
    //  | (*Exp).Id = Exp;
    fn parse_statement(&mut self) -> Result<Statement, miette::Error> {
        // TODO: remove panic here
        let statement = match self.lexer.peek().unwrap() {
            // Id = Exp;
            // Id.Id = Exp;
            Token {
                kind: TokenKind::Ident,
                ..
            } => {
                let var = self.lexer.expect(TokenKind::Ident)?;

                self.lexer.expect(TokenKind::Equal)?;

                let lhs = match self.lexer.peek() {
                    Some(Token {
                        kind: TokenKind::Dot,
                        ..
                    }) => {
                        self.lexer.expect(TokenKind::Dot)?;

                        let field = self.lexer.expect(TokenKind::Ident)?;

                        Expr::Binary(
                            Op::Dot,
                            Box::new(Expr::Atom(Atom::Ident(var.origin.to_owned()))),
                            Box::new(Expr::Atom(Atom::Ident(field.origin.to_owned()))),
                        )
                    }
                    _ => Expr::Atom(Atom::Ident(var.origin.to_owned())),
                };

                let rhs = self.parse_expression(0)?;

                self.lexer.expect(TokenKind::Semicolon)?;

                Statement::Assignment(Box::new(lhs), Box::new(rhs))
            }
            // output Exp;
            Token {
                kind: TokenKind::Output,
                ..
            } => {
                self.lexer.expect(TokenKind::Output)?;

                let lhs = self.parse_expression(0)?;

                self.lexer.expect(TokenKind::Semicolon)?;

                Statement::Output(Box::new(lhs))
            }
            // error Exp;
            Token {
                kind: TokenKind::Error,
                ..
            } => {
                self.lexer.expect(TokenKind::Error)?;

                let lhs = self.parse_expression(0)?;

                self.lexer.expect(TokenKind::Semicolon)?;

                Statement::Error(Box::new(lhs))
            }
            // if (Exp) { Stm } [else { Stm }]?
            Token {
                kind: TokenKind::If,
                ..
            } => {
                self.lexer.expect(TokenKind::If)?;

                self.lexer.expect(TokenKind::LParen)?;

                let condition = Box::new(self.parse_expression(0)?);

                self.lexer.expect(TokenKind::RParen)?;

                let yes = if let Some(Token {
                    kind: TokenKind::LCurly,
                    ..
                }) = self.lexer.peek()
                {
                    self.parse_block()?
                } else {
                    self.parse_statement()?
                };
                let yes = Box::new(yes);

                // else statements
                let no = if let Some(Token {
                    kind: TokenKind::Else,
                    ..
                }) = self.lexer.peek()
                {
                    self.lexer.expect(TokenKind::Else)?;

                    let no = if let Some(Token {
                        kind: TokenKind::LCurly,
                        ..
                    }) = self.lexer.peek()
                    {
                        self.parse_block()?
                    } else {
                        self.parse_statement()?
                    };

                    Some(Box::new(no))
                } else {
                    None
                };

                Statement::If { condition, yes, no }
            },
            // while (Exp) { Stm }
            Token {
                kind: TokenKind::While,
                ..
            } => {
                self.lexer.expect(TokenKind::While)?;

                self.lexer.expect(TokenKind::LParen)?;

                let condition = Box::new(self.parse_expression(0)?);

                self.lexer.expect(TokenKind::RParen)?;

                let body = Box::new(self.parse_block()?);

                Statement::While { condition, body }
            }
            // *Exp = Exp;
            Token {
                kind: TokenKind::Star,
                ..
            } => {
                let pointer = self.parse_expression(0)?;

                self.lexer.expect(TokenKind::Equal)?;

                let rhs = self.parse_expression(0)?;

                self.lexer.expect(TokenKind::Semicolon)?;

                Statement::Assignment(Box::new(pointer), Box::new(rhs))
            }
            // (*Exp).Id = Exp;
            Token {
                kind: TokenKind::LParen,
                ..
            } => {
                self.lexer.expect(TokenKind::Star)?;

                let pointer = self.parse_expression(0)?;

                self.lexer.expect(TokenKind::RParen)?;

                self.lexer.expect(TokenKind::Dot)?;

                let field = self.lexer.expect(TokenKind::Ident)?;

                let lhs = Expr::Binary(
                    Op::Dot,
                    Box::new(Expr::Unary(Op::Star, Box::new(pointer))),
                    Box::new(Expr::Atom(Atom::Ident(field.origin.to_owned()))),
                );

                self.lexer.expect(TokenKind::Equal)?;

                let rhs = self.parse_expression(0)?;

                self.lexer.expect(TokenKind::Semicolon)?;

                Statement::Assignment(Box::new(lhs), Box::new(rhs))
            }
            token => {
                return Err(miette::miette!(
                    labels = vec![LabeledSpan::at(
                        token.offset..token.offset + token.origin.len(),
                        "here"
                    )],
                    help = "Expected a statement",
                    "Unexpected token"
                )
                .with_source_code(self.lexer.whole.to_string()));
            }
        };

        Ok(statement)
    }

    // Exp -> Int
    //  | Id
    //  | Exp + Exp | Exp – Exp | Exp * Exp | Exp / Exp
    //  | Exp > Exp | Exp == Exp
    //  | ( Exp )
    //  | input
    //  | alloc Exp
    //  | & Id
    //  | * Exp
    //  | null
    //  | { Id:Exp, …, Id:Exp }
    //  | Exp.Id
    //  | Exp( Exp, ..., Exp )
    fn parse_expression(&mut self, min_bp: u8) -> Result<Expr, miette::Error> {
        // TODO: remove panic
        let mut lhs = match self.lexer.next().unwrap() {
            // Int
            Token {
                kind: TokenKind::Number(value),
                ..
            } => Expr::Atom(Atom::Number(value)),
            // Id
            Token {
                origin: ident,
                kind: TokenKind::Ident,
                ..
            } => Expr::Atom(Atom::Ident(ident.to_owned())),
            // ( Exp )
            Token {
                kind: TokenKind::LParen,
                ..
            } => {
                let lhs = self.parse_expression(0)?;
                self.lexer.expect(TokenKind::RParen)?;
                lhs
            }
            // input
            Token {
                kind: TokenKind::Input,
                ..
            } => Expr::Atom(Atom::Input),
            // alloc Exp
            Token {
                kind: TokenKind::Alloc,
                ..
            } => {
                let lhs = self.parse_expression(0)?;
                Expr::Unary(Op::Alloc, Box::new(lhs))
            }
            // unary
            // &Id
            Token {
                kind: TokenKind::Ampersand,
                ..
            } => {
                let ident = self.lexer.expect(TokenKind::Ident)?;
                Expr::Unary(
                    Op::Ampersand,
                    Box::new(Expr::Atom(Atom::Ident(ident.origin.to_owned()))),
                )
            }
            // *Exp
            Token {
                kind: TokenKind::Star,
                ..
            } => {
                let lhs = self.parse_expression(0)?;
                Expr::Unary(Op::Star, Box::new(lhs))
            }
            // -Expr
            Token {
                kind: TokenKind::Minus,
                ..
            } => {
                let ((), r_bp) = prefix_binding_power(Op::Minus);
                let rhs = self.parse_expression(r_bp)?;
                Expr::Unary(Op::Minus, Box::new(rhs))
            }
            // null
            Token {
                kind: TokenKind::Null,
                ..
            } => Expr::Atom(Atom::Null),
            // { Id:Exp, ..., Id:Exp }
            Token {
                kind: TokenKind::LCurly,
                ..
            } => {
                let mut names = Vec::new();
                let mut values = Vec::new();

                loop {
                    let ident = self.lexer.expect(TokenKind::Ident)?;

                    self.lexer.expect(TokenKind::Colon)?;

                    let value = self.parse_expression(0)?;

                    // Field value cannot be a Record
                    if let Expr::Atom(Atom::Record { .. }) = value {
                        return Err(miette::miette!(
                            labels = vec![LabeledSpan::at(
                                ident.offset..ident.offset + ident.origin.len(),
                                "here"
                            )],
                            "Field value cannot be a Record"
                        )
                        .with_source_code(self.lexer.whole.to_string()));
                    }

                    names.push(ident.origin.to_owned());
                    values.push(value);

                    if !matches!(
                        self.lexer.peek(),
                        Some(Token {
                            kind: TokenKind::Comma,
                            ..
                        })
                    ) {
                        break;
                    }
                    self.lexer.expect(TokenKind::Comma)?;
                }
                self.lexer.expect(TokenKind::RCurly)?;

                Expr::Atom(Atom::Record { names, values })
            }
            token => {
                return Err(miette::miette!(
                    labels = vec![LabeledSpan::at(
                        token.offset..token.offset + token.origin.len(),
                        "here"
                    )],
                    help = "Expected an expression",
                    "Unexpected token"
                )
                .with_source_code(self.lexer.whole.to_string()))
            }
        };

        loop {
            let op = match self.lexer.peek().unwrap() {
                Token {
                    kind: TokenKind::Plus,
                    ..
                } => Op::Plus,
                Token {
                    kind: TokenKind::Minus,
                    ..
                } => Op::Minus,
                Token {
                    kind: TokenKind::Star,
                    ..
                } => Op::Star,
                Token {
                    kind: TokenKind::Slash,
                    ..
                } => Op::Slash,
                Token {
                    kind: TokenKind::Greater,
                    ..
                } => Op::Greater,
                Token {
                    kind: TokenKind::EqualEqual,
                    ..
                } => Op::EqualEqual,
                Token {
                    kind: TokenKind::Dot,
                    ..
                } => Op::Dot,
                Token {
                    kind: TokenKind::LParen,
                    ..
                } => Op::Call,
                Token {
                    kind:
                        TokenKind::Semicolon
                        | TokenKind::RParen
                        | TokenKind::Comma
                        | TokenKind::Equal
                        | TokenKind::RCurly,
                    ..
                } => break,
                token => {
                    return Err(miette::miette!(
                        labels = vec![LabeledSpan::at(
                            token.offset..token.offset + token.origin.len(),
                            "here"
                        )],
                        help = format!("Expected an operator, found {:?}", token.kind),
                        "Unexpected operator"
                    )
                    .with_source_code(self.lexer.whole.to_string()))
                }
            };

            // Exp.Id
            // Exp( Exp, ..., Exp )
            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next().unwrap();

                lhs = match op {
                    Op::Dot => {
                        let ident = self.lexer.expect(TokenKind::Ident)?;
                        Expr::Binary(
                            Op::Dot,
                            Box::new(lhs),
                            Box::new(Expr::Atom(Atom::Ident(ident.origin.to_owned()))),
                        )
                    }
                    Op::Call => {
                        let mut args = Vec::new();
                        if !matches!(
                            self.lexer.peek(),
                            Some(Token {
                                kind: TokenKind::RParen,
                                ..
                            })
                        ) {
                            loop {
                                args.push(self.parse_expression(0)?);
                                if !matches!(
                                    self.lexer.peek(),
                                    Some(Token {
                                        kind: TokenKind::Comma,
                                        ..
                                    })
                                ) {
                                    break;
                                }
                                self.lexer.expect(TokenKind::Comma)?;
                            }
                        }
                        self.lexer.expect(TokenKind::RParen)?;
                        Expr::Call(Box::new(lhs), args)
                    }
                    _ => unreachable!(),
                };
                continue;
            }

            // Exp + Exp | Exp – Exp
            // Exp * Exp | Exp / Exp
            // Exp > Exp | Exp == Exp
            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next().unwrap();

                let rhs = self.parse_expression(r_bp)?;

                lhs = Expr::Binary(op, Box::new(lhs), Box::new(rhs));
                continue;
            }

            break;
        }

        Ok(lhs)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Greater,
    EqualEqual,
    Dot,
    Call,
    Alloc,
    Ampersand,
}

// -Expr
fn prefix_binding_power(op: Op) -> ((), u8) {
    match op {
        Op::Minus => ((), 7),
        _ => unreachable!(),
    }
}

// Exp + Exp | Exp – Exp
// Exp * Exp | Exp / Exp
// Exp > Exp | Exp == Exp
fn infix_binding_power(op: Op) -> Option<(u8, u8)> {
    match op {
        Op::Plus | Op::Minus => Some((1, 2)),
        Op::Star | Op::Slash => Some((3, 4)),
        Op::Greater | Op::EqualEqual => Some((5, 6)),
        _ => None,
    }
}

// Exp.Id
// Exp( Exp, ..., Exp )
fn postfix_binding_power(op: Op) -> Option<(u8, ())> {
    match op {
        Op::Dot => Some((9, ())),
        Op::Call => Some((11, ())),
        _ => None,
    }
}
