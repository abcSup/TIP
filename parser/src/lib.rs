use lexer::{Lexer, Token, TokenKind};
use miette::LabeledSpan;

#[derive(Debug)]
pub enum TokenTree<'de> {
    Function {
        name: Token<'de>,
        parameters: Vec<Token<'de>>,
        declarations: Vec<Token<'de>>,
        block: Box<TokenTree<'de>>,
        return_stmt: Box<TokenTree<'de>>,
    },
    Call(Box<TokenTree<'de>>, Vec<TokenTree<'de>>),
    Cons(Op, Vec<TokenTree<'de>>),
    Equal(Box<TokenTree<'de>>, Box<TokenTree<'de>>),
    Output(Box<TokenTree<'de>>),
    Error(Box<TokenTree<'de>>),
    If {
        condition: Box<TokenTree<'de>>,
        yes: Box<TokenTree<'de>>,
        no: Option<Box<TokenTree<'de>>>,
    },
    While {
        condition: Box<TokenTree<'de>>,
        body: Box<TokenTree<'de>>,
    },
    Block(Vec<TokenTree<'de>>),
    Atom(Atom<'de>),
}

#[derive(Debug)]
pub enum Atom<'de> {
    Number(u64),
    Ident(&'de str),
    Record {
        names: Vec<&'de str>,
        values: Vec<TokenTree<'de>>,
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

    // Prog -> Fun ... Fun
    pub fn parse_program(&mut self) -> Result<TokenTree<'de>, miette::Error> {
        self.parse_function()
    }

    // Fun -> Id ( parameter_list ) {
    //     var_block?
    //     Stm
    //     return Exp;
    //   }
    pub fn parse_function(&mut self) -> Result<TokenTree<'de>, miette::Error> {
        let name = self.lexer.expect(TokenKind::Ident)?;

        self.lexer.expect(TokenKind::LParen)?;

        let parameters = self.parse_parameter_list()?;

        self.lexer.expect(TokenKind::RParen)?;

        self.lexer.expect(TokenKind::LCurly)?;

        let declarations = self.parse_var_block()?;

        // let mut block = Vec::new();
        let block = Box::new(self.parse_statement(false)?);

        self.lexer.expect(TokenKind::Return)?;

        let return_stmt = Box::new(self.parse_expression(0)?);

        self.lexer.expect(TokenKind::Semicolon)?;

        self.lexer.expect(TokenKind::RCurly)?;

        Ok(TokenTree::Function {
            name,
            parameters,
            declarations,
            block,
            return_stmt,
        })
    }

    // Id, ..., Id
    fn parse_parameter_list(&mut self) -> Result<Vec<Token<'de>>, miette::Error> {
        let mut parameters = Vec::new();
        if matches!(
            self.lexer.peek(),
            Some(Token {
                kind: TokenKind::Ident,
                ..
            })
        ) {
            loop {
                parameters.push(self.lexer.expect(TokenKind::Ident)?);

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
    fn parse_var_block(&mut self) -> Result<Vec<Token<'de>>, miette::Error> {
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
                variables.push(self.lexer.expect(TokenKind::Ident)?);

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

    // Stm -> Id = Exp;
    //  | output Exp;
    //  | error Exp;
    //  | Stm Stm
    //  | if (Exp) { Stm } [else { Stm }]?
    //  | while (Exp) { Stm }
    //  | *Exp = Exp;
    //  | Id.Id = Exp;
    //  | (*Exp).Id = Exp;
    fn parse_statement(&mut self, single: bool) -> Result<TokenTree<'de>, miette::Error> {
        let mut statements = Vec::new();
        loop {
            // TODO: remove panic here
            let stmt = match self.lexer.peek().unwrap() {
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

                            TokenTree::Cons(
                                Op::Dot,
                                vec![
                                    TokenTree::Atom(Atom::Ident(var.origin)),
                                    TokenTree::Atom(Atom::Ident(field.origin)),
                                ],
                            )
                        }
                        _ => TokenTree::Atom(Atom::Ident(var.origin)),
                        // TODO: Eof?
                    };

                    let rhs = self.parse_expression(0)?;

                    self.lexer.expect(TokenKind::Semicolon)?;

                    TokenTree::Equal(Box::new(lhs), Box::new(rhs))
                }
                // output Exp;
                Token {
                    kind: TokenKind::Output,
                    ..
                } => {
                    self.lexer.expect(TokenKind::Output)?;

                    let lhs = self.parse_expression(0)?;

                    self.lexer.expect(TokenKind::Semicolon)?;

                    TokenTree::Output(Box::new(lhs))
                }
                // error Exp;
                Token {
                    kind: TokenKind::Error,
                    ..
                } => {
                    self.lexer.expect(TokenKind::Error)?;

                    let lhs = self.parse_expression(0)?;

                    self.lexer.expect(TokenKind::Semicolon)?;

                    TokenTree::Error(Box::new(lhs))
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
                        self.lexer.expect(TokenKind::LCurly)?;

                        let yes = Box::new(self.parse_statement(false)?);

                        self.lexer.expect(TokenKind::RCurly)?;

                        yes
                    } else {
                        Box::new(self.parse_statement(true)?)
                    };

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
                            self.lexer.expect(TokenKind::LCurly)?;

                            let block = self.parse_statement(false)?;

                            self.lexer.expect(TokenKind::RCurly)?;

                            block
                        } else {
                            self.parse_statement(true)?
                        };

                        Some(Box::new(no))
                    } else {
                        None
                    };

                    // else statements
                    // let no = match self.lexer.peek() {
                    //     Some(Token {
                    //         kind: TokenKind::Else,
                    //         ..
                    //     }) => {
                    //         self.lexer.expect(TokenKind::Else)?;

                    //         self.lexer.expect(TokenKind::LCurly)?;

                    //         let no = self.parse_statement(false)?;

                    //         self.lexer.expect(TokenKind::RCurly)?;

                    //         Some(Box::new(no))
                    //     }
                    //     _ => None,
                    // };

                    TokenTree::If { condition, yes, no }
                }
                // while (Exp) { Stm }
                Token {
                    kind: TokenKind::While,
                    ..
                } => {
                    self.lexer.expect(TokenKind::While)?;

                    self.lexer.expect(TokenKind::LParen)?;

                    let condition = Box::new(self.parse_expression(0)?);

                    self.lexer.expect(TokenKind::RParen)?;

                    self.lexer.expect(TokenKind::LCurly)?;

                    let body = Box::new(self.parse_statement(false)?);

                    self.lexer.expect(TokenKind::RCurly)?;

                    TokenTree::While { condition, body }
                }
                // *Exp = Exp;
                Token {
                    kind: TokenKind::Star,
                    ..
                } => {
                    let lhs = self.parse_expression(0)?;

                    self.lexer.expect(TokenKind::Equal)?;

                    let rhs = self.parse_expression(0)?;

                    self.lexer.expect(TokenKind::Semicolon)?;

                    TokenTree::Equal(
                        Box::new(TokenTree::Cons(Op::Star, vec![lhs])),
                        Box::new(rhs),
                    )
                }
                // (*Exp).Id = Exp;
                Token {
                    kind: TokenKind::LParen,
                    ..
                } => {
                    self.lexer.expect(TokenKind::Star)?;

                    let lhs = self.parse_expression(0)?;

                    self.lexer.expect(TokenKind::RParen)?;

                    self.lexer.expect(TokenKind::Dot)?;

                    let field = self.lexer.expect(TokenKind::Ident)?;

                    self.lexer.expect(TokenKind::Equal)?;

                    let rhs = self.parse_expression(0)?;

                    self.lexer.expect(TokenKind::Semicolon)?;

                    TokenTree::Equal(
                        Box::new(TokenTree::Cons(
                            Op::Dot,
                            vec![
                                TokenTree::Cons(Op::Star, vec![lhs]),
                                TokenTree::Atom(Atom::Ident(field.origin)),
                            ],
                        )),
                        Box::new(rhs),
                    )
                }
                // { Stmt, ..., Stmt }
                // Note: This is not in the spec, but appears in examples
                Token {
                    kind: TokenKind::LCurly,
                    ..
                } => {
                    self.lexer.expect(TokenKind::LCurly)?;

                    let body = self.parse_statement(false)?;

                    self.lexer.expect(TokenKind::RCurly)?;

                    body
                }
                Token {
                    kind: TokenKind::Return | TokenKind::RCurly,
                    ..
                } => break,
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

            statements.push(stmt);

            if single {
                break;
            }
        }

        Ok(TokenTree::Block(statements))
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
    fn parse_expression(&mut self, min_bp: u8) -> Result<TokenTree<'de>, miette::Error> {
        // TODO: remove panic
        let mut lhs = match self.lexer.next().unwrap() {
            // Int
            Token {
                kind: TokenKind::Number(value),
                ..
            } => TokenTree::Atom(Atom::Number(value)),
            // Id
            Token {
                origin: ident,
                kind: TokenKind::Ident,
                ..
            } => TokenTree::Atom(Atom::Ident(ident)),
            // ( Exp )
            Token {
                kind: TokenKind::LParen,
                ..
            } => {
                let lhs = self.parse_expression(0)?;
                self.lexer.expect(TokenKind::RParen)?;
                TokenTree::Cons(Op::Group, vec![lhs])
            }
            // input
            Token {
                kind: TokenKind::Input,
                ..
            } => TokenTree::Atom(Atom::Input),
            // alloc Exp
            Token {
                kind: TokenKind::Alloc,
                ..
            } => {
                let lhs = self.parse_expression(0)?;
                TokenTree::Cons(Op::Alloc, vec![lhs])
            }
            // unary
            // &Id
            Token {
                kind: TokenKind::Ampersand,
                ..
            } => {
                let ident = self.lexer.expect(TokenKind::Ident)?;
                TokenTree::Cons(
                    Op::Ampersand,
                    vec![TokenTree::Atom(Atom::Ident(ident.origin))],
                )
            }
            // *Exp
            Token {
                kind: TokenKind::Star,
                ..
            } => {
                let lhs = self.parse_expression(0)?;
                TokenTree::Cons(Op::Star, vec![lhs])
            }
            // -Expr
            Token {
                kind: TokenKind::Minus,
                ..
            } => {
                let ((), r_bp) = prefix_binding_power(Op::Minus);
                let rhs = self.parse_expression(r_bp)?;
                TokenTree::Cons(Op::Minus, vec![rhs])
            }
            // null
            Token {
                kind: TokenKind::Null,
                ..
            } => TokenTree::Atom(Atom::Null),
            // { Id:Exp, ..., Id:Exp }
            Token {
                kind: TokenKind::LCurly,
                ..
            } => {
                let mut names = Vec::new();
                let mut values = Vec::new();

                loop {
                    let name = self.lexer.expect(TokenKind::Ident)?;

                    self.lexer.expect(TokenKind::Colon)?;

                    let value = self.parse_expression(0)?;

                    // Field value cannot be a Record
                    if let TokenTree::Atom(Atom::Record { .. }) = value {
                        return Err(miette::miette!(
                            labels = vec![LabeledSpan::at(
                                name.offset..name.offset + name.origin.len(),
                                "here"
                            )],
                            "Field value cannot be a Record"
                        )
                        .with_source_code(self.lexer.whole.to_string()));
                    }

                    names.push(name.origin);
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

                TokenTree::Atom(Atom::Record { names, values })
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
                        TokenTree::Cons(op, vec![TokenTree::Atom(Atom::Ident(ident.origin))])
                    }
                    Op::Call => {
                        let mut args = Vec::new();
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
                        self.lexer.expect(TokenKind::RParen)?;
                        TokenTree::Call(Box::new(lhs), args)
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

                lhs = TokenTree::Cons(op, vec![lhs, rhs]);
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
    Group,
    Alloc,
    Ampersand,
    Output,
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
