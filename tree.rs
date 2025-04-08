use std::collections::HashMap;

#[derive(Debug)]
pub struct Expr{
    pub tree: ExprTree,
    pub exprType: Option<Box<Type>>
}
#[derive(Debug)]
pub enum ExprTree {
    IntLit(i64),
    CharLit(char),
    BoolLit(bool),
    DoubleLit(f64),
    ArrayLit(Vec<Box<ExprTree>>),
    JustLit(Box<ExprTree>),
    NothingLit,
    TupleLit(Vec<Box<ExprTree>>),
    Ident(String),
    Plus(Box<ExprTree>, Box<ExprTree>),
    Minus(Box<ExprTree>, Box<ExprTree>),
    Mul(Box<ExprTree>, Box<ExprTree>),
    Div(Box<ExprTree>, Box<ExprTree>),
    Mod(Box<ExprTree>, Box<ExprTree>),
    Exp(Box<ExprTree>, Box<ExprTree>),
    Dot(Box<ExprTree>, Box<ExprTree>),
    Call(Box<ExprTree>, Vec<Box<ExprTree>>),
    Eq(bool, Box<ExprTree>, Box<ExprTree>), // The first arg is true, when == and false when !=
    Cmp(bool, bool, Box<ExprTree>, Box<ExprTree>), // 1st arg: true -> ">", false -> "<", 2nd arg: true -> nonstrict, false -> strict.
    IfExpr(Box<ExprTree>, Box<ExprTree>, Box<ExprTree>)
}

#[derive(Debug)]
pub struct Lambda {
    pub params: Vec<(String, Type)>,
    pub code: Expr
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Object(String),
    Int,
    Char,
    Bool,
    Double,
    FuncType(Vec<Box<Type>>),
    Array(Option<Box<Type>>),
    Maybe(Box<Type>),
    Tuple(Vec<Box<Type>>),
    Sum(Vec<Box<Type>>)
}
#[derive(Debug)]
pub enum SpType {
    Reg(Type),
    Gen(Box<Type>, Lambda),
    Restrict(Box<Type>, Lambda)
}
#[derive(Debug)]
pub enum AttrFlag {
    Computable,
    Global,
    Primary,
    None
}
#[derive(Debug)]
pub struct Attr {
    pub name: String,
    pub attrType: SpType,
    pub default: Option<Lambda>,
    pub flag: AttrFlag
}
impl Attr {
    pub fn new() -> Attr {
        Attr { name: String::new(), attrType: SpType::Reg(Type::Int), default: None,
            flag: AttrFlag::None}
    }
}
#[derive(Debug)]
pub enum ReshapeOptions {
    Collapse(String),
    New(Attr)
}
#[derive(Debug)]
pub enum Command {
    Open(String),
    NewEntity(String, Vec<Attr>),
    Eval(Expr),
    Add(String, Vec<(String, Vec<Expr>)>),
    Delete(String, Lambda),
    Trans(String, Lambda),
    Reshape(String, Vec<ReshapeOptions>, Option<String>),
    Project(String, Vec<String>, String),
    Join(Vec<String>, Lambda, String),
    Product(Vec<String>, String),
    Drop(String)
}
#[derive(Debug)]
pub struct DBState {
    pub header: Vec<(String, Vec<Attr>)>,
    pub data: HashMap<(u64, u64), Vec<Expr>>
}

pub fn escapes(c: &char) -> Option<char> {
    match c {
        'n' => Some('\n'),
        '\\' => Some('\\'),
        '\"' => Some('\"'),
        _ => None
    }
}
pub struct Lexer {
    input: String,
    index: usize,
    col: u64,
    line: u64,
    errors: Vec<String>
}
impl Iterator for Lexer {
    type Item = (String, u64, u64);
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(a) = self.input.chars().nth(self.index) {
            let (c_line, c_col) = (self.line, self.col);
            if a == '\n' {
                self.line += 1;
                self.col = 1;
                self.index += 1;
            }
            else if a.is_whitespace() {
                self.col += 1;
                self.index += 1;
            } else if a.is_digit(10) || a == '-' {
                let is_neg = a == '-';
                if is_neg {
                    match self.input.chars().nth(self.index + 1) {
                        Some(c) if c.is_digit(10) => {},
                        Some(_) | None => {
                            self.col += 1;
                            self.index += 1;
                            return Some(("-".to_string(), c_line, c_col))
                        }
                    }
                }
                let mut tok = String::new();
                tok.push(a);
                self.col += 1;
                self.index += 1;
                loop {
                    if let Some(a) = self.input.chars().nth(self.index) {
                        if a.is_digit(10) {
                            tok.push(a);
                            self.index += 1;
                            self.col += 1;
                        } else { break }
                    } else { break }
                }
                if self.input.chars().nth(self.index) == Some('.') {
                    tok.push('.');
                    self.index += 1;
                    self.col += 1;
                    loop {
                        if let Some(a) = self.input.chars().nth(self.index) {
                            if a.is_digit(10) {
                                tok.push(a);
                                self.index += 1;
                                self.col += 1;
                            } else { break }
                        } else { break }
                    }
                    if tok.chars().last() == Some('.') {
                        tok.push('0');
                    }
                }
                return Some((tok, c_line, c_col));
            } else if a.is_alphabetic() {
                let mut tok = a.to_string();
                self.col += 1;
                self.index += 1;
                loop {
                    if let Some(a) = self.input.chars().nth(self.index) {
                        if a.is_alphanumeric() {
                            tok.push(a);
                            self.index += 1;
                            self.col += 1;
                        } else { break }
                    } else { break }
                }
                return Some((tok, c_line, c_col));
            } else if a == '\"' {
                let mut tok = a.to_string();
                self.index += 1;
                self.col += 1;
                while self.index < self.input.len() {
                    let b = self.input.chars().nth(self.index).unwrap();
                    if b == '\n' {
                        self.line += 1;
                        self.col = 1;
                        self.index += 1;
                        continue;
                    } else {
                        self.col += 1;
                    }
                    if b == '\\' {
                        match self.input.chars().nth(self.index + 1) {
                            Some(c) => {
                                if let Some(esc) = escapes(&c) {
                                    self.col += 1;
                                    tok.push(esc);
                                    self.index += 1;
                                } else {
                                    self.errors.push(format!["Unrecognized escape-sequence at ({}, {})", self.line, self.col]);
                                }
                            },
                            None => self.errors.push(
                                format!["Unrecognized escape-sequence at ({}, {})", self.line, self.col])
                        }
                    }
                    else if b == '\"' {
                        tok.push('\"');
                        self.index += 1;
                        break;
                    } else {
                        tok.push(b);
                    }
                    self.index += 1;
                }
                if tok.chars().last() != Some('\"') {
                    self.errors.push("Expected '\"', found EOF.".to_string());
                    tok.push('\"');
                }
                return Some((tok, c_line, c_col));
            } else if a == '\'' {
                let mut tok = "\'".to_string();
                self.index += 1;
                self.col += 1;
                if self.input.chars().nth(self.index + 1) == Some('\'') {
                    match self.input.chars().nth(self.index) {
                        Some(a) if !a.is_whitespace() || a == ' ' => {
                            self.col += 2;
                            self.index += 2;
                            tok.push(a);
                            tok.push('\'');
                            return Some((tok, c_line, c_col));
                        }
                        Some(_) | None => {}
                    }
                } else if self.input.chars().nth(self.index + 2) == Some('\'') && self.input.chars().nth(self.index) == Some('\\') {
                    match self.input.chars().nth(self.index + 1) {
                        Some(a) => {
                            if let Some(esc) = escapes(&a) {
                                tok.push(esc);
                                tok.push('\'');
                                self.col += 3;
                                self.index += 3;
                                return Some((tok, c_line, c_col));
                            }
                        }, None => {}
                    }
                }
                self.errors.push(format!["Unrecognized char literal at ({}, {})", c_line, c_col]);
                tok.push('\'');
                return Some((tok, c_line, c_col));
            }
            else {
                self.col += 1;
                self.index += 1;
                return Some((a.to_string(), c_line, c_col));
            }
        }
        None
    }
}
impl Lexer {
    pub fn new(s: String) -> Lexer {
        let mut str = String::new();
        let mut isCom = false;
        for i in 0..s.chars().count() {
            if isCom {
                if s.chars().nth(i) == Some('@') && s.chars().nth(i - 1) != Some('\\') {
                    isCom = false;
                }
            } else if s.chars().nth(i) == Some('@') {
                isCom = true;
            } else {
                str.push(s.chars().nth(i).unwrap());
            }
        }
        Lexer { input: str, col: 1, line: 1, index: 0, errors: if isCom {vec!["Expected '@', found EOF.".to_string()]}
         else {vec![]} }
    }
    pub fn get_errors(&self) -> &Vec<String> { &self.errors }
    pub fn coords(&self) -> (u64, u64) { (self.line.clone(), self.col.clone()) }
    pub fn lookahead(&mut self) -> Option<String> {
        let params = (self.index, self.line, self.col);
        let res = self.next();
        (self.index, self.line, self.col) = params;
        return res.map(|x| x.0);
    }
}

pub fn expect(input: &mut Lexer, tok: Vec<&str>) -> Result<(), String> {
    for i in tok {
        match input.next() {
            Some((lexeme, line, col)) => if i == lexeme {} else {
                return Err(format!("Expected '{}', found '{}' at ({}, {}).", i, lexeme, line, col))
            },
            None => return Err(format!("Expected '{}', found EOF.", i))
        }
    } Ok(())
}

pub fn find_bracket(input: &mut Lexer, close_br: &str, open_br: &str) -> Result<(Vec<(String, u64, u64)>, u64, u64), String> {
    let mut res = vec![];
    let mut cnt = 1;
    while let Some((lexeme, line, col)) = input.next() {
        if lexeme == close_br {
            cnt -= 1;
            if cnt == 0 {
                return Ok((res, line, col));
            }
        } else if lexeme == open_br {
            cnt += 1;
        }
        res.push((lexeme, line, col));
    }
    Err(format!("Expected '{}', found EOF.", close_br))
}

pub fn find(input: &mut Lexer, tok: &str) -> Result<Vec<(String, u64, u64)>, String> {
    let mut res = vec![];
    while let Some((lexeme, line, col)) = input.next() {
        if lexeme == tok {
            return Ok(res);
        }
        res.push((lexeme, line, col));
    }
    Err(format!("Expected '{}', found EOF.", tok))
}

pub fn read_ident(input: &mut Lexer) -> Result<String, String> {
    match input.next() {
        Some((lexeme, line, col)) => {
            if let Some(a) = lexeme.chars().nth(0) {
                if a.is_alphabetic() {
                    return Ok(lexeme);
                }
            }
            return Err(format!("Expected identifier, found '{}' at ({}, {}).", lexeme, line, col))
        },
        None => return Err(format!("Expected identifier, found EOF."))
    }
}