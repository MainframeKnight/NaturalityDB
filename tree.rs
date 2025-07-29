use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum ExprTree {
    Ref(String, String, Box<Node>, u64, u64),
    For(String, Box<Lambda>),
    IntLit(i64),
    CharLit(char),
    BoolLit(bool),
    DoubleLit(f64),
    ArrayLit(Vec<Box<Node>>, Option<Type>),
    JustLit(Box<Node>),
    NothingLit(Type),
    TupleLit(Vec<Box<Node>>),
    Ident(String),
    Plus(Box<Node>, Box<Node>),
    Minus(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Mod(Box<Node>, Box<Node>),
    Exp(Box<Node>, Box<Node>),
    Dot(Box<Node>, Box<Node>),
    Call(Box<Node>, Vec<Box<Node>>),
    Eq(bool, Box<Node>, Box<Node>), // The first arg is true, when == and false when !=
    Cmp(bool, bool, Box<Node>, Box<Node>), // 1st arg: true -> ">", false -> "<", 2nd arg: true -> nonstrict, false -> strict.
    IfExpr(Box<Node>, Box<Node>, Box<Node>),
    LambdaExpr(Box<Lambda>)
}

#[derive(Debug, Clone)]
pub struct Node {
    pub tree: ExprTree,
    pub col: u64,
    pub ln: u64
}
impl Node {
    pub fn simple(t: ExprTree) -> Node { Self { tree: t, col: 0, ln: 0 }}
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: Vec<(String, Type)>,
    pub code: Node,
    pub named: Option<(String, Type)>
}

#[derive(Debug, Clone)]
pub struct CoordStr {
    pub name: String, pub ln: u64, pub col: u64
}
impl PartialEq for CoordStr {
    fn eq(&self, other: &Self) -> bool {
        return self.name == other.name;
    }
}
impl CoordStr {
    pub fn new(n: String) -> Self { CoordStr { name: n, ln: 0, col: 0 }}
}
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Object(CoordStr),
    Int,
    Char,
    Bool,
    Double,
    FuncType(Vec<Box<Type>>),
    Array(Box<Type>),
    Maybe(Box<Type>),
    Tuple(Vec<Box<Type>>),
    Sum(Vec<Box<Type>>)
}
#[derive(Debug, Clone)]
pub enum SpType {
    Reg(Type),
    Gen(Box<Type>, Lambda, u64),
    Restrict(Box<Type>, Lambda)
}
#[derive(Debug, PartialEq, Clone)]
pub enum AttrFlag {
    Computable,
    Global,
    Unique,
    None
}
#[derive(Debug, Clone)]
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
    Commit(String),
    NewEntity(String, Vec<Attr>),
    Eval(Node),
    Add(String, Vec<(String, Vec<Node>)>),
    Delete(String, Lambda),
    Trans(String, Lambda, Vec<(String, Lambda)>),
    Reshape(String, Vec<ReshapeOptions>, Option<String>),
    Project(String, Vec<String>, String),
    Join(Vec<String>, Lambda, String),
    Product(Vec<String>, String),
    Drop(String)
}
#[derive(Debug)]
pub struct ComNode {
    pub cmd: Command,
    pub col: u64,
    pub ln: u64
}
#[derive(Debug)]
pub struct DBState {
    pub header: Vec<(String, Vec<Attr>)>,
    pub data: HashMap<(u64, u64), Vec<Node>>,
    pub ref_list: HashMap<(u64, u64), u64>
}

pub fn escapes(c: &char) -> Option<char> {
    match c {
        'n' => Some('\n'),
        '\\' => Some('\\'),
        '\"' => Some('\"'),
        '@' => Some('@'),
        _ => None
    }
}

pub trait LexerIterator: Iterator<Item = (String, u64, u64)> {
    fn lookahead(&mut self) -> Option<String>;
    fn coords(&self) -> (u64, u64);
}

pub struct TokenList<'a> {
    pub tokens: &'a Vec<(String, u64, u64)>,
    pub index: u64
}
impl Iterator for TokenList<'_> {
    type Item = (String, u64, u64);
    fn next(&mut self) -> Option<Self::Item> {
        match self.tokens.get(self.index as usize) {
            Some(res) => {
                self.index += 1;
                Some(res.clone())
            }, None => None
        }
    }
}
impl LexerIterator for TokenList<'_> {
    fn coords(&self) -> (u64, u64) {
        (self.tokens[self.index as usize].1, self.tokens[self.index as usize].2)
    }
    fn lookahead(&mut self) -> Option<String> {
        self.tokens.get(self.index as usize).map(|x| x.0.clone())
    }
}

pub struct Lexer {
    input: String,
    pub index: usize,
    pub col: u64,
    pub line: u64,
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
                    if let Some(c) = self.input.chars().nth(self.index) {
                        if c.is_alphanumeric() {
                            tok.push(c);
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
                if s.chars().nth(i) == Some('@') && (i == 0 || s.chars().nth(i - 1) != Some('\\')) {
                    isCom = false;
                } else if s.chars().nth(i) == Some('\n') {
                    str.push(s.chars().nth(i).unwrap());
                }
            } else if s.chars().nth(i) == Some('@') && (i == 0 || s.chars().nth(i - 1) != Some('\\')) {
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
impl LexerIterator for Lexer {
    fn lookahead(&mut self) -> Option<String> {
        self.lookahead()
    }
    fn coords(&self) -> (u64, u64) {
        self.coords()
    }
}

pub fn expect<T: LexerIterator>(input: &mut T, tok: Vec<&str>) -> Result<(), String> {
    for i in tok {
        match input.next() {
            Some((lexeme, line, col)) => if i == lexeme {} else {
                return Err(format!("Expected '{}', found '{}' at ({}, {}).", i, lexeme, line, col))
            },
            None => return Err(format!("Expected '{}', found EOF.", i))
        }
    } Ok(())
}

pub fn find_bracket<T: LexerIterator>(input: &mut T, close_br: &str, open_br: &str) -> Result<(Vec<(String, u64, u64)>, u64, u64), String> {
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

pub fn read_ident<T: LexerIterator>(input: &mut T) -> Result<String, String> {
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