use std::{collections::HashMap, fs};
use crate::tree::{Attr, AttrFlag, DBState, Expr, ExprTree, Lambda, SpType, Type};

fn binary_read_string(b: &[u8], index: &mut usize) -> Option<String> {
    let mut i = *index;
    while *b.get(i)? != 0 { i += 1; }
    let res = Some(String::from_utf8(Vec::from(b.get(*index..i)?)).ok()?);
    *index = i + 1;
    res
}
fn binary_write_string(s: &str) -> Vec<u8> {
    let mut b = s.as_bytes().to_vec();
    b.push(0);
    b
}

fn binary_read_expr(b: &[u8], index: &mut usize) -> Option<ExprTree> {
    *index += 1;
    match *b.get(*index - 1)? {
        1 => {
            let v = Some(ExprTree::IntLit(i64::from_le_bytes(
                b.get(*index..*index + 8)?.try_into().ok()?)));
            *index += 8; v
        }, 2 => {
            let mut res = vec![];
            while *b.get(*index)? != 0 {
                res.push(*b.get(*index)?);
                *index += 1;
            }
            *index += 1;
            Some(ExprTree::CharLit(String::from_utf8(res).ok()?.chars().nth(0)?))
        }, 3 => {
            *index += 1;
            Some(ExprTree::BoolLit(*b.get(*index - 1)? == 1))
        }, 4 => {
            let v = Some(ExprTree::DoubleLit(f64::from_le_bytes(b.get(*index..*index + 8)?.try_into().ok()?)));
            *index += 8; v
        }, 5 | 8 => {
            let is_array = *b.get(*index - 1)? == 5;
            let mut res_vec = vec![];
            while *b.get(*index)? != 0 {
                res_vec.push(Box::new(binary_read_expr(b, index)?));
            }
            *index += 1;
            Some(if is_array {ExprTree::ArrayLit(res_vec)} else {ExprTree::TupleLit(res_vec)})
        }, 6 => {
            let e = binary_read_expr(b, index)?;
            Some(ExprTree::JustLit(Box::new(e)))
        }, 7 => { Some(ExprTree::NothingLit) },
        9 => {
            let e = binary_read_string(b, index)?;
            Some(ExprTree::Ident(e))
        }, 10..=16 => {
            let op = *b.get(*index - 1)?;
            let (p1, p2) = (Box::new(binary_read_expr(b, index)?)
                , Box::new(binary_read_expr(b, index)?));
            Some(match op {
                10 => ExprTree::Plus(p1, p2),
                11 => ExprTree::Minus(p1, p2),
                12 => ExprTree::Mul(p1, p2),
                13 => ExprTree::Div(p1, p2),
                14 => ExprTree::Mod(p1, p2),
                15 => ExprTree::Exp(p1, p2),
                16 => ExprTree::Dot(p1, p2),
                _ => panic!("bug")
            })
        }, 17 => {
            let mut res_vec = vec![];
            let func = binary_read_expr(b, index)?;
            while *b.get(*index)? != 0 {
                res_vec.push(Box::new(binary_read_expr(b, index)?));
            }
            *index += 1;
            Some(ExprTree::Call(Box::new(func), res_vec))
        }, 18 => {
            *index += 1;
            let equal = *b.get(*index - 1)? == 1;
            let (p1, p2) = (Box::new(binary_read_expr(b, index)?)
                , Box::new(binary_read_expr(b, index)?));
            Some(ExprTree::Eq(equal, p1, p2))
        }, 19 => {
            *index += 2;
            let greater = *b.get(*index - 2)? == 1;
            let strict = *b.get(*index - 1)? == 1;
            let (p1, p2) = (Box::new(binary_read_expr(b, index)?)
                , Box::new(binary_read_expr(b, index)?));
            Some(ExprTree::Cmp(greater, strict, p1, p2))
        }, 20 => {
            let (p1, p2, p3) = (Box::new(binary_read_expr(b, index)?)
                , Box::new(binary_read_expr(b, index)?), Box::new(binary_read_expr(b, index)?));
            Some(ExprTree::IfExpr(p1, p2, p3))
        },
        _ => None
    }
}
fn binary_write_expr(t: &ExprTree) -> Vec<u8> {
    match t {
        ExprTree::IntLit(v) => {
            let mut res = vec![1];
            res.append(&mut v.to_le_bytes().to_vec());
            res},
        ExprTree::CharLit(c) => {
            let mut res = vec![2];
            res.append(&mut c.to_string().as_bytes().to_vec());
            res.push(0);
            res},
        ExprTree::BoolLit(b) => vec![3, if *b {1} else {0}],
        ExprTree::DoubleLit(d) => {
            let mut res = vec![4];
            res.append(&mut d.to_le_bytes().to_vec());
            res},
        ExprTree::ArrayLit(v) => {
            let mut res = vec![5];
            res.append(&mut v.iter().map(|x| binary_write_expr(x.as_ref())).flatten().collect());
            res.push(0);
            res},
        ExprTree::TupleLit(v) => {
            let mut res = vec![8];
            res.append(&mut v.iter().map(|x| binary_write_expr(x.as_ref())).flatten().collect());
            res.push(0);
            res},
        ExprTree::JustLit(v) => {
            let mut res = vec![6];
            res.append(&mut binary_write_expr(v));
            res},
        ExprTree::NothingLit => vec![7],
        ExprTree::Ident(s) => {
            let mut res = vec![9];
            res.append(&mut binary_write_string(s));
            res},
        ExprTree::Plus(c1, c2) => {
            let mut res = vec![10];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res},
        ExprTree::Minus(c1, c2) => {
            let mut res = vec![11];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res},
        ExprTree::Mul(c1, c2) => {
            let mut res = vec![12];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res},
        ExprTree::Div(c1, c2) => {
            let mut res = vec![13];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res},
        ExprTree::Mod(c1, c2) => {
            let mut res = vec![14];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res},
        ExprTree::Exp(c1, c2) => {
            let mut res = vec![15];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res},
        ExprTree::Dot(c1, c2) => {
            let mut res = vec![16];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res},
        ExprTree::Call(f, args) => {
            let mut res = vec![17];
            res.append(&mut binary_write_expr(f));
            res.append(&mut args.iter().map(|x| binary_write_expr(x.as_ref())).flatten().collect());
            res.push(0);
            res},
        ExprTree::Eq(b, c1, c2) => {
            let mut res = vec![18, if *b {1} else {0}];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res},
        ExprTree::Cmp(gr, st, c1, c2) => {
            let mut res = vec![19, if *gr {1} else {0}, if *st {1} else {0}];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res},
        ExprTree::IfExpr(c1, c2, c3) => {
            let mut res = vec![20];
            res.append(&mut binary_write_expr(c1));
            res.append(&mut binary_write_expr(c2));
            res.append(&mut binary_write_expr(c3));
            res},
    }
}

fn binary_read_type(b: &[u8], index: &mut usize) -> Option<SpType> {
    *index += 1;
    match *b.get(*index - 1)? {
        1 => {
            let str = binary_read_string(b, index)?;
            Some(SpType::Reg(Type::Object(str)))
        }, 2 => { Some(SpType::Reg(Type::Int)) },
        3 => { Some(SpType::Reg(Type::Char)) },
        4 => { Some(SpType::Reg(Type::Bool)) },
        5 => { Some(SpType::Reg(Type::Double)) },
        6 | 9 | 10 => {
            let code = *b.get(*index - 1)?;
            let mut res_vec = vec![];
            while *b.get(*index)? != 0 {
                res_vec.push(Box::new(match binary_read_type(b, index)? {
                    SpType::Reg(t) => t,
                    _ => return None
                }));
            }
            *index += 1;
            match code {
                6 => Some(SpType::Reg(Type::FuncType(res_vec))),
                9 => Some(SpType::Reg(Type::Tuple(res_vec))),
                10 => Some(SpType::Reg(Type::Sum(res_vec))),
                _ => panic!["bug"]
            }
        }, 7 | 8 => {
            let is_array = *b.get(*index - 1)? == 7;
            let v = match binary_read_type(b, index)? {
                SpType::Reg(t) => t,
                _ => return None
            };
            Some(if is_array {SpType::Reg(Type::Array(Some(Box::new(v))))}
                else {SpType::Reg(Type::Maybe(Box::new(v)))})
        }, 11 | 12 => {
            let is_gen = *b.get(*index - 1)? == 11;
            let v = Box::new(match binary_read_type(b, index)? {
                SpType::Reg(t) => t,
                _ => return None
            });
            let l = binary_parse_lambda(b, index)?;
            Some(if is_gen {SpType::Gen(v, l)} else {SpType::Restrict(v, l)})
        }, _ => None
    }
}
fn binary_write_type(t: &SpType) -> Vec<u8> {
    match t {
        SpType::Reg(Type::Object(s)) => {
            let mut res = vec![1];
            res.append(&mut binary_write_string(s));
            res},
        SpType::Reg(Type::Int) => vec![2],
        SpType::Reg(Type::Char) => vec![3],
        SpType::Reg(Type::Bool) => vec![4],
        SpType::Reg(Type::Double) => vec![5],
        SpType::Reg(Type::FuncType(v)) => {
            let mut res = vec![6];
            res.append(&mut v.iter().map(|x| binary_write_type(&SpType::Reg(*x.clone()))).flatten().collect());
            res.push(0);
            res},
            SpType::Reg(Type::Tuple(v)) => {
            let mut res = vec![9];
            res.append(&mut v.iter().map(|x| binary_write_type(&SpType::Reg(*x.clone()))).flatten().collect());
            res.push(0);
            res},
        SpType::Reg(Type::Sum(v)) => {
            let mut res = vec![10];
            res.append(&mut v.iter().map(|x| binary_write_type(&SpType::Reg(*x.clone()))).flatten().collect());
            res.push(0);
            res},
        SpType::Reg(Type::Array(v0)) => {
            let mut res = vec![7];
            match v0 {
                Some(v) => res.append(&mut binary_write_type(&SpType::Reg(*v.clone()))),
                None => {}
            }
            res},
        SpType::Reg(Type::Maybe(v)) => {
            let mut res = vec![8];
            res.append(&mut binary_write_type(&SpType::Reg(*v.clone())));
            res},
        SpType::Gen(t, l) => {
            let mut res = vec![11];
            res.append(&mut binary_write_type(&SpType::Reg(*t.clone())));
            res.append(&mut binary_write_lambda(l));
            res},
        SpType::Restrict(t, l) =>  {
            let mut res = vec![12];
            res.append(&mut binary_write_type(&SpType::Reg(*t.clone())));
            res.append(&mut binary_write_lambda(l));
            res}
    }
}

fn binary_parse_lambda(b: &[u8], index: &mut usize) -> Option<Lambda> {
    let mut res_vec = vec![];
    while *b.get(*index)? != 0 {
        res_vec.push((binary_read_string(b, index)?, match binary_read_type(b, index)? {
            SpType::Reg(t) => t,
            _ => return None
        }));
    }
    *index += 1;
    let expr = binary_read_expr(b, index)?;
    Some(Lambda { params: res_vec, code: Expr { tree: expr, exprType: None } })
}
fn binary_write_lambda(l: &Lambda) -> Vec<u8> {
    let mut res = vec![];
    for i in &l.params {
        res.append(&mut binary_write_string(&i.0));
        res.append(&mut binary_write_type(&SpType::Reg(i.1.clone())));
    }
    res.push(0);
    res.append(&mut binary_write_expr(&l.code.tree));
    res
}

fn binary_parse_attr(b: &[u8], index: &mut usize) -> Option<Attr> {
    let name = binary_read_string(b, index)?;
    let attrType = binary_read_type(b, index)?;
    let default = match b.get(*index) {
        Some(c) if *c != 0 => {
            *index += 1;
            Some(binary_parse_lambda(b, index)?)
        }, _ => {*index += 1; None}
    };
    let flag = match b.get(*index) {
        Some(0) => AttrFlag::None,
        Some(1) => AttrFlag::Computable,
        Some(2) => AttrFlag::Global,
        Some(3) => AttrFlag::Primary,
        _ => return None
    };
    *index += 1;
    Some(Attr {name, attrType, default, flag} )
}
fn binary_write_attr(a: &Attr) -> Vec<u8> {
    let mut res = vec![];
    res.append(&mut binary_write_string(&a.name));
    res.append(&mut binary_write_type(&a.attrType));
    match &a.default {
        Some(l) => {
            res.push(1);
            res.append(&mut binary_write_lambda(&l));
        }, None => res.push(0)
    }
    res.push(match &a.flag {
        AttrFlag::None => 0,
        AttrFlag::Computable => 1,
        AttrFlag::Global => 2,
        AttrFlag::Primary => 3
    });
    res
}

impl DBState {
    pub fn from_file(filename: &str) -> Option<Self> {
        let mut res = DBState { header: vec![], data: HashMap::new() };
        let bytestream = std::fs::read(filename).ok()?;
        let mut file_pos: usize = 0;
        while *bytestream.get(file_pos)? != 0 {
            let ent_name = binary_read_string(&bytestream, &mut file_pos)?;
            let mut attr_vec = vec![];
            while *bytestream.get(file_pos)? != 0 {
                attr_vec.push(binary_parse_attr(&bytestream, &mut file_pos)?);
            }
            file_pos += 1;
            res.header.push((ent_name, attr_vec));
        }
        file_pos += 1;
        while bytestream.get(file_pos) != None {
            let c1 = u64::from_le_bytes(bytestream.get(file_pos..file_pos + 8)?.try_into().ok()?);
            file_pos += 8;
            let c2 = u64::from_le_bytes(bytestream.get(file_pos..file_pos + 8)?.try_into().ok()?);
            file_pos += 8;
            let mut expr_vec = vec![];
            while *bytestream.get(file_pos)? != 0 {
                expr_vec.push(Expr {tree: binary_read_expr(&bytestream, &mut file_pos)?, exprType: None });
            }
            file_pos += 1;
            if res.data.contains_key(&(c1, c2)) { return None; }
            res.data.insert((c1, c2), expr_vec);
        }
        Some(res)
    }
    pub fn to_file(&self, filename: &str) -> Result<(), std::io::Error> {
        let mut res = vec![];
        for i in &self.header {
            res.append(&mut binary_write_string(&i.0));
            for j in &i.1 {
                res.append(&mut binary_write_attr(j));
            }
            res.push(0);
        }
        res.push(0);
        for i in &self.data {
            res.append(&mut u64::to_le_bytes(i.0.0).to_vec());
            res.append(&mut u64::to_le_bytes(i.0.1).to_vec());
            for j in i.1 {
                res.append(&mut binary_write_expr(&j.tree));
            }
            res.push(0);
        }
        fs::write(filename, res)?;
        Ok(())
    }
}