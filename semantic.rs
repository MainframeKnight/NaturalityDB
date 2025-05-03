use std::collections::HashMap;
use crate::{compute::compute, tree::{self, AttrFlag, DBState, ExprTree, Type}};

impl ToString for ExprTree {
    fn to_string(&self) -> String {
        match self {
            Self::IntLit(n) => n.to_string(),
            Self::BoolLit(n) => n.to_string(),
            Self::CharLit(n) => n.to_string(),
            Self::ArrayLit(v, _) => {
                match v.get(0).map(|x| *x.clone()) {
                    Some(Self::CharLit(_)) => {
                        let mut res = String::from("\"");
                        res.push_str(&v.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(""));
                        res.push('"');
                        return res;
                    }, _ => {}
                }
                let mut res = "[".to_string();
                for i in 0..v.len() {
                    res += &v[i].to_string();
                    if i != v.len() - 1 {
                        res += ", ";
                    }
                }
                res.push(']');
                res
            }, Self::TupleLit(v) => {
                let mut res = "(".to_string();
                for i in 0..v.len() {
                    res += &v[i].to_string();
                    if i != v.len() - 1 {
                        res += ", ";
                    }
                }
                res.push(')');
                res
            }, Self::DoubleLit(d) => d.to_string(),
            Self::JustLit(v) => format!["Just({})", v.to_string()],
            Self::NothingLit(_) => "Nothing".to_string(),
            _ => String::new()
        }
    }
}

fn get_tree_type(t: &mut ExprTree, db: &DBState, params: &HashMap<String, Type>) -> Result<Type, String> {
    match t {
        ExprTree::IntLit(_) => Ok(Type::Int),
        ExprTree::CharLit(_) => Ok(Type::Char),
        ExprTree::DoubleLit(_) => Ok(Type::Double),
        ExprTree::BoolLit(_) => Ok(Type::Bool),
        ExprTree::Ref(ent, attr, val, u, v) => {
            let ent_pos = db.header.iter().position(|x| x.0 == *ent).ok_or(
                format!["Reference to non-recognized entity '{}'.", ent])?;
            let attr_pos = db.header[ent_pos].1.iter().position(|x| x.name == *attr).ok_or(
                format!["Reference to non-recognized attribute '{}' of entity '{}'.", attr, ent])?;
            if db.header[ent_pos].1[attr_pos].flag != AttrFlag::Unique {
                return Err(format!["Reference to non-unique attribute '{}' of entity '{}'.", attr, ent]);
            }
            let val_type = val.get_type(db, params)?;
            if match &db.header[ent_pos].1[attr_pos].attrType {
                tree::SpType::Reg(t) => *t != val_type,
                tree::SpType::Gen(t, _, _) => **t != val_type,
                tree::SpType::Restrict(t, _) => **t != val_type
            } {
                return Err(format!["The referenced value doesn't have necessary type."]);
            }
            let mut found = false;
            for i in 0..db.data[&(ent_pos as u64, attr_pos as u64)].len() {
                match compute(&ExprTree::Eq(true, val.clone(), 
                    Box::new(db.data[&(ent_pos as u64, attr_pos as u64)][i].clone())), db, &HashMap::new())? {
                        ExprTree::BoolLit(true) => {
                            found = true;
                            *v = i as u64;
                        }, _ => {}
                }
            }
            if !found {
                return Err(format!["The referenced value is not present in entity '{}'.", ent]);
            }
            *u = ent_pos as u64;
            Ok(Type::Object(db.header[ent_pos].0.clone()))
        },
        ExprTree::LambdaExpr(lm) => lm.get_type(db, params),
        ExprTree::ArrayLit(t, opt_type) => {
            if t.len() == 0 {
                return Ok(Type::Array(Box::new(opt_type.clone().ok_or("type needed for [] literal")?)));
            }
            let elem_type = get_tree_type(&mut t[0], db, params)?;
            for i in &mut t[1..] {
                if elem_type != get_tree_type(i, db, params)? {
                    return Err("Array literals cannot contain elements of different types.".to_string());
                }
            }
            Ok(Type::Array(Box::new(elem_type)))
        }, ExprTree::TupleLit(v) => {
            let mut types = vec![];
            for i in v {
                types.push(Box::new(get_tree_type(i, db, params)?));
            }
            Ok(Type::Tuple(types))
        }, ExprTree::JustLit(t) => Ok(Type::Maybe(Box::new(get_tree_type(t, db, params)?))),
        ExprTree::NothingLit(t) => Ok(Type::Maybe(Box::new(t.clone()))),
        ExprTree::Ident(s) => {
            if params.contains_key(s) { return Ok(params[s].clone()); }
            return Err(format!["Unrecognized identifier: '{}'", s]);
        }, ExprTree::IfExpr(cond, b1, b2) => {
            let ct = get_tree_type(cond, db, params)?;
            if ct == Type::Bool {
                let (t1, t2) = (get_tree_type(b1, db, params)?, get_tree_type(b2, db, params)?);
                if t1 == t2 {
                    return Ok(t1);
                }
                return Ok(Type::Sum(vec![Box::new(t1), Box::new(t2)]));
            }
            return Err("if clause has a non-bool condition.".to_string());
        }, ExprTree::Mod(v1, v2) => {
            let (t1, t2) = (get_tree_type(v1, db, params)?, get_tree_type(v2, db, params)?);
            if t1 == Type::Int && t2 == Type::Int {
                Ok(Type::Int)
            } else {Err(format!["'{:?}' % '{:?}' is undefined.", t1, t2])}
        }, ExprTree::Exp(v1, v2) => {
            let (t1, t2) = (get_tree_type(v1, db, params)?, get_tree_type(v2, db, params)?);
            if (t1 == Type::Int || t1 == Type::Double) && (t2 == Type::Int || t2 == Type::Double) {
                Ok(if t1 == Type::Double || t2 == Type::Double {Type::Double} else {Type::Int})
            } else {Err(format!["'{:?}' ^ '{:?}' is undefined.", t1, t2])}
        }, ExprTree::Mul(v1, v2) => {
            let (t1, t2) = (get_tree_type(v1, db, params)?, get_tree_type(v2, db, params)?);
            if (t1 == Type::Int || t1 == Type::Double) && (t2 == Type::Int || t2 == Type::Double) {
                Ok(if t1 == Type::Double || t2 == Type::Double {Type::Double} else {Type::Int})
            } else {Err(format!["'{:?}' * '{:?}' is undefined.", t1, t2])}
        }, ExprTree::Div(v1, v2) => {
            let (t1, t2) = (get_tree_type(v1, db, params)?, get_tree_type(v2, db, params)?);
            if (t1 == Type::Int || t1 == Type::Double) && (t2 == Type::Int || t2 == Type::Double) {
                Ok(if t1 == Type::Double || t2 == Type::Double {Type::Double} else {Type::Int})
            } else {Err(format!["'{:?}' / '{:?}' is undefined.", t1, t2])}
        }, ExprTree::Minus(v1, v2) => {
            let (t1, t2) = (get_tree_type(v1, db, params)?, get_tree_type(v2, db, params)?);
            if (t1 == Type::Int || t1 == Type::Double) && (t2 == Type::Int || t2 == Type::Double) {
                Ok(if t1 == Type::Double || t2 == Type::Double {Type::Double} else {Type::Int})
            } else {Err(format!["'{:?}' - '{:?}' is undefined.", t1, t2])}
        }, ExprTree::Plus(v1, v2) => {
            let (t1, t2) = (get_tree_type(v1, db, params)?, get_tree_type(v2, db, params)?);
            if (t1 == Type::Int || t1 == Type::Double) && (t2 == Type::Int || t2 == Type::Double) {
                return Ok(if t1 == Type::Double || t2 == Type::Double {Type::Double} else {Type::Int});
            } else if let Type::Tuple(ref tup1) = t1 {
                if let Type::Tuple(ref tup2) = t2 {
                    return Ok(Type::Tuple([tup1.clone(), tup2.clone()].concat()));
                }
            } else if let Type::Array(ref e1) = t1 {
                if let Type::Array(ref e2) = t2 {
                    if e1 == e2 { return Ok(Type::Array(Box::new(t1))); }
                }
            }
            return Err(format!["'{:?}' + '{:?}' is undefined.", t1, t2]);
        }, ExprTree::Eq(_, v1, v2) => {
            let mut comp = true;
            let (t1, t2) = (get_tree_type(v1, db, params)?, get_tree_type(v2, db, params)?);
            match t1.clone() {
                Type::Object(_) | Type::FuncType(_) | Type::Sum(_) => comp = false,
                Type::Array(t) => if t2 != Type::Array(t) {comp = false;}
                Type::Bool => if t2 != Type::Bool {comp = false;}
                Type::Int | Type::Char => if t2 != Type::Int && t2 != Type::Char {comp = false;}
                Type::Tuple(v) => if t2 != Type::Tuple(v) {comp = false;}
                Type::Maybe(v) => if t2 != Type::Maybe(v) {comp = false;}
                Type::Double => if t2 == Type::Double || t2 == Type::Int {
                    return Err("Comparing double with int or double is disallowed because it is inexact.".to_string());
                } else {comp = false;}
            }
            if comp {Ok(Type::Bool)} else {Err(format!["types '{:?}' and '{:?}' are noncomparable for equality.", t1, t2])}
        }, ExprTree::Cmp(_, _, v1, v2) => {
            let (t1, t2) = (get_tree_type(v1, db, params)?, get_tree_type(v2, db, params)?);
            if (t1 == Type::Int || t1 == Type::Char) && (t2 == Type::Int || t2 == Type::Char) {
                return Ok(Type::Bool);
            } else if (t1 == Type::Int || t1 == Type::Double) && (t2 == Type::Int || t2 == Type::Double) {
                return Ok(Type::Bool);
            }
            Err(format!["types '{:?}' and '{:?}' are noncomparable.", t1, t2])
        }, ExprTree::For(ent, lm) => {
            if db.header.iter().position(|(x, _)| *x == *ent) == None {
                return Err(format!["reference to non-recognized entity in 'for'."]);
            }
            let lm_type = lm.get_type(db, params)?;
            match lm_type {
                Type::FuncType(v) => {
                    if **v.get(0).ok_or(format!["wrong type of lambda in 'for'."])? == Type::Object(ent.clone()) {
                        match **v.get(1).ok_or(format!["wrong type of lambda in 'for'."])? {
                            Type::Maybe(ref t) => {
                                return Ok(Type::Array(t.clone()));
                            }, _ => {}
                        }
                    }
                }, _ => {}
            }
            return Err(format!["wrong type of lambda in 'for'."]);
        }, ExprTree::Call(f, args) => {
            match get_tree_type(f, db, params)? {
                Type::FuncType(v) => {
                    let mut arg_types = vec![];
                    for x in args {
                        arg_types.push(Box::new(get_tree_type(x, db, params)?));
                    }
                    if v[0..v.len() - 1] == arg_types {
                        return Ok(*v[0].clone());
                    }
                    Err("Parameter type mismatch in call.".to_string())
                }, _ => Err("Non-function type cannot be called.".to_string())
            }
        }, ExprTree::Dot(v1, v2) => {
            match **v2 {
                ExprTree::Ident(ref s2) => {
                    if let ExprTree::Ident(ref s1) = **v1 {
                        if s1 == "Std" {
                            // std functions go here.
                            return Err(format!["Unrecognized standard identifier '{}'", s2])
                        } else {      
                            if let Some((_, ent)) = db.header.iter().find(|(x, _)| *x == *s1) {
                                if let Some(a) = ent.iter().find(|x| x.name == *s2) {
                                    if let AttrFlag::Global = a.flag {
                                        return match &a.attrType {
                                            tree::SpType::Reg(r) => Ok(r.clone()),
                                            tree::SpType::Restrict(t, _) => Ok(*t.clone()),
                                            tree::SpType::Gen(t, _, _) => Ok(*t.clone())
                                        };
                                    }
                                }
                            }
                        }
                    }
                    if let Type::Object(ent) = get_tree_type(v1, db, params)? {
                        if let Some((_, ent_v)) = db.header.iter().find(|(x, _)| *x == ent) {
                            if let Some(a) = ent_v.iter().find(|x| x.name == *s2) {
                                return match &a.attrType {
                                    tree::SpType::Reg(r) => Ok(r.clone()),
                                    tree::SpType::Restrict(t, _) => Ok(*t.clone()),
                                    tree::SpType::Gen(t, _, _) => Ok(*t.clone())
                                };
                            }
                        }
                        return Err(format!["Unrecognized or incorrect entity reference of attribute '{}' of entity '{}'.", s2, ent]);
                    }
                    Err(format!["'.' applied to non-identifier."])
                }, _ => Err(format!["'.' applied to non-identifier."])
            }
        }
    }
}

impl tree::ExprTree {
    pub fn get_type(&mut self, db: &DBState, params: &HashMap<String, Type>) -> Result<Type, String> {
        get_tree_type(self, db, params)
    }
}

impl tree::Lambda {
    pub fn get_type(&mut self, db: &DBState, params: &HashMap<String, Type>) -> Result<Type, String> {
        let mut params_ht = params.clone();
        let mut param_types = Vec::new();
        for i in &self.params {
            tree::SpType::Reg(i.1.clone()).check(db)?;
            param_types.push(Box::new(i.1.clone()));
            params_ht.insert(i.0.clone(), i.1.clone());
        }
        param_types.push(Box::new(self.code.get_type(db, &params_ht)?));
        Ok(Type::FuncType(param_types))
    }
}

impl tree::SpType {
    pub fn check(&mut self, db: &DBState) -> Result<(), String> {
        match self {
            tree::SpType::Gen(t, func, _) => {
                tree::SpType::Reg(*t.clone()).check(db)?;
                if func.get_type(db, &HashMap::new())? == Type::FuncType(vec![Box::new(Type::Int), Box::new(*t.clone())]) {} else {
                    return Err("Incorrect type of Gen function.".to_string());
                }
            }, tree::SpType::Restrict(t, func) => {
                tree::SpType::Reg(*t.clone()).check(db)?;
                if func.get_type(db, &HashMap::new())? == Type::FuncType(vec![Box::new(*t.clone()), Box::new(Type::Bool)]) {} else {
                    return Err("Incorrect type of Restrict function.".to_string());
                }
            }, tree::SpType::Reg(t) => {
                match t {
                    tree::Type::Bool | tree::Type::Char | tree::Type::Double | tree::Type::Int => {},
                    tree::Type::Array(t) | tree::Type::Maybe(t) => tree::SpType::Reg(*t.clone()).check(db)?,
                    tree::Type::Tuple(v) | tree::Type::FuncType(v) | tree::Type::Sum(v) =>
                        for t in v {
                            tree::SpType::Reg(*t.clone()).check(db)?;
                        },
                    tree::Type::Object(s) => if let None = db.header.iter().find(|x| x.0 == *s) {
                        return Err("Object type refers to a non-recognized entity.".to_string());
                    }
                }
            }
        }
        Ok(())
    }
}