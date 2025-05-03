use std::collections::HashMap;
use crate::tree::{self, ExprTree};

fn comp<T: std::cmp::PartialOrd>(g: &bool, ns: &bool, e1: &T, e2: &T) -> bool {
    if *g {if *ns {*e1 >= *e2} else {*e1 > *e2}} else {if *ns {*e1 <= *e2} else {*e1 < *e2}}
}
fn int_double_op(r1: &ExprTree, r2: &ExprTree, op: char) -> Result<ExprTree, String> {
    match r1 {
        ExprTree::IntLit(v) => {
            match r2 {
                ExprTree::DoubleLit(v2) => return Ok(ExprTree::DoubleLit( if op == '*' 
                    {*v as f64 * v2} else if op == '/' {*v as f64 / v2} else if op == '-' {*v as f64 - v2}
                    else {(*v as f64).powf(*v2)}
                )),
                ExprTree::IntLit(v2) => return Ok(ExprTree::IntLit(if op == '*' 
                    {v * v2} else if op == '/' {v / v2} else if op == '-' {v - v2} else {v.pow((*v2).try_into().unwrap())})),
                _ => return Err(format!["Type mismatch in {}", op])
            }
        }, ExprTree::DoubleLit(v) => {
            match r2 {
                ExprTree::DoubleLit(v2) => return Ok(ExprTree::DoubleLit(if op == '*' 
                    {v * v2} else if op == '/' {v / v2} else if op == '-' {v - v2} else {(*v).powf(*v2)})),
                ExprTree::IntLit(v2) => return Ok(ExprTree::DoubleLit(if op == '*' 
                    {v * (*v2 as f64)} else if op == '/' {v / *v2 as f64} else if op == '-' {v - *v2 as f64}
                    else {(*v).powf(*v2 as f64)})),
                _ => return Err(format!["Type mismatch in {}", op])
            }
        }, _ => return Err(format!["Type mismatch in {}", op])
    }
}

pub fn find_refs(expr: &ExprTree) -> Vec<(u64, u64)> {
    match expr {
        ExprTree::ArrayLit(v, _) | ExprTree::TupleLit(v) => v.iter().map(|x| find_refs(x)).flatten().collect(),
        ExprTree::Ref(_, _, _, u1, u2) => vec![(*u1, *u2)],
        ExprTree::JustLit(v) => find_refs(v),
        ExprTree::Plus(u, v) | ExprTree::Minus(u, v) | ExprTree::Div(u, v) |
            ExprTree::Mod(u, v) | ExprTree::Dot(u, v) | ExprTree::Mul(u, v) |
            ExprTree::Exp(u, v) | ExprTree::Eq(_, u, v) | ExprTree::Cmp(_, _, u, v) => {
                let mut temp = find_refs(u);
                temp.append(&mut find_refs(v));
                temp
            }, ExprTree::Call(u, v) => {
                let mut temp: Vec<_> = v.iter().map(|x| find_refs(x)).flatten().collect();
                temp.append(&mut find_refs(u));
                temp
            }, ExprTree::IfExpr(a, b, c) => {
                let mut temp = find_refs(a);
                temp.append(&mut find_refs(b));
                temp.append(&mut find_refs(c));
                temp
            }, ExprTree::LambdaExpr(lm) => find_refs(&lm.code),
            _ => vec![]
    }
}

pub fn compute(expr: &ExprTree, db: &tree::DBState, params: &HashMap<String, tree::ExprTree>) -> Result<ExprTree, String> {
    match expr {
        ExprTree::BoolLit(_) | ExprTree::CharLit(_) | ExprTree::DoubleLit(_) |
        ExprTree::IntLit(_)| ExprTree::NothingLit(_)| ExprTree::LambdaExpr(_) | ExprTree::Ref(_, _, _, _, _) => Ok(expr.clone()),
        ExprTree::JustLit(v) => Ok(ExprTree::JustLit(Box::new(compute(&v, db, params)?))),
        ExprTree::ArrayLit(v, t) => {
            let mut res_vec = vec![];
            for i in v {
                res_vec.push(Box::new(compute(&i, db, params)?));
            }
            Ok(ExprTree::ArrayLit(res_vec, t.clone()))
        }, ExprTree::TupleLit(v) => {
            let mut res_vec = vec![];
            for i in v {
                res_vec.push(Box::new(compute(&i, db, params)?));
            }
            Ok(ExprTree::TupleLit(res_vec))
        }, ExprTree::IfExpr(cond, e1, e2 ) => {
            match compute(cond, db, params)? {
                ExprTree::BoolLit(true) => return compute(e1, db, params),
                _ => return compute(e2, db, params)
            }
        }, ExprTree::Mod(e1, e2) => {
            let (r1, r2) = (compute(e1, db, params)?, compute(e2, db, params)?);
            if let (ExprTree::IntLit(val1), ExprTree::IntLit(val2)) = (r1, r2) {
                return Ok(ExprTree::IntLit(val1 % val2));
            }
            return Err("Type mismatch in %".to_string());
        }, ExprTree::Mul(e1, e2) => {
            let (r1, r2) = (compute(e1, db, params)?, compute(e2, db, params)?);
            return int_double_op(&r1, &r2, '*');
        }, ExprTree::Div(e1, e2) => {
            let (r1, r2) = (compute(e1, db, params)?, compute(e2, db, params)?);
            return int_double_op(&r1, &r2, '/');
        }, ExprTree::Exp(e1, e2) => {
            let (r1, r2) = (compute(e1, db, params)?, compute(e2, db, params)?);
            return int_double_op(&r1, &r2, '^');
        }, ExprTree::Minus(e1, e2) => {
            let (r1, r2) = (compute(e1, db, params)?, compute(e2, db, params)?);
            return int_double_op(&r1, &r2, '-');
        }, ExprTree::Plus(e1, e2) => {
            let (r1, r2) = (compute(e1, db, params)?, compute(e2, db, params)?);
            match r1 {
                ExprTree::ArrayLit(v, t) => {
                    match r2 {
                        ExprTree::ArrayLit(v2, _) => return Ok(ExprTree::ArrayLit([v, v2].concat(), t)),
                        _ => return Err("Type mismatch in +".to_string())
                    }
                }, ExprTree::TupleLit(v) => {
                    match r2 {
                        ExprTree::TupleLit(v2) => return Ok(ExprTree::TupleLit([v, v2].concat())),
                        _ => return Err("Type mismatch in +".to_string())
                    }
                }, ExprTree::IntLit(v1) => {
                    match r2 {
                        ExprTree::DoubleLit(v2) => return Ok(ExprTree::DoubleLit(v1 as f64 + v2)),
                        ExprTree::IntLit(v2) => return Ok(ExprTree::IntLit(v1 + v2)),
                        _ => return Err("Type mismatch in +".to_string())
                    }
                }, ExprTree::DoubleLit(v1) => {
                    match r2 {
                        ExprTree::DoubleLit(v2) => return Ok(ExprTree::DoubleLit(v1 + v2)),
                        ExprTree::IntLit(v2) => return Ok(ExprTree::DoubleLit(v1 + v2 as f64)),
                        _ => return Err("Type mismatch in +".to_string())
                    }
                }, _ => return Err("Type mismatch in +".to_string())
            }
        }, ExprTree::Cmp(greater, nons, e1, e2) => {
            let (r1, r2) = (compute(e1, db, params)?, compute(e2, db, params)?);
            match r1 {
                ExprTree::IntLit(v) => {
                    match r2 {
                        ExprTree::CharLit(v2) => return Ok(ExprTree::BoolLit(comp(greater, nons, &v, &(v2 as i64)))),
                        ExprTree::DoubleLit(v2) => return Ok(ExprTree::BoolLit(comp(greater, nons, &(v as f64), &v2))),
                        ExprTree::IntLit(v2) => return Ok(ExprTree::BoolLit(comp(greater, nons, &v, &v2))),
                        _ => return Err("Type mismatch in cmp".to_string())
                    }
                }, ExprTree::CharLit(v) => {
                    match r2 {
                        ExprTree::CharLit(v2) => return Ok(ExprTree::BoolLit(comp(greater, nons, &v, &v2))),
                        ExprTree::IntLit(v2) => return Ok(ExprTree::BoolLit(comp(greater, nons, &(v as i64), &v2))),
                        _ => return Err("Type mismatch in cmp".to_string())
                    }
                }, ExprTree::DoubleLit(v) => {
                    match r2 {
                        ExprTree::DoubleLit(v2) => return Ok(ExprTree::BoolLit(comp(greater, nons, &v, &v2))),
                        ExprTree::IntLit(v2) => return Ok(ExprTree::BoolLit(comp(greater, nons, &v, &(v2 as f64)))),
                        _ => return Err("Type mismatch in cmp".to_string())
                    }
                }, _ => return Err("Type mismatch in cmp".to_string())
            }
        }, ExprTree::Eq(eq, e1, e2) => {
            let (r1, r2) = (compute(e1, db, params)?, compute(e2, db, params)?);
            match r1 {
                ExprTree::ArrayLit(vals, _) => {
                    match r2 {
                        ExprTree::ArrayLit(vals2, _) => {
                            let mut ok = true;
                            if vals.len() != vals2.len() { return Ok(ExprTree::BoolLit(!*eq)); }
                            for i in 0..vals.len() {
                                match compute(&ExprTree::Eq(true, vals[i].clone(), vals2[i].clone()), db, params)? {
                                    ExprTree::BoolLit(true) => {},
                                    _ => {ok = false; break;}
                                }
                            }
                            return Ok(ExprTree::BoolLit(if *eq {ok} else {!ok}));
                        },
                        _ => return Err("Type mismatch".to_string())
                    }
                }, ExprTree::BoolLit(b1) => {
                    match r2 {
                        ExprTree::BoolLit(b2) => return Ok(ExprTree::BoolLit(if *eq {b1 == b2} else {b1 != b2})),
                        _ => return Err("Type mismatch".to_string())
                    }
                }, ExprTree::IntLit(b1) => {
                    match r2 {
                        ExprTree::IntLit(b2) => return Ok(ExprTree::BoolLit(if *eq {b1 == b2} else {b1 != b2})),
                        _ => return Err("Type mismatch".to_string())
                    }
                }, ExprTree::CharLit(b1) => {
                    match r2 {
                        ExprTree::CharLit(b2) => return Ok(ExprTree::BoolLit(if *eq {b1 == b2} else {b1 != b2})),
                        _ => return Err("Type mismatch".to_string())
                    }
                }, ExprTree::TupleLit(vals) => {
                    match r2 {
                        ExprTree::TupleLit(vals2) => {
                            let mut ok = true;
                            if vals.len() != vals2.len() { return Err("Type mismatch".to_string()); }
                            for i in 0..vals.len() {
                                match compute(&ExprTree::Eq(true, vals[i].clone(), vals2[i].clone()), db, params)? {
                                    ExprTree::BoolLit(true) => {},
                                    _ => {ok = false; break;}
                                }
                            }
                            return Ok(ExprTree::BoolLit(if *eq {ok} else {!ok}));
                        },
                        _ => return Err("Type mismatch".to_string())
                    }
                }, ExprTree::NothingLit(_) => {
                    return Ok(ExprTree::BoolLit(match r2 {
                        ExprTree::NothingLit(_) => *eq, _ => !*eq
                    }))
                }, ExprTree::JustLit(v) => {
                    match r2 {
                        ExprTree::JustLit(v2) => return Ok(ExprTree::BoolLit(
                            match compute(&ExprTree::Eq(*eq, v, v2), db, params)? {
                                ExprTree::BoolLit(true) => true, _ => false
                            })),
                        _ => return Err("Type mismatch".to_string())
                    }
                }, _ => return Err("Type mismatch".to_string())
            }
        }, ExprTree::Ident(name) => {
            match params.get(name) {
                Some(e) => Ok(e.clone()),
                None => return Err(format!["Undefined identifier '{}'", name])
            }
        }, ExprTree::For(ent, lm) => {
            let pos = db.header.iter().position(|(x, _)| *x == *ent).unwrap();
            let mut res = vec![];
            let iter_len = match db.data.get(&(pos as u64, 0)) {
                Some(a) => a.len(),
                None => return Ok(ExprTree::ArrayLit(vec![], None))
            };
            for i in 0..iter_len {
                let mut new_params = params.clone();
                new_params.insert(lm.params[0].0.clone(), ExprTree::Ref(String::new(), String::new(), Box::new(ExprTree::TupleLit(vec![])), pos as u64, i as u64));
                match compute(&lm.code, db, &new_params)? {
                    ExprTree::JustLit(t) => res.push(t),
                    _ => {}
                }
            }
            Ok(ExprTree::ArrayLit(res, None))
        }, ExprTree::Call(e1, e2) => {
            match **e1 {
                ExprTree::LambdaExpr(ref lm) => {
                    let mut new_params = params.clone();
                    for i in 0..lm.params.len() {
                        new_params.insert(lm.params[i].0.clone(), compute(&e2[i], db, params)?);
                    }
                    compute(&lm.code, db, &new_params)
                }, _ => return Err("Type mismatch in call".to_string())
            }
        }, ExprTree::Dot(e1, e2) => {
            if let (ExprTree::Ident(s1), ExprTree::Ident(s2)) = (*e1.clone(), *e2.clone()) {
                if s1 == "Std" {
                    // Implementation of std functions.
                    return Err(format!["Std function '{}' not found.", s1]);
                } else if let Some((_, ent)) = db.header.iter().find(|(x, _)| *x == *s1) {
                    if let Some(a) = ent.iter().find(|x| x.name == *s2) {
                        if let tree::AttrFlag::Global = a.flag {
                            return Err(format!["Globals not supported yet."]);
                        }
                    }
                }
            }
            if let (t, ExprTree::Ident(s)) = (*e1.clone(), *e2.clone()) {
                match compute(&t, db, params)? {
                    ExprTree::Ref(_, _, _, ent, pos) => {
                        if let Some(attr) = db.header[ent as usize].1.iter().position(|x| x.name == *s) {
                            return Ok(db.data[&(ent, attr as u64)][pos as usize].clone());
                        }
                        return Err(format!["Incorrect reference in dot opeartor"])
                    }, _ => return Err(format!["Incorrect application of dot opeartor"])
                }
            }
            return Err(format!["Incorrect application of dot opeartor"]);
        }
    }
}