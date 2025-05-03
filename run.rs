use std::collections::{HashMap, HashSet};

use crate::{compute::{compute, find_refs}, tree::{self, ExprTree, Type}};

impl tree::Command {
    pub fn complete(&mut self, mut db: tree::DBState) -> Result<tree::DBState, String> {
        match self {
            tree::Command::Open(name) => {
                tree::DBState::from_file(name).ok_or(format!["Error opening file '{}'", name])
            }, tree::Command::Commit(name) => {
                db.to_file(name).map_err(|x| x.to_string())?;
                Ok(db)
            }, tree::Command::NewEntity(name, attrs) => {
                match db.header.iter().position(|x| x.0 == *name) {
                    Some(_) => Err(format!["The entity '{}' already exists.", name]),
                    None => {
                        let mut attr_names = HashSet::new();
                        for i in &mut *attrs {
                            if attr_names.contains(&i.name) {
                                return Err(format!["Unable to create entity '{}' with identical attribute names '{}'.",
                                    name, i.name]);
                            }
                            if let Some(_) = i.default {
                                return Err(format!["Default values only allowed in new attributes in Reshape."]);
                            }
                            attr_names.insert(i.name.clone());
                        }
                        db.header.push((name.clone(), attrs.to_vec()));
                        Ok(db)
                    }
                }
            }, tree::Command::Drop(ent) => {
                match db.header.iter().position(|x| x.0 == *ent) {
                    Some(pos) => {
                        db.header.swap_remove(pos);
                        let mut rem_v = vec![];
                        for ((ent,ht_pos),_) in db.data.iter().by_ref() {
                            if *ent == pos as u64 {
                                rem_v.push((*ent, *ht_pos));
                            }
                        }
                        for i in rem_v {
                            for j in 0..db.data[&i].len() as u64 {
                                for k in find_refs(&db.data[&i][j as usize]) {
                                    if db.ref_list[&k] == 1 {
                                        db.ref_list.remove(&k);
                                    } else {
                                        *db.ref_list.get_mut(&k).unwrap() -= 1;
                                    }
                                }
                                if db.ref_list.contains_key(&(i.0, j)) {
                                    return Err(format!["Unable to drop '{}' as it is bound by reference constraint.", ent]);
                                }
                            }
                            db.data.remove(&i);
                        }
                        Ok(db)
                    },
                    None => Err(format!["Unable to drop a non-recognized entity '{}'.", ent])
                }
            }, tree::Command::Add(ent, vals) => {
                let ent_num = db.header.iter().position(|x| x.0 == *ent)
                    .ok_or(format!["Unable to find entity '{}' in Add command.", ent])?;
                let mut ent_attrs = db.header[ent_num].1.clone();
                let mut new_vals = vec![vec![]; ent_attrs.len()];
                let mut num_of_vals = -1;
                for (attr, data) in vals {
                    if num_of_vals != -1 && data.len() as i32 != num_of_vals {
                        return Err(format!["Different number of values in different attrbutes in Add."]);
                    }
                    let cur_attr = ent_attrs.iter().position(|x| x.name == *attr)
                        .ok_or(format!["Entity '{}' doesn't contain the attribute '{}'.", ent, attr])?;
                    if let tree::AttrFlag::Computable | tree::AttrFlag::Global = ent_attrs[cur_attr].flag {
                        return Err(format!["Unable to add values to global/computable attribute '{}'.", 
                            ent_attrs[cur_attr].name]);
                    }
                    for i in &mut *data {
                        let mut i_value = compute(i, &db, &HashMap::new())?;
                        let i_type = i_value.get_type(&db, &HashMap::new())?;
                        match &ent_attrs[cur_attr].attrType {
                            tree::SpType::Reg(t) => if *t != i_type {
                                return Err(format!["Unable to add a value of type '{:?}' to attribute '{}' of type '{:?}'.",
                                    i_type, attr, t]);
                            } else {},
                            tree::SpType::Gen(_, _, _) => return Err("Unable to add values to Gen-type attribute.".to_string()),
                            tree::SpType::Restrict(t, p) => {
                                if **t != i_type {
                                    return Err(format!["Unable to add a value of type '{:?}' to attribute '{}' of restricted type '{:?}'.",
                                        i_type, attr, t]);
                                } else {
                                    match compute(&ExprTree::Call(Box::new(ExprTree::LambdaExpr(Box::new(p.clone()))),
                                        vec![Box::new(i.clone())]), &db, &HashMap::new())? {
                                            ExprTree::BoolLit(true) => {},
                                            _ => return Err(format!["Unable to add value not satisfying restriction of attribute '{}'",
                                                attr])
                                    }
                                }
                            }
                        }
                        new_vals[cur_attr].push(i.clone());
                    }
                    if num_of_vals == -1 {
                        num_of_vals = data.len() as i32;
                    }
                }
                for i in 0..new_vals.len() {
                    match ent_attrs[i].flag {
                        tree::AttrFlag::Computable => return Err(format!["Computable attributes not supported."]),
                        tree::AttrFlag::Global => continue,
                        tree::AttrFlag::None => {},
                        tree::AttrFlag::Unique => { 
                            if db.data.contains_key(&(ent_num as u64, i as u64)) {
                                for val in db.data[&(ent_num as u64, i as u64)].iter().as_ref() {
                                for add_val in new_vals[i].iter().as_ref() {
                                    match compute(&ExprTree::Eq(true, Box::new(val.clone()), Box::new(add_val.clone())), &db, &HashMap::new())? {
                                        ExprTree::BoolLit(true) => return Err(format!["Unable to add existing value to a unique attribute '{}' of entity '{}'.",
                                                ent_attrs[i].name, ent]),
                                        _ => {}
                                    }
                                }}
                            }
                        }
                    }
                    if new_vals[i].len() == 0 {
                        return Err(format!["Attribute '{}' not specified in Add.", ent_attrs[i].name]);
                    }
                    match &mut ent_attrs[i].attrType {
                        tree::SpType::Gen(_, lm, cnt) => {
                            for _ in 0..new_vals[i].len() {
                                let res = compute(&ExprTree::Call(Box::new(ExprTree::LambdaExpr(Box::new(lm.clone()))),
                                vec![Box::new(ExprTree::IntLit(*cnt as i64))]), &db, &HashMap::new())?;
                                if !db.data.contains_key(&(ent_num as u64, i as u64)) {
                                    db.data.insert((ent_num as u64, i as u64), vec![]);
                                }
                                db.data.get_mut(&(ent_num as u64, i as u64)).unwrap().push(res.clone());
                                for rf in find_refs(&res) {
                                    if db.ref_list.contains_key(&rf) {
                                        *db.ref_list.get_mut(&rf).unwrap() += 1;
                                    } else {
                                        db.ref_list.insert(rf, 1);
                                    }
                                }
                                *cnt += 1;
                            }
                        }, _ => {
                            if !db.data.contains_key(&(ent_num as u64, i as u64)) {
                                db.data.insert((ent_num as u64, i as u64), vec![]);
                            }
                            db.data.get_mut(&(ent_num as u64, i as u64)).unwrap().append(&mut new_vals[i]);
                            for x in &new_vals[i] {
                                for rf in find_refs(&x) {
                                    if db.ref_list.contains_key(&rf) {
                                        *db.ref_list.get_mut(&rf).unwrap() += 1;
                                    } else {
                                        db.ref_list.insert(rf, 1);
                                    }
                                }
                            }
                        }
                    }
                }
                Ok(db)
            }, tree::Command::Delete(ent, lm) => {
                match db.header.iter().position(|x| x.0 == *ent) {
                    Some(pos) => {
                        match lm.get_type(&db, &HashMap::new())? {
                            Type::FuncType(v) if v == vec![Box::new(Type::Object(ent.clone())), 
                                Box::new(Type::Bool)] => {},
                            q => return Err(format!["Incorrect type of lambda in Delete: '{:?}'", q])
                        }
                        let mut deletes = vec![];
                        for i in 0..db.data[&(0, 0)].len() {
                            match compute(&ExprTree::Call(Box::new(ExprTree::LambdaExpr(Box::new(lm.clone())))
                            , vec![Box::new(ExprTree::Ref(String::new(), String::new(), Box::new(ExprTree::TupleLit(vec![])), pos as u64, i as u64))]), &db, &HashMap::new())? {
                                ExprTree::BoolLit(true) => deletes.push(i), _ => {}
                            }
                        }
                        deletes.sort();
                        for i in 0..db.header[pos].1.len() {
                            let mut index = 0;
                            let mut err = false;
                            db.data.get_mut(&(pos as u64, i as u64)).ok_or("hash table key error")?.retain(|expr| {
                                let res = deletes.binary_search(&index);
                                index += 1;
                                if let Ok(_) = res {
                                    for k in find_refs(expr) {
                                        if db.ref_list[&k] == 1 {
                                            db.ref_list.remove(&k);
                                        } else {
                                            *db.ref_list.get_mut(&k).unwrap() -= 1;
                                        }   
                                    }
                                    if db.ref_list.contains_key(&(pos as u64, (index - 1) as u64)) {
                                        err = true;
                                    }
                                    false
                                } else {true}
                            });
                            if err {
                                return Err(format!["Unable to delete from '{}' as it contains a reference-bound value.", ent]);
                            }
                        }
                        Ok(db)
                    },
                    None => Err(format!["Unable to delete from a non-recognized entity '{}'.", ent])
                }
            }, tree::Command::Eval(expr) => {
                expr.get_type(&db, &HashMap::new())?;
                let res = compute(expr, &db, &HashMap::new())?;
                println!("{}", res.to_string());
                Ok(db)
            }, tree::Command::Trans(ent, cond, attrs) => {
                match db.header.iter().position(|x| x.0 == *ent) {
                    Some(pos) => {
                        if cond.get_type(&db, &HashMap::new())? != Type::FuncType(vec![
                            Box::new(Type::Object(ent.clone())), Box::new(Type::Bool)
                        ]) {return Err(format!["Incorrect type of predicate lambda in Transform."])}
                        for (s, lm) in attrs.as_mut_slice() {
                            match db.header[pos].1.iter().position(|x| x.name == *s) {
                                Some(a) => {
                                    match &db.header[pos].1[a].flag {
                                        tree::AttrFlag::Computable | tree::AttrFlag::Global => return Err(format![
                                            "Unable to modify computable/global attribute '{}' in Transform.", s]),
                                        _ => {}
                                    }
                                    let target_type = match &db.header[pos].1[a].attrType {
                                        tree::SpType::Reg(t) => t.clone(),
                                        tree::SpType::Restrict(t, _) => *t.clone(),
                                        tree::SpType::Gen(t, _, _) => *t.clone()
                                    };
                                    if lm.get_type(&db, &HashMap::new())? != Type::FuncType(vec![
                                        Box::new(Type::Object(ent.clone())), Box::new(target_type)
                                    ]) {return Err(format!["Incorrect type of transformation lambda on attribute '{}'
                                        of entity '{}' in Transform.", s, ent])}
                                }, None => return Err(format!["Attribute '{}' not found in entity '{}' in Transform.",
                                    s, ent])
                            }
                        }
                        for i in 0..db.data[&(pos as u64, 0 as u64)].len() {
                            match compute(&ExprTree::Call(Box::new(ExprTree::LambdaExpr(Box::new(cond.clone()))),
                                vec![Box::new(ExprTree::Ref(String::new(), String::new(), Box::new(ExprTree::TupleLit(vec![])), pos as u64, i as u64))]), &db, &HashMap::new())? {
                                ExprTree::BoolLit(true) => {
                                    for (name, lm) in &mut *attrs {
                                        let new_val = compute(&ExprTree::Call(Box::new(ExprTree::LambdaExpr(Box::new(lm.clone()))),
                                            vec![Box::new(ExprTree::Ref(String::new(), String::new(), Box::new(ExprTree::TupleLit(vec![])), pos as u64, i as u64))]), &db, &HashMap::new())?;
                                        let attr_pos = db.header[pos].1.iter().position(|x| x.name == *name).unwrap();
                                        if let tree::AttrFlag::Unique = db.header[pos].1[attr_pos].flag {
                                            for val in db.data[&(pos as u64, i as u64)].iter().as_ref() {
                                                match compute(&ExprTree::Eq(true, Box::new(val.clone()), Box::new(new_val.clone())), &db, &HashMap::new())? {
                                                    ExprTree::BoolLit(true) => return Err(format!["Unable to transform a unique attribute '{}' of entity '{}'
                                                        as it would invalidate uniqueness.", name, ent]),
                                                    _ => {}
                                                }
                                            }
                                        }
                                        for k in find_refs(&db.data[&(pos as u64, attr_pos as u64)][i]) {
                                            if db.ref_list[&k] == 1 {
                                                db.ref_list.remove(&k);
                                            } else {
                                                *db.ref_list.get_mut(&k).unwrap() -= 1;
                                            }
                                        }
                                        if db.ref_list.contains_key(&(pos as u64, i as u64)) {
                                            return Err(format!["Unable to drop '{}' as it is bound by reference constraint.", ent]);
                                        }
                                        db.data.get_mut(&(pos as u64, attr_pos as u64)).unwrap()[i] = new_val.clone();
                                        for rf in find_refs(&new_val) {
                                            if db.ref_list.contains_key(&rf) {
                                                *db.ref_list.get_mut(&rf).unwrap() += 1;
                                            } else {
                                                db.ref_list.insert(rf, 1);
                                            }
                                        }
                                    }
                                }, _ => {}
                            }
                        }
                        Ok(db)
                    },
                    None => Err(format!["Unable to transform a non-recognized entity '{}'.", ent])
                }
            }, tree::Command::Reshape(ent, v, new_ent) => {
                let cur_ent = db.header.iter().position(|v| v.0 == *ent).ok_or(
                    format!["Unable to reshape a non-recognized entity '{}'.", ent])?;
                let target = match new_ent {
                    Some(name) => {
                        db.header.push((name.clone(), db.header[cur_ent].1.clone()));
                        let mut temp_cont = vec![];
                        for ((_, k), v) in db.data.iter().filter(|(x,_)| x.0 == cur_ent as u64) {
                            temp_cont.push(((db.header.len() - 1) as u64, k.clone(), v.clone()));
                        }
                        for i in temp_cont {
                            db.data.insert((i.0, i.1), i.2);
                        }
                        db.header.len() - 1
                    }, None => cur_ent
                };
                let mut collapsed = vec![];
                for ro in v {
                    match ro {
                        tree::ReshapeOptions::Collapse(atr) => {
                            let atr_pos = db.header[target].1.iter().position(|x| x.name == *atr).ok_or(
                                format!["Unable to collapse non-recognized attribute '{}' of entity '{}'.", atr, ent])?;
                            collapsed.push(atr_pos);
                        }, tree::ReshapeOptions::New(atr) => {
                            if db.header[target].1.iter().position(|x| x.name == atr.name) != None {
                                return Err(format!["Attribute '{}' of entity '{}' already exists and cannot be added in Reshape."
                                    , atr.name, ent]);
                            }
                            let lm_type = atr.default.as_mut().unwrap().get_type(&db, &HashMap::new())?;
                            let needed_type = tree::Type::FuncType(vec![Box::new(tree::Type::Object(ent.clone())),
                                Box::new(match &atr.attrType {
                                    tree::SpType::Reg(t) => t.clone(),
                                    tree::SpType::Restrict(t, _) => *t.clone(),
                                    tree::SpType::Gen(t, _, _) => *t.clone()
                            })]);
                            if lm_type != needed_type {
                                return Err(format!["Default value of attribute '{}' of entity '{}' doesn't match
                                    its type '{:?}'", atr.name, ent, needed_type]);
                            }
                            db.header[target].1.push(atr.clone());
                            let mut newvals = vec![];
                            if let Some(a) = db.data.get(&(target as u64, 0)) {
                                for i in 0..a.len() {
                                    newvals.push(compute(&ExprTree::Call(Box::new(ExprTree::LambdaExpr(Box::new(atr.default.clone().unwrap()))), 
                                        vec![Box::new(ExprTree::Ref(String::new(), String::new(), Box::new(ExprTree::TupleLit(vec![])), target as u64, i as u64))]),&db, &HashMap::new())?);
                                }
                            }
                            for i in newvals {
                                match db.data.get_mut(&(target as u64, (db.header[target].1.len() - 1) as u64)) {
                                    Some(v) => v.push(i.clone()),
                                    None => {db.data.insert((target as u64, (db.header[target].1.len() - 1) as u64), vec![i.clone()]); ()}
                                }
                                for rf in find_refs(&i) {
                                    if db.ref_list.contains_key(&rf) {
                                        *db.ref_list.get_mut(&rf).unwrap() += 1;
                                    } else {
                                        db.ref_list.insert(rf, 1);
                                    }
                                }
                            }
                        }
                    }
                }
                collapsed.sort();
                collapsed.reverse();
                for c in collapsed {
                    let mut err = false;
                    db.data.retain(|(k1, k2), v| {
                        if *k1 != target as u64 || *k2 != c as u64 {
                            return true;
                        }
                        for val in 0..v.len() {
                            for k in find_refs(&v[val]) {
                                if db.ref_list[&k] == 1 {
                                    db.ref_list.remove(&k);
                                } else {
                                    *db.ref_list.get_mut(&k).unwrap() -= 1;
                                }
                            }
                            if db.ref_list.contains_key(&(*k1, val as u64)) {
                                err = true;
                            }
                        }
                        false
                    });
                    for i in c+1..db.header[target].1.len() {
                        if db.data.contains_key(&(target as u64, i as u64)) {
                            let temp = db.data.remove(&(target as u64, i as u64)).unwrap();
                            db.data.insert((target as u64, i as u64 - 1), temp);
                        }
                    }
                    if err {
                        return Err(format!["Unable to drop '{}' as it is bound by reference constraint.", ent]);
                    }
                    db.header[target].1.remove(c);
                }
                Ok(db)
            }, _ => Err("This command is not supported.".to_string())
        }
    }
}