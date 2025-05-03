use crate::tree::{self, ExprTree};

fn bracket_searcher(open_br: &str, close_br: &str, code: &Vec<(String, u64, u64)>, index: &mut u32,
    end_ln: u64, end_col: u64) -> Result<Vec<(String, u64, u64)>, String> {
    let mut res = vec![];
    let mut cnt = 1;
    while let Some((lexeme, line, col)) = code.iter().nth(*index as usize) {
        if lexeme == close_br {
            cnt -= 1;
            if cnt == 0 {
                return Ok(res);
            }
        } else if lexeme == open_br {
            cnt += 1;
        }
        res.push((lexeme.clone(), line.clone(), col.clone()));
        *index += 1;
    }
    Err(format!("Expected '{}' in expression at ({}, {}).", close_br, end_ln, end_col))
}
fn grammar_expect(tok: &str, code: &Vec<(String, u64, u64)>, index: &mut u32, end_ln: u64, end_cl: u64) -> Result<(), String> {
    if *index >= code.len() as u32 { 
        return Err(format!["Expected '{}' at ({}, {})", tok, end_ln, end_cl]);
    }
    let (lx, ln, cl) = &code[*index as usize];
    if lx == tok {
        *index += 1;
        return Ok(());
    }
    Err(format!["Expected '{}', found '{}' at ({}, {}).", tok, lx, ln, cl])
}
fn parse_lit(code: &Vec<(String, u64, u64)>, index: &mut u32, end_ln: u64, end_cl: u64) -> Result<ExprTree, String> {
    if *index >= code.len() as u32 { 
        return Err(format!["Expected literal or identifier at ({}, {})", end_ln, end_cl]);
    }
    let (lx, ln, cl) = &code[*index as usize];
    *index += 1;
    if let Ok(v) = lx.parse::<i64>() {
        return Ok(ExprTree::IntLit(v));
    }
    if let Ok(v) = lx.parse::<f64>() {
        return Ok(ExprTree::DoubleLit(v));
    }
    if lx.starts_with('\'') {
        if lx.chars().nth(1) == Some('\\') {
            return Ok(ExprTree::CharLit(tree::escapes(&lx.chars().nth(2).ok_or("bug")?).ok_or("bug")?));
        }
        return Ok(ExprTree::CharLit(lx.chars().nth(1).ok_or("bug")?));
    }
    if lx.starts_with('\"') {
        let mut res = Vec::new();
        for i in &lx.chars().collect::<Vec<_>>()[1..lx.chars().count() - 1] {
            res.push(Box::new(ExprTree::CharLit(*i)));
        }
        return Ok(ExprTree::ArrayLit(res, Some(tree::Type::Char)))
    }
    if lx == "true" { return Ok(ExprTree::BoolLit(true)); }
    if lx == "false" { return Ok(ExprTree::BoolLit(false)); }
    if lx == "Nothing" { 
        grammar_expect(":", code, index, end_ln, end_cl)?;
        if *index >= code.len() as u32 { 
            return Err(format!["Expected type at ({}, {})", end_ln, end_cl]);
        }
        let (_, ln, cl) = &code[*index as usize];
        let mut type_toks = tree::TokenList { tokens: code, index: *index as u64 };
        let t = match tree::SpType::parse_type(&mut type_toks)? {
            tree::SpType::Reg(r) => r,
            _ => return Err(format!["Special type not allowed at ({}, {})", ln, cl])
        };
        *index += type_toks.index as u32;
        return Ok(ExprTree::NothingLit(t)); 
    }
    if lx == "Just" {
        grammar_expect("(", code, index, end_ln, end_cl)?;
        let inner_vec = bracket_searcher("(", ")", code, index, end_ln, end_cl)?;
        let res = grammar_parser("E1", &inner_vec, &mut 0, end_ln, end_cl)?;
        grammar_expect(")", code, index, end_ln, end_cl)?;
        return Ok(ExprTree::JustLit(Box::new(res)));
    }
    if lx == "[" || lx == "tup" {
        if lx == "tup" { grammar_expect("(", code, index, end_ln, end_cl)?; }
        let mut arg_vec = Vec::new();
        let mut bracket_counter = 1;
        let is_array = lx == "[";
        let mut iterated = false;
        loop {
            let mut inner_vec = Vec::new();
            while let Some(v) = code.iter().nth(*index as usize) {
                if v.0 == "," && bracket_counter == 1 { break; }
                else if v.0 == "(" && !is_array || v.0 == "[" && is_array { bracket_counter += 1; }
                else if v.0 == ")" && !is_array || v.0 == "]" && is_array { 
                    bracket_counter -= 1;
                    if bracket_counter == 0 {
                        break;
                    }
                }
                inner_vec.push(v.clone());
                *index += 1;
            }
            if *index >= code.len() as u32 { 
                return Err(format!["Expected '{}' or ',' in {} literal at ({}, {}).",
                if is_array {"]"} else {")"}, if is_array {"array"} else {"tuple"}, end_ln, end_cl]); }
            if inner_vec.len() == 0 && !iterated &&
                 (code[*index as usize].0 == ")" && !is_array || code[*index as usize].0 == "]" && is_array) { *index += 1; break; }
            let cur = &code[*index as usize];
            *index += 1;
            let res = grammar_parser("E1", &inner_vec, &mut 0, end_ln, end_cl)?;
            if !iterated { iterated = true; }
            arg_vec.push(Box::new(res));
            if cur.0 == ")" && !is_array || cur.0 == "]" && is_array { break; }
        }
        if is_array {
            if arg_vec.len() == 0 {
                grammar_expect(":", code, index, end_ln, end_cl)?;
                if *index >= code.len() as u32 { 
                    return Err(format!["Expected type at ({}, {})", end_ln, end_cl]);
                }
                let (_, ln, cl) = &code[*index as usize];
                let mut type_toks = tree::TokenList { tokens: code, index: *index as u64 };
                let t = match tree::SpType::parse_type(&mut type_toks)? {
                    tree::SpType::Reg(r) => r,
                    _ => return Err(format!["Special type not allowed at ({}, {})", ln, cl])
                };
                *index += type_toks.index as u32;
                return Ok(ExprTree::ArrayLit(vec![], Some(t)));
            }
            return Ok(ExprTree::ArrayLit(arg_vec, None));
        }
        return Ok(ExprTree::TupleLit(arg_vec));
    }
    if lx == "for" {
        grammar_expect("(", code, index, end_ln, end_cl)?;
        let (lx2, ln, cl) = &code[*index as usize];
        if let Some(c) = lx2.chars().nth(0) {
            if !c.is_alphabetic() {
                return Err(format!["Expected identifier at ({}, {}), found '{}'.", ln, cl, lx2]);
            }
        }
        *index += 1;
        grammar_expect(")", code, index, end_ln, end_cl)?;
        let mut type_toks = tree::TokenList { tokens: code, index: *index as u64 };
        let t = tree::Lambda::parse_lambda(&mut type_toks)?;
        *index = type_toks.index as u32;
        return Ok(ExprTree::For(lx2.clone(), Box::new(t)));
    }
    if let Some(c) = lx.chars().nth(0) {
        if c.is_alphabetic() {
            return Ok(ExprTree::Ident(lx.to_string()));
        }
    }
    if lx == "#" {
        grammar_expect("(", code, index, end_ln, end_cl)?;
        let (lx2, ln2, cl2) = &code[*index as usize];
        let id1 = match lx2.chars().nth(0) {
            Some(c) if c.is_alphabetic() => lx,
            _ => return Err(format!["Expected identifier, found '{}' in reference at ({}, {}).", lx2, ln2, cl2])
        };
        *index += 1;
        grammar_expect(",", code, index, end_ln, end_cl)?;
        let (lx2, ln2, cl2) = &code[*index as usize];
        let id2 = match lx2.chars().nth(0) {
            Some(c) if c.is_alphabetic() => lx,
            _ => return Err(format!["Expected identifier, found '{}' in reference at ({}, {}).", lx2, ln2, cl2])
        };
        *index += 1;
        grammar_expect(",", code, index, end_ln, end_cl)?;
        let res = grammar_parser("E1", code, index, end_ln, end_cl)?;
        grammar_expect(")", code, index, end_ln, end_cl)?;
        return Ok(ExprTree::Ref(id1.clone(), id2.clone(), Box::new(res), 0, 0));
    }
    *index -= 1;
    Err(format!["Expected identifier or literal, found '{}' at ({}, {}).", lx, ln, cl])
}

fn grammar_parser(nonterm: &str, code: &Vec<(String, u64, u64)>, index: &mut u32, end_ln: u64, end_cl: u64)
    -> Result<ExprTree, String> {
    match nonterm {
        "E1" => {
            let val1 = grammar_parser("E2", code, index, end_ln, end_cl)?;
            match code.iter().nth(*index as usize) {
                Some((a,_,_)) if a == "=" => {
                    *index += 1;
                    grammar_expect("=", code, index, end_ln, end_cl)?;
                    let val2 = grammar_parser("E2", code, index, end_ln, end_cl)?;
                    return Ok(ExprTree::Eq(true, Box::new(val1), Box::new(val2)));
                }, Some((a,_,_)) if a == "!" => {
                    *index += 1;
                    grammar_expect("=", code, index, end_ln, end_cl)?;
                    let val2 = grammar_parser("E2", code, index, end_ln, end_cl)?;
                    return Ok(ExprTree::Eq(false, Box::new(val1), Box::new(val2)));
                }, Some((tok, ln, cl)) => Err(format!["Unexpected token '{}' in expression at ({}, {})",
                    tok, ln, cl]),
                None => Ok(val1)
            }
        }, "E2" => {
            let val1 = grammar_parser("E3", code, index, end_ln, end_cl)?;
            match code.iter().nth(*index as usize) {
                Some((a,_,_)) if "<>".contains(a) => {
                    let is_greater = a == ">";
                    let mut is_eq = false;
                    *index += 1;
                    match code.iter().nth(*index as usize) {
                        Some((b,_,_)) if b == "=" => {is_eq = true; *index += 1;},
                        Some(_) | None => {}
                    }
                    let val2 = grammar_parser("E3", code, index, end_ln, end_cl)?;
                    return Ok(ExprTree::Cmp(is_greater, is_eq, Box::new(val1), Box::new(val2)));
                }, _ => Ok(val1)
            }
        }, "E3" => {
            let mut res = grammar_parser("E4", code, index, end_ln, end_cl)?;
            while let Some((a, _, _)) = code.iter().nth(*index as usize) {
                if a == "+" || a == "-" {
                    *index += 1;
                    let val = grammar_parser("E4", code, index, end_ln, end_cl)?;
                    res = if a == "+" {ExprTree::Plus(Box::new(res), Box::new(val))} else {ExprTree::Minus(Box::new(res), Box::new(val))}
                } else { break; }
            }
            return Ok(res);
        }, "E4" => {
            let mut res = grammar_parser("E5", code, index, end_ln, end_cl)?;
            while let Some((a, _, _)) = code.iter().nth(*index as usize) {
                if a == "*" || a == "/" || a == "%" {
                    *index += 1;
                    let val = grammar_parser("E5", code, index, end_ln, end_cl)?;
                    if a == "*" { res = ExprTree::Mul(Box::new(res), Box::new(val)); }
                    else if a == "/" { res = ExprTree::Div(Box::new(res), Box::new(val)); }
                    else if a == "%" { res = ExprTree::Mod(Box::new(res), Box::new(val)); }
                } else { break; }
            }
            return Ok(res);
        }, "E5" => {
            let res = grammar_parser("E6", code, index, end_ln, end_cl)?;
            match code.iter().nth(*index as usize) {
                Some((a, _, _)) if a == "^" => {
                    *index += 1;
                    return Ok(ExprTree::Exp(Box::new(res), Box::new(grammar_parser("E6", code, index, end_ln, end_cl)?)));
                }, Some(_) | None => return Ok(res)
            }
        }, "E6" => {
            match code.iter().nth(*index as usize) {
                Some((a, _, _)) if a == "if" => {
                    grammar_expect("if", code, index, end_ln, end_cl)?;
                    let inner_vec = bracket_searcher("if", "then", code, index, end_ln, end_cl)?;
                    let cond = grammar_parser("E1", &inner_vec, &mut 0, end_ln, end_cl)?;
                    grammar_expect("then", code, index, end_ln, end_cl)?;
                    let inner_vec = bracket_searcher("then", "else", code, index, end_ln, end_cl)?;
                    let br1 = grammar_parser("E1", &inner_vec, &mut 0, end_ln, end_cl)?;
                    grammar_expect("else", code, index, end_ln, end_cl)?;
                    let br2 = grammar_parser("E1", code, index, end_ln, end_cl)?;
                    return Ok(ExprTree::IfExpr(Box::new(cond), Box::new(br1), Box::new(br2)));
                }, Some(_) | None => Ok(grammar_parser("E7", code, index, end_ln, end_cl)?)}
        }
        "E7" => {
            let mut res = match code.iter().nth(*index as usize) {
                Some((a, _, _)) if a == "(" => {
                    *index += 1;
                    let inner_vec = bracket_searcher("(", ")", code, index, end_ln, end_cl)?;
                    let v = grammar_parser("E1", &inner_vec, &mut 0, end_ln, end_cl)?;
                    grammar_expect(")", code, index, end_ln, end_cl)?;
                    v
                }, Some((a, _, _)) if a == "lambda" => {
                    *index += 1;
                    let mut type_toks = tree::TokenList { tokens: code, index: *index as u64 };
                    let t = tree::Lambda::parse_lambda(&mut type_toks)?;
                    *index += type_toks.index as u32;
                    ExprTree::LambdaExpr(Box::new(t))
                }, Some(_) | None => parse_lit(code, index, end_ln, end_cl)?
            };
            while let Some((a, _, _)) = code.iter().nth(*index as usize) {
                if a == "." {
                    *index += 1;
                    res = ExprTree::Dot(Box::new(res), Box::new(parse_lit(code, index, end_ln, end_cl)?));
                } else if a == "(" {
                    let mut arg_vec = Vec::new();
                    *index += 1;
                    loop {
                        let mut do_exit = false;
                        let mut inner_vec = Vec::new();
                        let mut bracket_counter = 1;
                        while let Some(v) = code.iter().nth(*index as usize) {
                            if v.0 == "," && bracket_counter == 1 { 
                                do_exit = false;
                                break; }
                            else if v.0 == "(" { bracket_counter += 1; }
                            else if v.0 == ")" {
                                bracket_counter -= 1;
                                if bracket_counter == 0 {
                                    do_exit = true;
                                    break;
                                }
                            }
                            inner_vec.push(v.clone());
                            *index += 1;
                        }
                        if inner_vec.len() == 0 && do_exit { break; }
                        if *index >= code.len() as u32 { 
                            return Err(format!["Expected ')' or ',' in function call at ({}, {})", end_ln, end_cl]); }
                        if let Some(v) = code.iter().nth(*index as usize + 1) {
                            if !do_exit && v.0 == ")" {
                                return Err(format!["Expected expression after ',' in function call at ({}, {}).", v.1, v.2]);
                            }
                        }
                        let res = grammar_parser("E1", &inner_vec, &mut 0, end_ln, end_cl)?;
                        arg_vec.push(Box::new(res));
                        if do_exit { break; }
                        *index += 1;
                    }
                    *index += 1;
                    res = ExprTree::Call(Box::new(res), arg_vec);
                } else { break; }
            }
            return Ok(res);
        }, _ => panic!("bug (incorrect nonterminal specified)")
    }
}

impl ExprTree {
    pub fn new(code: Vec<(String, u64, u64)>, end_ln: u64, end_col: u64) -> Result<ExprTree, String> {
        if code.len() == 0 {Err(format!["Expected expression at ({}, {}).", end_ln, end_col]) }
        else {
            Ok(grammar_parser("E1", &code, &mut 0, end_ln, end_col)?)
        }
    }
}