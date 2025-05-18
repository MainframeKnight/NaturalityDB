use crate::tree::{self, expect, find_bracket, read_ident, Lexer};
type Cmd = tree::Command;
type attr = tree::Attr;

fn parse_attrlist(input: &mut Lexer) -> Result<Vec<tree::ExprTree>, String> {
    let mut res = vec![];
    let mut arg_vec = vec![];
    let mut closed = false;
    while let Some((lx, ln, cl)) = input.next() {
        if lx == ";" {
            res.push(tree::ExprTree::new(arg_vec, ln, cl)?);
            arg_vec = vec![];
            continue;
        }
        else if lx == "$" {
            res.push(tree::ExprTree::new(arg_vec, ln, cl)?);
            closed = true;
            break; 
        }
        arg_vec.push((lx, ln, cl));
    }
    if closed {Ok(res)} else {Err("Expected ';', found EOF.".to_string())}
}
fn parse_reshape(input: &mut Lexer) -> Result<tree::ReshapeOptions, String> {
    match input.next() {
        Some((lx, ln, cl)) => if lx == "new" {
            let newattr = attr::parse(input)?;
            if let None = newattr.default {
                return Err(format!["New attribute '{}' in reshape doesn't have a default value at ({}, {}).", newattr.name, ln, cl])
            }
            Ok(tree::ReshapeOptions::New(newattr))
        } else if lx == "collapse" {
            Ok(tree::ReshapeOptions::Collapse(read_ident(input)?))
        } else {
            Err(format!["Expected 'new' or 'collapse', found '{}' at ({}, {}).", lx, ln, cl])
        },
        None => return Err("Expected 'new' or 'collapse', found EOF.".to_string())
    }
}

impl attr {
    fn parse(input: &mut Lexer) -> Result<attr, String> {
        let mut res = attr::new();
        let lexeme;
        match input.lookahead() {
            Some(lx) => lexeme = lx,
            None => return Err("Expected attribute definition, found EOF.".to_string())
        }
        if lexeme == "computable" {
            expect(input, vec!["computable"])?;
            res.flag = tree::AttrFlag::Computable;
        } else if lexeme == "global" {
            expect(input, vec!["global"])?;
            res.flag = tree::AttrFlag::Global;
        } else if lexeme == "unique" {
            expect(input, vec!["unique"])?;
            res.flag = tree::AttrFlag::Unique;
        }
        res.name = read_ident(input)?;
        if lexeme != "global" {
            expect(input, vec![":"])?;
            res.attrType = tree::SpType::parse_type(input)?;
            let lex = input.lookahead();
            if res.flag == tree::AttrFlag::Computable || lex == Some("as".to_string()) {
                expect(input, vec!["as"])?;
                res.default = Some(tree::Lambda::parse_lambda(input)?);
            }
        } else {
            res.default = Some(tree::Lambda::parse_lambda(input)?);
        }
        return Ok(res);
    }
}

impl Cmd {
    pub fn parse_program(input: &mut Lexer) -> Result<Vec<Cmd>, String> {
        let value;
        let (lexeme, line, col);
        match input.next() {
            Some(e) => (lexeme, line, col) = e,
            None => return Ok(Vec::new())
        }
        if lexeme == "entity" {
            let name = read_ident(input)?;
            expect(input, vec!["{"])?;
            let mut attr_vec = vec![attr::parse(input)?];
            loop {
                match input.lookahead() {
                    Some(s) => if s == "}" { break; } else {
                        attr_vec.push(attr::parse(input)?);
                    },
                    None => return Err("Expected ',' or '}', found EOF.".to_string())
                }
            }
            expect(input, vec!["}"])?;
            value = Cmd::NewEntity(name, attr_vec);
        } else if lexeme == "eval" {
            expect(input, vec!["{"])?;
            let (br,br1, br2) = find_bracket(input, "}", "{")?;
            value = Cmd::Eval(tree::ExprTree::new(br, br1, br2)?);
        } else if lexeme == "commit" {
            let (file, ln, cl) = input.next().ok_or("Expected filename, found EOF.")?;
            if file.starts_with("\"") {
                value = Cmd::Commit(file[1..file.len() - 1].to_string());
            } else {
                return Err(format!["Expected string, found '{}' at ({}, {}).", file, ln, cl]);
            }
        } else if lexeme == "open" {
            let (file, ln, cl) = input.next().ok_or("Expected filename, found EOF.")?;
            if file.starts_with("\"") {
                value = Cmd::Open(file[1..file.len() - 1].to_string());
            } else {
                return Err(format!["Expected string, found '{}' at ({}, {}).", file, ln, cl]);
            }
        } else if lexeme == "add" {
            let mut attr_vec = Vec::new();
            let name = read_ident(input)?;
            expect(input, vec!["{"])?;
            let ident = read_ident(input)?;
            expect(input, vec![":"])?;
            let vals = parse_attrlist(input)?;
            attr_vec.push((ident, vals));
            loop {
                match input.lookahead() {
                    Some(lexeme) => {
                        if lexeme == "}" { break; }
                        let ident = read_ident(input)?;
                        expect(input, vec![":"])?;
                        let vals = parse_attrlist(input)?;
                        attr_vec.push((ident, vals));
                    }
                    None => return Err("Expected '}', found EOF.".to_string())
                }
            }
            expect(input, vec!["}"])?;
            value = Cmd::Add(name, attr_vec);
        } else if lexeme == "delete" {
            let name = read_ident(input)?;
            let lam = tree::Lambda::parse_lambda(input)?;
            value = Cmd::Delete(name, lam);
        } else if lexeme == "transform" {
            let name = read_ident(input)?;
            expect(input, vec!["{"])?;
            let mut new_vals = vec![];
            loop {
                match input.lookahead() {
                    Some(s) if s == "}" => break,
                    Some(_) => {
                        let attr = read_ident(input)?;
                        expect(input, vec![":"])?;
                        let val = tree::Lambda::parse_lambda(input)?;
                        new_vals.push((attr, val));
                    },
                    None => return Err("Expected '}', found EOF.".to_string())
                }
            }
            expect(input, vec!["}", "where"])?;
            let cond = tree::Lambda::parse_lambda(input)?;
            value = Cmd::Trans(name, cond, new_vals);
        } else if lexeme == "reshape" {
            let name = read_ident(input)?;
            expect(input, vec!["{"])?;
            let mut res = vec![parse_reshape(input)?];
            loop {
                match input.lookahead() {
                    Some(lexeme) => {
                        if lexeme == "}" { break; }
                        res.push(parse_reshape(input)?);
                    }
                    None => return Err("Expected '}', found EOF.".to_string())
                }
            }
            expect(input, vec!["}"])?;
            let mut resName = None;
            if input.lookahead() == Some("as".to_string()) {
                expect(input, vec!["as"])?;
                resName = Some(read_ident(input)?);
            }
            value = Cmd::Reshape(name, res, resName);
        } else if lexeme == "project" {
            let name = read_ident(input)?;
            expect(input, vec!["{"])?;
            let mut res = vec![read_ident(input)?];
            loop {
                match input.lookahead() {
                    Some(lexeme) => {
                        if lexeme == "}" { break; }
                        expect(input, vec![","])?;
                        res.push(read_ident(input)?);
                    }
                    None => return Err("Expected '}', found EOF.".to_string())
                }
            }
            expect(input, vec!["}", "as"])?;
            value = Cmd::Project(name, res, read_ident(input)?);
        } else if lexeme == "join" {
            let mut res = vec![read_ident(input)?];
            loop {
                match input.lookahead() {
                    Some(lexeme) => {
                        if lexeme == "by" { break; }
                        expect(input, vec![","])?;
                        res.push(read_ident(input)?);
                    }
                    None => return Err("Expected 'by', found EOF.".to_string())
                }
            }
            expect(input, vec!["by"])?;
            let lam = tree::Lambda::parse_lambda(input)?;
            expect(input, vec!["as"])?;
            value = Cmd::Join(res, lam, read_ident(input)?);
        } else if lexeme == "product" {
            let mut res = vec![read_ident(input)?];
            loop {
                match input.lookahead() {
                    Some(lexeme) => {
                        if lexeme == "as" { break; }
                        expect(input, vec![","])?;
                        res.push(read_ident(input)?);
                    }
                    None => return Err("Expected 'as', found EOF.".to_string())
                }
            }
            expect(input, vec!["as"])?;
            value = Cmd::Product(res, read_ident(input)?);
        } else if lexeme == "drop" {
            value = Cmd::Drop(read_ident(input)?);
        } else {
            return Err(format!["Unrecognized command at ({}, {}).", line, col]);
        }
        let mut cmd = Self::parse_program(input)?;
        let mut res = vec![value];
        res.append(&mut cmd);
        if input.get_errors().len() > 0 {
            let mut err_str = String::from("Lexer errors occurred:\n");
            for i in input.get_errors().iter() {
                err_str.push_str(i);
                err_str.push('\n');
            }
            return Err(err_str);
        }
        return Ok(res);
    }
}