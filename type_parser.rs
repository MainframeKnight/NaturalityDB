use crate::tree::{self, expect, read_ident, LexerIterator, SpType};

impl tree::Lambda {
    pub fn parse_lambda<T: LexerIterator>(input: &mut T) -> Result<tree::Lambda, String> {
        let mut named;
        match input.next() {
            Some(a) if a.0.len() != 0 && a.0.chars().nth(0).unwrap().is_alphabetic() => {
                named = Some((a.0, tree::Type::Bool));
                expect(input, vec!["("])?;
            }, Some(a) if a.0 == "(" => {
                named = None;
            }, _ => return Err(format!["Expected '(', found EOF."])
        };
        let mut paramvec: Vec<(String, tree::Type)> = vec![];
        let id = read_ident(input)?;
        expect(input, vec![":"])?;
        let t = tree::Type::parse(input)?;
        paramvec.push((id, t));
        loop {
            match input.lookahead() {
                Some(lexeme) => {
                    if lexeme == ")" { break; }
                    expect(input, vec![","])?;
                    let id = read_ident(input)?;
                    expect(input, vec![":"])?;
                    let t = tree::Type::parse(input)?;
                    paramvec.push((id, t));
                },
                None => return Err(format!["Expected ')', found EOF."])
            }
        }
        expect(input, vec![")", "-", ">"])?;
        if named != None {
            named.as_mut().unwrap().1 = tree::Type::parse(input)?;
        }
        expect(input, vec!["{"])?;
        let (val, br1, br2) = tree::find_bracket(input, "}", "{")?;
        return Ok(tree::Lambda{ params: paramvec, named: named, code: 
            tree::Node::new(val, br1, br2)?});
    }
}
impl tree::SpType {
    pub fn parse_type<T: LexerIterator>(input: &mut T) -> Result<tree::SpType, String> {
        let primtypes: Vec<&str> = vec!["Int", "Char", "Bool", "Double"];
        let mut types: Vec<tree::Type> = vec![tree::Type::Int, 
            tree::Type::Char, tree::Type::Bool, tree::Type::Double];
        let (lexeme, line, col);
        match input.next() {
            Some((lx, ln, cl)) => (lexeme, line, col) = (lx, ln, cl),
            None => return Err("Expected type, found EOF.".to_string())
        }
        for (i,s) in primtypes.iter().enumerate() {
            if lexeme == *s {
                return Ok(tree::SpType::Reg(types.swap_remove(i)));
            }
        }
        if lexeme == "Func" {
            expect(input, vec!["("])?;
            let args = tree::Type::parse_vectype(input)?;
            if args.len() < 2 {
                return Err(format!["Too few parameters in function type at ({}, {}).", line, col]);
            }
            return Ok(SpType::Reg(tree::Type::FuncType(args)));
        } else if lexeme == "(" {
            let args = tree::Type::parse_vectype(input)?;
            if args.is_empty() {
                return Err(format!["Empty tuple type at ({}, {}).", line, col]);
            }
            return Ok(SpType::Reg(tree::Type::Tuple(args)));
        } else if lexeme == "Sum" {
            expect(input, vec!["("])?;
            let args = tree::Type::parse_vectype(input)?;
            if args.len() < 2 {
                return Err(format!["Too few parameters in sum type at ({}, {}).", line, col]);
            }
            return Ok(SpType::Reg(tree::Type::Sum(args)));
        } else if lexeme == "[" {
            let (ln, cl) = input.coords();
            match SpType::parse_type(input)? {
                SpType::Reg(t) => {
                    expect(input, vec!["]"])?;
                    return Ok(SpType::Reg(tree::Type::Array(Box::new(t))));
                }, _ => return Err(format!["Special type not allowed at ({}, {}).", ln, cl])
            }
        } else if lexeme == "Maybe" {
            expect(input, vec!["("])?;
            let (ln, cl) = input.coords();
            match SpType::parse_type(input)? {
                SpType::Reg(t) => {
                    expect(input, vec![")"])?;
                    return Ok(SpType::Reg(tree::Type::Maybe(Box::new(t))));
                }, _ => return Err(format!["Special type not allowed at ({}, {}).", ln, cl])
            }
        } else if lexeme == "Gen" {
            expect(input, vec!["("])?;
            let (ln, cl) = input.coords();
            let res = SpType::parse_type(input)?;
            expect(input, vec![","])?;
            let lm = tree::Lambda::parse_lambda(input)?;
            expect(input, vec![")"])?;
            return match res {
                SpType::Reg(t) => {
                    Ok(SpType::Gen(Box::new(t), lm, 0))
                }, _ => Err(format!["Special type not allowed at ({}, {}).", ln, cl])
            };
        } else if lexeme == "Restrict" {
            expect(input, vec!["("])?;
            let (ln, cl) = input.coords();
            let res = SpType::parse_type(input)?;
            expect(input, vec![","])?;
            let lm = tree::Lambda::parse_lambda(input)?;
            expect(input, vec![")"])?;
            return match res {
                SpType::Reg(t) => {
                    Ok(SpType::Restrict(Box::new(t), lm))
                }, _ => Err(format!["Special type not allowed at ({}, {}).", ln, cl])
            };
        } else if lexeme == "Object" {
            expect(input, vec!["("])?;
            let s = read_ident(input)?;
            expect(input, vec![")"])?;
            return Ok(SpType::Reg(tree::Type::Object(tree::CoordStr{ name: s, ln: line, col: col })));
        }
        return Err(format!["Unrecognized type '{}' at ({}, {})", lexeme, line, col]);
    }
}
impl tree::Type {
    fn parse_vectype<T: LexerIterator>(input: &mut T) -> Result<Vec<Box<tree::Type>>, String> {
        let mut args: Vec<Box<tree::Type>> = Vec::new();
        args.push(Box::new(Self::parse(input)?));
        loop {
            match input.lookahead() {
                Some(lexeme) => {
                    if lexeme == ")" { break; }
                    expect(input, vec![","])?;
                    args.push(Box::new(Self::parse(input)?));
                }
                None => return Err(format!["Expected ')', found EOF."])
            }
        }
        expect(input, vec![")"])?;
        Ok(args)
    }
    fn parse<T: LexerIterator>(input: &mut T) -> Result<tree::Type, String> {
        let (ln, cl) = input.coords();
        match SpType::parse_type(input)? {
            SpType::Reg(t) => Ok(t),
            _ => Err(format!["Special type not allowed at ({}, {}).", ln, cl])
        }
    }
}