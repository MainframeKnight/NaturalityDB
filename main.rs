mod tree;
mod expr_parser;
mod type_parser;
mod db_data;
mod command_parser;
mod semantic;
mod run;
mod compute;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    match args.get(1) {
        Some(file) => {
            match std::fs::read_to_string(file) {
                Ok(str) => {
                    let mut lx = tree::Lexer::new(str);
                    match &mut tree::Command::parse_program(&mut lx) {
                        Ok(v) => {
                            let mut state = tree::DBState::new();
                            for i in v {
                                state = match i.complete(state){
                                    Ok(s) => s,
                                    Err(e) => {
                                        println!("Error occurred: {}", e);
                                        std::process::exit(-1);
                                    }
                                };
                            }
                        }
                        Err(e) => println!("Parsing error occurred: {}", e)
                    }
                }, Err(_) => println!("Unable to open file '{}'.", file)
            }
        }
        None => println!("Incorrect cmd-args format: first parameter must be the input file.")
    }
}
