pub mod evaluator;
pub mod language;
pub mod lexer;
pub mod parser;

use std::{collections::HashMap, fs::read_to_string};

use evaluator::environment::EnvironmentState;
use lexer::Lexer;

#[macro_export]
macro_rules! dbg {
    ($($x:tt)*) => {
        {
            #[cfg(debug_assertions)]
            {
                std::dbg!($($x)*)
            }
            #[cfg(not(debug_assertions))]
            {
                ($($x)*)
            }
        }
    }
}

fn read_from_file(path: &str) -> String {
    read_to_string(path).unwrap()
}

fn main() {
    let input = read_from_file("./hello.shai");
    let state = EnvironmentState::new(HashMap::new());

    dbg!("{}", &input);
    let tokens = Lexer::new(&input);

    dbg!("{:?}", &tokens);

    let ast = match parser::parse(tokens) {
        Ok(v) => v,
        Err(e) => {
            println!("Parse error! {:?}", e);
            return;
        }
    };

    dbg!("AST: {:?}", &ast);

    let result = evaluator::evaluate(state, ast);
    match result {
        Ok((state, v)) => {
            dbg!("{:?}", state);
            dbg!("Return value{:?}", v);
        }

        Err(e) => {
            println!("{:?}", e);
        }
    }
}
