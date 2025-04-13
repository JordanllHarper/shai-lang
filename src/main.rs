pub mod condition_evaluation;
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

    dbg!("Input: {}", &input);
    let tokens = Lexer::new(&input);

    dbg!("Token stream: {:?}", &tokens);

    let ast = match parser::parse(tokens) {
        Ok(v) => v,
        Err(e) => {
            println!("Error!: {:?}", e);
            return;
        }
    };

    dbg!("Generated AST: {:?}", &ast);

    let result = evaluator::evaluate(state, ast);
    match result {
        Ok((state, v)) => {
            dbg!("State: {:?}", state);
        }

        Err(e) => {
            println!("Error!: {:?}", e);
        }
    }
}
