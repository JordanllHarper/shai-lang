pub mod evaluator;
pub mod language;
pub mod lexer;
pub mod parser;

use std::{collections::HashMap, env};

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

fn main() {
    let args = env::args();
    let path = if let Some(s) = args.into_iter().nth(1) {
        s
    } else {
        println!("Error! No source file specified");
        return;
    };
    let input = match std::fs::read_to_string(&path) {
        Ok(v) => v,
        Err(_) => {
            println!("Error! Invalid input file: {}", path);
            return;
        }
    };

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
        Ok((state, _)) => {
            dbg!("State: {:?}", state);
        }

        Err(e) => {
            println!("Error!: {:?}", e);
        }
    }
}
