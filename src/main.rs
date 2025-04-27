pub mod evaluator;
pub mod language;
pub mod lexer;
pub mod parser;

use std::env;

fn main() {
    let args = env::args();
    let path = if let Some(s) = args.into_iter().nth(1) {
        s
    } else {
        println!("Error! No source file specified");
        return;
    };

    macros::dbg!(&path);
    let input = match std::fs::read_to_string(&path) {
        Ok(v) => v,
        Err(e) => {
            println!("Error! Invalid input file: {}", path);
            println!("Error was {:?}", e);
            return;
        }
    };

    let result = shai::run(&input);

    match result {
        Ok((state, value)) => {
            macros::dbg!("State", state);
            macros::dbg!("Return value", value);
        }

        Err(e) => {
            println!("Error!: {:?}", e);
        }
    }
}
