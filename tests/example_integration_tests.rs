use std::fs;

use pretty_assertions::assert_eq;
use shai::{evaluator::environment::Value, language::Expression, run};

fn test(name: &str, expected_value: Value) {
    let path = format!("examples/{}.shai", name);
    let contents = fs::read_to_string(path).expect("Test file doesn't exist");

    match run(&contents) {
        Ok((_, value)) => assert_eq!(expected_value, value),
        Err(e) => panic!("Error: {:?}", e),
    }
}

#[test]
fn fizzbuzz() {
    test(
        "fizzbuzz",
        Value::ValueLiteral(shai::language::ValueLiteral::Array(vec![
            Expression::new_int(1),
            Expression::new_int(2),
            Expression::new_string("Fizz"),
            Expression::new_int(4),
            Expression::new_string("Buzz"),
            Expression::new_string("Fizz"),
            Expression::new_int(7),
            Expression::new_int(8),
            Expression::new_string("Fizz"),
            Expression::new_string("Buzz"),
        ])),
    );
}

#[test]
fn fib() {
    test(
        "fib",
        Value::ValueLiteral(shai::language::ValueLiteral::Array(vec![
            Expression::new_int(0),
            Expression::new_int(1),
            Expression::new_int(1),
            Expression::new_int(2),
            Expression::new_int(3),
            Expression::new_int(5),
            Expression::new_int(8),
            Expression::new_int(13),
            Expression::new_int(21),
            Expression::new_int(34),
        ])),
    )
}
