use std::collections::HashMap;

use crate::{
    language::{Assignment, Expression, SingleValue},
    parser, Lexer, Token,
};

enum RuntimeReferenceValue {
    Void,
    Int(i32),
    String(String),
    Char(char),
    Array(Vec<RuntimeReferenceValue>),
    Dictionary(HashMap<RuntimeReferenceValue, RuntimeReferenceValue>),
    Bool(bool),
    Float(f32),
    // Question: how do we encode a function as a reference value?
}

pub struct ScopeState {
    //            name of reference -> the value
    //            e.g. x            -> 5
    //            e.g. print        -> Function that prints to stdout
    references: HashMap<String, RuntimeReferenceValue>,
    inner_scopes: Vec<ScopeState>,
}

impl ScopeState {
    pub fn new_init() -> Self {
        Self {
            references: HashMap::new(),
            inner_scopes: Vec::new(),
        }
    }
    pub fn new(
        references: HashMap<String, RuntimeReferenceValue>,
        inner_scopes: Vec<ScopeState>,
    ) -> Self {
        Self {
            references,
            inner_scopes,
        }
    }
}

fn evaluate(state: ScopeState, expression: Expression) -> ScopeState {
    match expression {
        Expression::Operation {
            lhs,
            rhs,
            operation,
        } => evaluate_operation(*lhs, *rhs, operation, state),
        _ => todo!(),
    }
    /*
    # TODO: list:
    - [ ] Save variables into the state using the variable name.
    - [ ] Save function declaration and implemention into state.
    - [ ] Run a print function.
    - [ ] Mutate a variable in the state.
     */
    todo!()
}

fn evaluate_operation(
    lhs: Expression,
    rhs: Expression,
    operation: crate::language::TwoSideOperation,
    state: ScopeState,
) {
    match operation {
        crate::language::TwoSideOperation::FunctionCall => todo!(),
        crate::language::TwoSideOperation::Math(_) => todo!(),
        crate::language::TwoSideOperation::Assignment(a) => evaluate_assignment(a, state, lhs, rhs),
    }
    todo!()
}
fn expect_identifier_or_error(e: Expression) -> String {
    if let Expression::SingleValue(ref s) = e {
        if let SingleValue::Identifier(i) = s {
            return i.to_string();
        }
    };
    panic!("Invalid syntax. Expected identifier, found {:?}", e)
}

fn evaluate_rhs(e: Expression) -> RuntimeReferenceValue {
    // x = 5
    // x = "some string"
    // x = 'c'
    // x = true
    // x = {
    //     ... some code ...
    // }
    // x = [ 1, 2, 3 ]

    match e {
        Expression::SingleValue(s) => match s {
            SingleValue::ValueLiteral(l) => {
                let representation = l.representation;
                match l.native_type {
                    crate::language::NativeType::Void => RuntimeReferenceValue::Void,
                    crate::language::NativeType::Char => RuntimeReferenceValue::Char(
                        representation.chars().next().expect("This is a character"),
                    ),
                    crate::language::NativeType::Int => RuntimeReferenceValue::Int(
                        str::parse::<i32>(&representation).expect("This should be a number"),
                    ),
                    crate::language::NativeType::String => {
                        RuntimeReferenceValue::String(representation)
                    }
                    crate::language::NativeType::Float => RuntimeReferenceValue::Float(
                        str::parse::<f32>(&representation)
                            .expect("This should be a floating point number"),
                    ),
                    crate::language::NativeType::Bool => RuntimeReferenceValue::Bool(
                        str::parse::<bool>(&representation).expect("This to be a boolean."),
                    ),
                    crate::language::NativeType::Array(a) => {
                        let mut values = vec![];

                        RuntimeReferenceValue::Array(values)
                    }
                    crate::language::NativeType::Dictionary { key, value } => todo!(),
                    crate::language::NativeType::Function => todo!(),
                }
            }
            SingleValue::Identifier(_) => todo!(),
        },
        _ => todo!(),
        // Expression::MultipleValues(_) => todo!(),
        // Expression::Statement { expression, operation } => todo!(),
        // Expression::Operation { lhs, rhs, operation } => todo!(),
        // Expression::Evaluation(_) => todo!(),
        // Expression::Function(_) => todo!(),
        // Expression::If(_) => todo!(),
        // Expression::While { condition, body } => todo!(),
        // Expression::For { scoped_variable, iterable, body } => todo!(),
        // Expression::Body(_) => todo!(),
        // Expression::Range(_) => todo!(),
    }
}

fn evaluate_assignment(a: Assignment, state: ScopeState, lhs: Expression, rhs: Expression) {
    let mut references = state.references;
    let key = expect_identifier_or_error(lhs);
    let value = evaluate_rhs(rhs);

    references.insert(key, value);
}
// Runs the lexer and parser.
// Code blob references an overall "expression of code", from a single line to a function declaration and
// implementation.
pub fn run(code_blob: &str, state: ScopeState) -> ScopeState {
    let lexer = Lexer::new(code_blob);
    let tokens = lexer.collect::<Vec<Token>>();
    let parse_result = parser::parse(tokens);
    if let Ok(expr) = parse_result {
        evaluate(state, expr)
    } else {
        // TODO: Handle gracefully
        panic!("Invalid AST")
    }
}

#[cfg(test)]
mod test {
    use super::{run, ScopeState};
}
