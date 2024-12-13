use std::collections::HashMap;

use crate::{
    language::{Assignment, Expression, Function, NativeType, SingleValue, TwoSideOperation},
    parser, Lexer, Token,
};

enum ScopeValue {
    Void,
    Int(i32),
    String(String),
    Char(char),
    Array(Vec<ScopeValue>),
    Dictionary(HashMap<ScopeValue, ScopeValue>),
    Bool(bool),
    Float(f32),
    Function(Function),
}

pub struct Scope {
    //            name of reference -> the value
    //            e.g. x            -> 5
    //            e.g. print        -> Function that prints to stdout
    scope_values: HashMap<String, ScopeValue>,
    inner_scopes: Vec<Scope>,
}

impl Scope {
    pub fn new_init() -> Self {
        Self {
            scope_values: HashMap::new(),
            inner_scopes: Vec::new(),
        }
    }

    pub fn new(scope_values: HashMap<String, ScopeValue>, inner_scopes: Vec<Scope>) -> Self {
        Self {
            scope_values,
            inner_scopes,
        }
    }

    pub fn add_reference(mut self, key: String, reference: ScopeValue) -> Self {
        self.scope_values.insert(key, reference);
        self
    }
}

fn evaluate(state: Scope, expression: Expression) -> Scope {
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
    - [ ] Save variables into the scope using the variable name.
    - [ ] Save function declaration and implemention into state.
    - [ ] Run a print function.
    - [ ] Mutate a variable in the state.
     */
    todo!()
}

fn evaluate_operation(lhs: Expression, rhs: Expression, operation: TwoSideOperation, state: Scope) {
    match operation {
        TwoSideOperation::FunctionCall => todo!(),
        TwoSideOperation::Math(_) => todo!(),
        TwoSideOperation::Assignment(a) => evaluate_assignment(a, state, lhs, rhs),
    }
}

fn expect_identifier_or_error(e: Expression) -> String {
    if let Expression::SingleValue(ref s) = e {
        if let SingleValue::Identifier(i) = s {
            return i.to_string();
        }
    };
    panic!("Invalid syntax. Expected identifier, found {:?}", e)
}

fn evaluate_rhs(e: Expression) -> ScopeValue {
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
                    NativeType::Void => ScopeValue::Void,
                    NativeType::Char => ScopeValue::Char(
                        representation.chars().next().expect("This is a character"),
                    ),
                    NativeType::Int => ScopeValue::Int(
                        str::parse::<i32>(&representation).expect("This should be a number"),
                    ),
                    NativeType::String => ScopeValue::String(representation),
                    NativeType::Float => ScopeValue::Float(
                        str::parse::<f32>(&representation)
                            .expect("This should be a floating point number"),
                    ),
                    NativeType::Bool => ScopeValue::Bool(
                        str::parse::<bool>(&representation).expect("This to be a boolean."),
                    ),
                    NativeType::Array(a) => {
                        let mut values = vec![];

                        ScopeValue::Array(values)
                    }
                    NativeType::Dictionary { key, value } => todo!(),
                    NativeType::Function => todo!(),
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

fn evaluate_assignment(a: Assignment, state: Scope, lhs: Expression, rhs: Expression) {
    let mut references = state.scope_values;
    let key = expect_identifier_or_error(lhs);
    let value = evaluate_rhs(rhs);

    references.insert(key, value);
}
// Runs the lexer and parser.
// Code blob references an overall "expression of code", from a single line to a function declaration and
// implementation.
pub fn run(code_blob: &str, state: Scope) -> Scope {
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
    #[test]
    fn it_runs() {}
}
