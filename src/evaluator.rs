use std::collections::HashMap;

use crate::language::{Assignment, Expression, Function, NativeType, SingleValue};
#[derive(PartialEq, Debug)]
pub enum ScopeValue {
    Void,
    Int(i32),
    String(String),
    Char(char),
    Array(Vec<ScopeValue>),
    Bool(bool),
    Float(f32),
    Function(Function),
}

#[derive(Default, Debug, PartialEq)]
pub struct Scope {
    //            name of reference -> the value
    //            e.g. x            -> 5
    //            e.g. print        -> Function that prints to stdout
    scope_values: HashMap<String, ScopeValue>,
    inner_scopes: Vec<Scope>,
}

impl Scope {
    pub fn new(scope_values: HashMap<String, ScopeValue>, inner_scopes: Vec<Scope>) -> Self {
        Self {
            scope_values,
            inner_scopes,
        }
    }

    fn add_reference(mut self, key: String, value: ScopeValue) -> Self {
        self.scope_values.insert(key, value);
        self
    }
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
                        // NOTE: Requires implementation of array literals
                        ScopeValue::Array(values)
                    }
                    NativeType::Dictionary { key, value } => todo!(),
                    NativeType::Function => todo!(),
                }
            }
            SingleValue::Identifier(_) => todo!(),
        },
        // Expression::Assignment(a) => evaluate_assignment(a, state),
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

fn evaluate_assignment(a: Assignment, state: Scope) -> Scope {
    let key = a.identifier;
    let value = evaluate_rhs(*a.rhs);
    state.add_reference(key, value)
}

// Evaluates the expression in the scope and adds it as a value
pub fn evaluate(expression: Expression, state: Scope) -> Scope {
    match expression {
        Expression::Assignment(a) => evaluate_assignment(a, state),
        Expression::Function(f) => evaluate_function(*f, state),
        //Expression::Operation {
        //    lhs,
        //    rhs,
        //    operation,
        //} => evaluate_operation(*lhs, *rhs, operation, state),
        _ => todo!(),
    }
}

fn evaluate_function(f: Function, state: Scope) -> Scope {
    todo!()
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::language::{Assignment, Expression, NativeType, SingleValue, ValueLiteral};

    use super::{evaluate, Scope, ScopeValue};

    #[test]
    fn add_and_replace_variables_to_scope() {
        let add_int_expression = Expression::Assignment(Assignment::new(
            "x",
            Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new(
                NativeType::Int,
                "5",
            ))),
            None,
            None,
            false,
        ));
        let expected = Scope::new(
            HashMap::from([("x".to_string(), ScopeValue::Int(5))]),
            vec![],
        );
        let scope = evaluate(add_int_expression, Scope::default());
        assert_eq!(expected, scope);

        let add_bool_expression = Expression::Assignment(Assignment::new(
            "y",
            Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new(
                NativeType::Bool,
                "true",
            ))),
            None,
            None,
            false,
        ));

        let expected = Scope::new(
            HashMap::from([
                ("x".to_string(), ScopeValue::Int(5)),
                ("y".to_string(), ScopeValue::Bool(true)),
            ]),
            vec![],
        );

        let scope = evaluate(add_bool_expression, scope);
        assert_eq!(expected, scope);

        let add_float_expression = Expression::Assignment(Assignment::new(
            "z",
            Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new(
                NativeType::Float,
                "3.1",
            ))),
            None,
            None,
            false,
        ));

        let expected = Scope::new(
            HashMap::from([
                ("x".to_string(), ScopeValue::Int(5)),
                ("y".to_string(), ScopeValue::Bool(true)),
                ("z".to_string(), ScopeValue::Float(3.1)),
            ]),
            vec![],
        );
        let scope = evaluate(add_float_expression, scope);
        assert_eq!(expected, scope);

        let replace_expression = Expression::Assignment(Assignment::new(
            "z",
            Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new(
                NativeType::Char,
                "c",
            ))),
            None,
            None,
            false,
        ));

        let expected = Scope::new(
            HashMap::from([
                ("x".to_string(), ScopeValue::Int(5)),
                ("y".to_string(), ScopeValue::Bool(true)),
                ("z".to_string(), ScopeValue::Char('c')),
            ]),
            vec![],
        );
        let scope = evaluate(replace_expression, scope);
        assert_eq!(expected, scope);
        // TODO: More tests for the other data types
    }
    /*
    # TODO:
    - [x] Mutate a variable in the state.
    - [x] Save variables into the scope using the variable name.
    - [ ] Save function declaration and implemention into state.
    - [ ] Run a print function.
     */
}
