use crate::{
    call_stack::{StackFrame, StackFrameReference, StackFrameValue},
    language::{Assignment, Expression, Ident, NativeType, SingleValue, TwoSideOperation},
    parser, Lexer, Token,
};

fn evaluate(frame: StackFrame, expression: Expression) -> StackFrame {
    match expression {
        Expression::Assignment(a) => evaluate_assignment(a, frame),
        Expression::Operation {
            lhs,
            rhs,
            operation,
        } => evaluate_operation(*lhs, *rhs, operation, frame),
        _ => todo!(),
    }
    /*
    # TODO: list:
    - [ ] Save variables into the frame using the variable name.
    - [ ] Save function declaration and implemention.
    - [ ] Run a print function.
    - [ ] Mutate a variable in a future frame.
     */
}

fn evaluate_identifier(frame: StackFrame, ident: Ident) -> StackFrameReference {
    let value_associated = frame.get_reference(&ident);
    if let Some(v) = value_associated {
        v
    } else {
        panic!()
    }
}

fn evaluate_operation(
    lhs: Expression,
    rhs: Expression,
    operation: TwoSideOperation,
    frame: StackFrame,
) -> StackFrame {
    match operation {
        TwoSideOperation::FunctionCall => todo!(),
        TwoSideOperation::Math(_) => todo!(),
    }
}

fn evaluate_assignment_rhs(e: Expression) -> StackFrameReference {
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
                let value = match l.native_type {
                    NativeType::Void => StackFrameValue::Void,
                    NativeType::Char => StackFrameValue::Char(
                        representation.chars().next().expect("This is a character"),
                    ),
                    NativeType::Int => StackFrameValue::Int(
                        str::parse::<i32>(&representation).expect("This should be a number"),
                    ),
                    NativeType::String => StackFrameValue::String(representation),
                    NativeType::Float => StackFrameValue::Float(
                        str::parse::<f32>(&representation)
                            .expect("This should be a floating point number"),
                    ),
                    NativeType::Bool => StackFrameValue::Bool(
                        str::parse::<bool>(&representation).expect("This to be a boolean."),
                    ),
                    NativeType::Array(a) => {
                        let mut values = vec![];
                        // NOTE: Requires implementation of array literals
                        ScopeValue::Array(values)
                    }
                    NativeType::Dictionary { key, value } => todo!(),
                    NativeType::Function => todo!(),
                };
                StackFrameReference::new(value, true)
            }
            SingleValue::Identifier(i) => evaluate_identifier(frame, i),
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
