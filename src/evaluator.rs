use FunctionCall;

use crate::{call_stack::*, language::*};

fn evaluate_assignment_rhs(
    frame: StackFrame,
    key: &str,
    e: Expression,
    is_constant: bool,
) -> StackFrame {
    // x = 5
    // x = "some string"
    // x = 'c'
    // x = true
    // x = {
    //     ... some code ...
    // }
    // x = [ 1, 2, 3 ]

    let value = match e {
        Expression::ValueLiteral(l) => {
            let representation = l.representation;
            match l.native_type {
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
                    let values = vec![];
                    // NOTE: Requires implementation of array literals
                    StackFrameValue::Array(values)
                }
                NativeType::Dictionary(d) => todo!(),
                NativeType::Function => todo!(),
            }
        }
        Expression::Identifier(i) => {
            let reference = frame.get_reference(&i);
            if let Some(v) = reference {
                todo!();
            } else {
                panic!("Invalid, no reference associated with key")
            }
        }
        // Expression::Assignment(a) => evaluate_assignment(a, state),
        _ => todo!(),
        // Expression::MultipleValues(_) => todo!(),
        // Expression::Statement { expression, operation } => todo!(),
        // Expression::Evaluation(_) => todo!(),
        // Expression::Function(_) => todo!(),
        // Expression::If(_) => todo!(),
        // Expression::While { condition, body } => todo!(),
        // Expression::For { framed_variable, iterable, body } => todo!(),
        // Expression::Body(_) => todo!(),
        // Expression::Range(_) => todo!(),
    };

    let reference = StackFrameReference::new(value, is_constant);

    frame.add_or_mutate_reference(key, reference)
}

// Evaluates the expression in the frame and adds it as a value
pub fn evaluate(frame: StackFrame, expression: Expression) -> StackFrame {
    match expression {
        Expression::Assignment(a) => {
            let key = a.identifier;
            evaluate_assignment_rhs(frame, &key, *a.rhs, a.is_constant)
        }
        Expression::Function(f) => evaluate_function_definition(frame, f),

        Expression::FunctionCall(FunctionCall { identifier, args }) => {
            evaluate_function_call(frame, &identifier, args)
        }
        //Expression::Operation {
        //    lhs,
        //    rhs,
        //    operation,
        //} => evaluate_operation(*lhs, *rhs, operation, state),
        _ => todo!(),
    }
}

fn evaluate_function_call(
    frame: StackFrame,
    identifier: &str,
    args: Vec<Expression>,
) -> StackFrame {
    // TODO:
    // evaluate the body of the function
    // update variables that apply to this stack frame
    todo!()
}

fn evaluate_function_definition(frame: StackFrame, f: Function) -> StackFrame {
    todo!()
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use crate::{
        call_stack::StackFrameReference,
        language::{Expression, NativeType, ValueLiteral},
    };

    use super::{evaluate, StackFrame, StackFrameValue};

    #[test]
    fn add_and_replace_variables_to_frame() {
        let add_int_expression =
            Expression::new_assignment("x", Expression::new_int("5"), None, None, false);
        let expected = StackFrame::new(
            BTreeMap::from([(
                "x".to_string(),
                StackFrameReference::new(StackFrameValue::Int(5), false),
            )]),
            vec![],
        );
        let frame = evaluate(StackFrame::default(), add_int_expression);
        assert_eq!(expected, frame);

        let add_bool_expression =
            Expression::new_assignment("y", Expression::new_bool("true"), None, None, false);

        let expected = StackFrame::new(
            BTreeMap::from([
                (
                    "x".to_string(),
                    StackFrameReference::new(StackFrameValue::Int(5), false),
                ),
                (
                    "y".to_string(),
                    StackFrameReference::new(StackFrameValue::Bool(true), false),
                ),
            ]),
            vec![],
        );

        let frame = evaluate(frame, add_bool_expression);
        assert_eq!(expected, frame);

        let add_float_expression =
            Expression::new_assignment("z", Expression::new_float("3.1"), None, None, false);

        let expected = StackFrame::new(
            BTreeMap::from([
                (
                    "x".to_string(),
                    StackFrameReference::new(StackFrameValue::Int(5), false),
                ),
                (
                    "y".to_string(),
                    StackFrameReference::new(StackFrameValue::Bool(true), false),
                ),
                (
                    "z".to_string(),
                    StackFrameReference::new(StackFrameValue::Float(3.1), false),
                ),
            ]),
            vec![],
        );
        let frame = evaluate(frame, add_float_expression);
        assert_eq!(expected, frame);

        let replace_expression =
            Expression::new_assignment("z", Expression::new_char("c"), None, None, false);

        let expected = StackFrame::new(
            BTreeMap::from([
                (
                    "x".to_string(),
                    StackFrameReference::new(StackFrameValue::Int(5), false),
                ),
                (
                    "y".to_string(),
                    StackFrameReference::new(StackFrameValue::Bool(true), false),
                ),
                (
                    "z".to_string(),
                    StackFrameReference::new(StackFrameValue::Char('c'), false),
                ),
            ]),
            vec![],
        );
        let frame = evaluate(frame, replace_expression);
        assert_eq!(expected, frame);
        // TODO: More tests for the other data types
    }
    /*
    # TODO:
    - [x] Mutate a variable in the state.
    - [x] Save variables into the frame using the variable name.
    - [ ] Save function declaration and implemention into frame.
    - [ ] Run a print function.
     */
}
