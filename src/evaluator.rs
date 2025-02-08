use crate::{call_stack::*, language::*};

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
        Expression::SingleValue(s) => {
            let single_value = match s {
                SingleValue::ValueLiteral(l) => {
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
                            let mut values = vec![];
                            // NOTE: Requires implementation of array literals
                            StackFrameValue::Array(values)
                        }
                        NativeType::Dictionary { key, value } => todo!(),
                        NativeType::Function => todo!(),
                    }
                }
                SingleValue::Identifier(i) => {
                    let reference = frame.get_reference(&i);
                    if let Some(v) = reference {
                        todo!();
                    } else {
                        panic!("Invalid, no reference associated with key")
                    }
                }
            };
            single_value
        }
        // Expression::Assignment(a) => evaluate_assignment(a, state),
        _ => todo!(),
        // Expression::MultipleValues(_) => todo!(),
        // Expression::Statement { expression, operation } => todo!(),
        // Expression::Operation { lhs, rhs, operation } => todo!(),
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

fn evaluate_assignment(a: Assignment, frame: StackFrame) -> StackFrame {
    let key = a.identifier;
    evaluate_assignment_rhs(frame, &key, *a.rhs, a.is_constant)
}

// Evaluates the expression in the frame and adds it as a value
pub fn evaluate(expression: Expression, frame: StackFrame) -> StackFrame {
    match expression {
        Expression::Assignment(a) => evaluate_assignment(a, frame),
        Expression::Function(f) => evaluate_function(frame, *f),
        //Expression::Operation {
        //    lhs,
        //    rhs,
        //    operation,
        //} => evaluate_operation(*lhs, *rhs, operation, state),
        _ => todo!(),
    }
}

fn evaluate_function(state: StackFrame, f: Function) -> StackFrame {
    todo!()
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use crate::{
        call_stack::StackFrameReference,
        language::{Assignment, Expression, NativeType, SingleValue, ValueLiteral},
    };

    use super::{evaluate, StackFrame, StackFrameValue};

    #[test]
    fn add_and_replace_variables_to_frame() {
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
        let expected = StackFrame::new(
            BTreeMap::from([(
                "x".to_string(),
                StackFrameReference::new(StackFrameValue::Int(5), false),
            )]),
            vec![],
        );
        let frame = evaluate(add_int_expression, StackFrame::default());
        assert_eq!(expected, frame);

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

        let frame = evaluate(add_bool_expression, frame);
        assert_eq!(expected, frame);

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
        let frame = evaluate(add_float_expression, frame);
        assert_eq!(expected, frame);

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
        let frame = evaluate(replace_expression, frame);
        assert_eq!(expected, frame);
        // TODO: More tests for the other data types
    }
    /*
    # TODO:
    - [x] Mutate a variable in the state.
    - [x] Save variables into the frame using the variable name.
    - [ ] Save function declaration and implemention into state.
    - [ ] Run a print function.
     */
}
