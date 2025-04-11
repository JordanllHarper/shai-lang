use super::*;

pub fn get_string_from_expression(
    state: EnvironmentState,
    expr: Expression,
) -> Result<(EnvironmentState, String), EvaluatorError> {
    let (new_state, binding) = get_binding_from_expression(state.clone(), expr)?;
    let (new_state, result) = get_string_from_binding(new_state, &binding)?;
    Ok((new_state, result))
}

pub fn value_to_string(
    state: EnvironmentState,
    v: &Value,
) -> Result<(EnvironmentState, String), EvaluatorError> {
    let s = match v {
        Value::ValueLiteral(vl) => match vl {
            ValueLiteral::CharacterBased(c) => c.to_string(),
            ValueLiteral::Numeric(n) => n.to_string(),
            ValueLiteral::Bool(b) => b.to_string(),
            ValueLiteral::Array(array) => {
                let mut new_state = state.clone();
                let mut s = String::from("[");
                let len_array = array.len();
                for (index, expr) in array.iter().enumerate() {
                    let (maybe_state, result) =
                        get_string_from_expression(new_state, expr.clone())?;
                    s = format!("{}{}{}", s, result, {
                        if index == len_array - 1 {
                            "]"
                        } else {
                            ", "
                        }
                    });

                    new_state = maybe_state;
                }
                s
            }

            ValueLiteral::Dictionary(_) => todo!(),
            ValueLiteral::Function => todo!(),
        },

        Value::Void => "".to_string(),
        Value::Range(r) => r.to_string(),
    };

    Ok((state, s))
}

pub fn get_body_string_value(
    state: EnvironmentState,
    b: Vec<Expression>,
) -> Result<(EnvironmentState, String), EvaluatorError> {
    let mut new_state = state.clone();
    let mut s = String::new();
    for expr in b {
        let (maybe_state, value) = evaluate(new_state, expr)?;

        let (maybe_state, new_s) = value_to_string(maybe_state, &value)?;

        s = format!("{}{}", s, new_s);
        new_state = maybe_state;
    }

    Ok((new_state, s))
}

pub fn get_string_from_binding(
    state: EnvironmentState,
    binding: &EnvironmentBinding,
) -> Result<(EnvironmentState, String), EvaluatorError> {
    match binding {
        EnvironmentBinding::Value(v) => value_to_string(state, v),
        EnvironmentBinding::Function(f) => todo!(),
        EnvironmentBinding::Identifier(i) => match state.get_local_binding(i) {
            Some(binding) => get_string_from_binding(state, &binding),
            None => Err(EvaluatorError::NoSuchIdentifier),
        },
    }
}

// Code adapted from quaternic (2021)
pub fn directed_range(a: i32, b: i32) -> impl Iterator<Item = i32> {
    let mut start = a;
    let end = b;
    std::iter::from_fn(move || {
        use std::cmp::Ordering::*;
        match start.cmp(&end) {
            Less => {
                start += 1;
                Some(start - 1)
            }
            Equal => None,
            Greater => {
                start -= 1;
                Some(start + 1)
            }
        }
    })
}

pub fn get_binding_from_expression(
    state: EnvironmentState,
    expr: Expression,
) -> Result<(EnvironmentState, EnvironmentBinding), EvaluatorError> {
    match expr {
        Expression::ValueLiteral(v) => {
            Ok((state, EnvironmentBinding::Value(Value::ValueLiteral(v))))
        }
        Expression::Identifier(i) => {
            let binding = get_identifier_binding(&state, &i)?;
            Ok((state, binding))
        }
        Expression::Evaluation(_) => todo!(),
        Expression::FunctionCall(_) => todo!(),
        Expression::Body(e) => {
            let (state, body_value) = evaluate_body(state, e)?;
            Ok((state, EnvironmentBinding::Value(body_value)))
        }
        // NOTE: This generates a list of numbers for the user. An optimization here could be to
        // lazy load each number rather than generate this whole list.
        Expression::Range(r) => {
            println!("{:?}", r);
            let (state, from_value) = get_values_from_expression(state, *r.from)?;
            let (state, to_value) = get_values_from_expression(state, *r.to)?;

            let values: Vec<Expression> = match (from_value, to_value) {
                (
                    Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(i1))),
                    Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(i2))),
                ) => {
                    let mut range = directed_range(i1, i2).collect::<Vec<i32>>();
                    if r.inclusive {
                        range.push(i2);
                    };

                    range
                        .into_iter()
                        .map(Expression::new_int)
                        .collect::<Vec<Expression>>()
                }
                _ => return Err(EvaluatorError::InvalidIterable),
            };

            Ok((state, EnvironmentBinding::new_arr(values)))
        }
        Expression::MathOperation(o) => {
            let (state, value) = evaluate_operation(state, o)?;
            Ok((state, EnvironmentBinding::Value(value)))
        }
        _ => todo!(),
    }
}

pub fn get_values_from_binding(
    state: EnvironmentState,
    binding: EnvironmentBinding,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match binding {
        EnvironmentBinding::Value(v) => Ok((state, v)),
        EnvironmentBinding::Function(f) => evaluate_function(state, f, vec![]),
        EnvironmentBinding::Identifier(i) => {
            let value = get_identifier_binding_recursively(&state, &i)?;
            Ok((state, value))
        }
    }
}

pub fn get_values_from_expression(
    state: EnvironmentState,
    expr: Expression,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (state, binding) = get_binding_from_expression(state, expr)?;
    let (state, value) = get_values_from_binding(state, binding)?;
    Ok((state, value))
}
