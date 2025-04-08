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
        EnvironmentBinding::Range(_) => todo!(),
    }
}
pub fn get_binding_from_expression(
    state: EnvironmentState,
    expr: Expression,
) -> Result<(EnvironmentState, EnvironmentBinding), EvaluatorError> {
    let binding = match expr {
        Expression::ValueLiteral(v) => EnvironmentBinding::Value(Value::ValueLiteral(v)),
        Expression::Identifier(i) => {
            let result = get_identifier_binding(&state, &i);
            match result {
                Ok(v) => v,
                Err(e) => return Err(e),
            }
        }
        Expression::Evaluation(_) => todo!(),
        Expression::FunctionCall(_) => todo!(),
        Expression::Body(e) => todo!(),
        _ => return Err(EvaluatorError::InvalidEvaluation),
    };
    Ok((state, binding))
}
