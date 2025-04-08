use super::*;

pub fn get_string_from_expression(
    state: EnvironmentState,
    expr: Expression,
) -> (EnvironmentState, Result<String, EvaluatorError>) {
    let (new_state, result) = get_binding_from_expression(state.clone(), expr);
    let b = match result {
        Ok(binding) => binding,
        Err(e) => return (state, Err(e)),
    };
    let (new_state, result) = get_string_from_binding(new_state, &b);

    (new_state, result)
}

pub fn value_literal_to_string(
    state: EnvironmentState,
    vl: &ValueLiteral,
) -> (EnvironmentState, Result<String, EvaluatorError>) {
    let s = match vl {
        ValueLiteral::CharacterBased(c) => c.to_string(),
        ValueLiteral::Numeric(n) => n.to_string(),
        ValueLiteral::Bool(b) => b.to_string(),
        ValueLiteral::Array(array) => {
            let mut new_state = state.clone();
            let mut s = String::from("[");
            let len_array = array.len();
            for (index, expr) in array.iter().enumerate() {
                let (maybe_state, result) = get_string_from_expression(new_state, expr.clone());
                match result {
                    Ok(v) => {
                        s = format!("{}{}{}", s, v, {
                            if index == len_array - 1 {
                                "]"
                            } else {
                                ", "
                            }
                        })
                    }
                    Err(e) => return (state, Err(e)),
                }
                new_state = maybe_state;
            }
            return (new_state, Ok(s));
        }
        ValueLiteral::Dictionary(_) => todo!(),
        ValueLiteral::Function => todo!(),
    };
    (state, Ok(s))
}

pub fn get_body_string_value(
    state: EnvironmentState,
    b: Vec<Expression>,
) -> (EnvironmentState, Result<String, EvaluatorError>) {
    let mut new_state = state.clone();
    let mut s = String::new();
    for expr in b {
        let (maybe_state, new_s) = match evaluate(new_state, expr) {
            (maybe_state, Ok(v)) => match v {
                Value::ValueLiteral(vl) => match value_literal_to_string(maybe_state, &vl) {
                    (maybe_state, Ok(new_s)) => (maybe_state, new_s),
                    (_, Err(e)) => return (state, Err(e)),
                },
                Value::Void => (maybe_state, "".to_string()),
            },
            (_, Err(e)) => return (state, Err(e)),
        };
        s = format!("{}{}", s, new_s);
        new_state = maybe_state;
    }

    (new_state, Ok(s))
}

pub fn get_string_from_binding(
    state: EnvironmentState,
    binding: &EnvironmentBinding,
) -> (EnvironmentState, Result<String, EvaluatorError>) {
    match binding {
        EnvironmentBinding::Value(v) => match v {
            Value::ValueLiteral(vl) => value_literal_to_string(state, vl),
            Value::Void => (state, Ok(String::new())),
        },
        EnvironmentBinding::Function(f) => todo!(),
        EnvironmentBinding::Identifier(i) => match state.get_local_binding(i) {
            Some(binding) => get_string_from_binding(state, &binding),
            None => (state, Err(EvaluatorError::NoSuchIdentifier)),
        },
        EnvironmentBinding::Range(_) => todo!(),
    }
}
pub fn get_binding_from_expression(
    state: EnvironmentState,
    expr: Expression,
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    let binding = match expr {
        Expression::ValueLiteral(v) => EnvironmentBinding::Value(Value::ValueLiteral(v)),
        Expression::Identifier(i) => {
            let result = get_identifier_binding(&state, &i);
            match result {
                Ok(v) => v,
                Err(e) => return (state, Err(e)),
            }
        }
        Expression::Evaluation(_) => todo!(),
        Expression::FunctionCall(_) => todo!(),
        Expression::Body(e) => todo!(),
        _ => return (state, Err(EvaluatorError::InvalidEvaluation)),
    };
    (state, Ok(binding))
}
