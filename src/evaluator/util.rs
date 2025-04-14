use super::*;

pub fn map_expression_to_string(
    state: EnvironmentState,
    expr: Expression,
) -> Result<(EnvironmentState, String), EvaluatorError> {
    let (new_state, binding) = map_expression_to_binding(state.clone(), expr)?;
    let (new_state, result) = map_binding_to_string(new_state, &binding)?;
    Ok((new_state, result))
}

pub fn map_value_to_string(
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
                    let (maybe_state, result) = map_expression_to_string(new_state, expr.clone())?;
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

            ValueLiteral::Dictionary(d) => {
                let mut new_state = state.clone();
                let mut s = String::from("{");
                let len_array = d.len();
                for (index, (key, expr)) in d.iter().enumerate() {
                    let mut maybe_state = new_state.clone();
                    let key_string = match key {
                        DictionaryKey::String(s) => s.to_string(),
                        DictionaryKey::Int(i) => i.to_string(),
                        DictionaryKey::Bool(b) => b.to_string(),
                        DictionaryKey::Identifier(i) => {
                            let value = get_identifier_binding_recursively(&state, i)?;
                            let (s, string) = map_value_to_string(maybe_state, &value)?;
                            maybe_state = s;
                            string
                        }
                        DictionaryKey::Char(c) => c.to_string(),
                    };
                    let (maybe_state, result) =
                        map_expression_to_string(maybe_state, expr.clone())?;
                    s = format!("{}\n{}: {}{}", s, key_string, result, {
                        if index == len_array - 1 {
                            "\n}"
                        } else {
                            ", "
                        }
                    });

                    new_state = maybe_state;
                }
                s
            }
            ValueLiteral::Function => "function".to_string(),
        },

        Value::Void => "".to_string(),
        Value::Range(r) => r.to_string(),
    };

    Ok((state, s))
}

pub fn map_body_to_string(
    state: EnvironmentState,
    b: Vec<Expression>,
) -> Result<(EnvironmentState, String), EvaluatorError> {
    let mut new_state = state.clone();
    let mut s = String::new();
    for expr in b {
        let (maybe_state, value) = evaluate(new_state, expr)?;

        let (maybe_state, new_s) = map_value_to_string(maybe_state, &value)?;

        s = format!("{}{}", s, new_s);
        new_state = maybe_state;
    }

    Ok((new_state, s))
}

pub fn map_expression_vec_to_string(
    state: EnvironmentState,
    exprs: Vec<Expression>,
) -> Result<(EnvironmentState, Vec<String>), EvaluatorError> {
    let mut strings = vec![];

    let mut new_state = state.clone();

    for expr in exprs {
        let (maybe_state, s) = map_expression_to_string(new_state, expr)?;
        strings.push(s);
        new_state = maybe_state;
    }
    Ok((new_state, strings))
}

pub fn map_binding_to_string(
    state: EnvironmentState,
    binding: &EnvironmentBinding,
) -> Result<(EnvironmentState, String), EvaluatorError> {
    match binding {
        EnvironmentBinding::Value(v) => map_value_to_string(state, v),
        EnvironmentBinding::Function(f) => todo!(),
        EnvironmentBinding::Identifier(i) => match state.get_local_binding(i) {
            Some(binding) => map_binding_to_string(state, &binding),
            None => Err(EvaluatorError::NoSuchIdentifier),
        },
    }
}

// Code adapted from quaternic (2021)
pub fn convert_to_directed_range(a: i32, b: i32) -> impl Iterator<Item = i32> {
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

pub fn map_expression_to_binding(
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
            let (state, from_value) = map_expression_to_value(state, *r.from)?;
            let (state, to_value) = map_expression_to_value(state, *r.to)?;

            let values: Vec<Expression> = match (from_value, to_value) {
                (
                    Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(i1))),
                    Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(i2))),
                ) => {
                    let mut range = convert_to_directed_range(i1, i2).collect::<Vec<i32>>();
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
        Expression::Index(_) => todo!(),
        Expression::Statement(_) => todo!(),
        Expression::Function(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::While(_) => todo!(),
        Expression::For(_) => todo!(),
        Expression::Assignment(a) => evaluate_assignment(state, a)
            .map(|(state, value)| (state, EnvironmentBinding::Value(value))),
    }
}

pub fn map_binding_to_value(
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

pub fn map_expression_to_value(
    state: EnvironmentState,
    expr: Expression,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (state, binding) = map_expression_to_binding(state, expr)?;
    let (state, value) = map_binding_to_value(state, binding)?;
    Ok((state, value))
}

pub fn map_identifier_to_value(
    state: EnvironmentState,
    ident: &str,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let binding = state
        .get_local_binding(ident)
        .ok_or(EvaluatorError::NoSuchIdentifier)?;
    map_binding_to_value(state, binding)
}

pub fn map_dictionary_key_to_value(
    state: EnvironmentState,
    key: DictionaryKey,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match key {
        DictionaryKey::String(s) => Ok((state, Value::new_string(&s))),
        DictionaryKey::Int(i) => Ok((state, Value::new_numeric(NumericLiteral::Int(i)))),
        DictionaryKey::Bool(b) => Ok((state, Value::new_bool(b))),
        DictionaryKey::Identifier(i) => map_identifier_to_value(state, &i),
        DictionaryKey::Char(c) => Ok((state, Value::new_char(c))),
    }
}

pub fn map_value_to_dictionary_key(
    state: EnvironmentState,
    v: Value,
) -> Result<(EnvironmentState, DictionaryKey), EvaluatorError> {
    let key = match v {
        Value::ValueLiteral(ValueLiteral::CharacterBased(CharacterBasedLiteral::String(s))) => {
            DictionaryKey::String(s)
        }
        Value::ValueLiteral(ValueLiteral::CharacterBased(CharacterBasedLiteral::Char(c))) => {
            DictionaryKey::Char(c)
        }
        Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(i))) => DictionaryKey::Int(i),
        Value::ValueLiteral(ValueLiteral::Bool(b)) => DictionaryKey::Bool(b),
        _ => return Err(EvaluatorError::InvalidDictionaryKey),
    };

    Ok((state, key))
}

pub fn get_identifier_binding(
    state: &EnvironmentState,
    symbol: &str,
) -> Result<EnvironmentBinding, EvaluatorError> {
    state
        .get_local_binding(symbol)
        .map_or(Err(EvaluatorError::NoSuchIdentifier), |binding| {
            Ok(binding.clone())
        })
}
pub fn get_identifier_binding_recursively(
    state: &EnvironmentState,
    symbol: &str,
) -> Result<Value, EvaluatorError> {
    state
        .get_local_binding(symbol)
        .map_or(
            Err(EvaluatorError::NoSuchIdentifier),
            |binding| match binding {
                EnvironmentBinding::Value(v) => Ok(v.clone()),
                EnvironmentBinding::Function(_) => todo!(),
                EnvironmentBinding::Identifier(i) => get_identifier_binding_recursively(state, &i),
            },
        )
}
