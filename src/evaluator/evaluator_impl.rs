use std::collections::HashMap;

use evaluation::evaluate_bindings;
use iterate::{iterate_array, iterate_dict};

use macros::dbg;

use super::*;

pub fn evaluate_range(
    state: EnvironmentState,
    r: Range,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (state, from_value) = map_expression_to_value(state, *r.from)?;
    let (state, to_value) = map_expression_to_value(state, *r.to)?;

    Ok((
        state,
        match (from_value, to_value) {
            (
                Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(i1))),
                Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(i2))),
            ) => Value::Range(RangeValue::new(i1, i2, r.inclusive)),
            _ => return Err(EvaluatorError::InvalidIterable),
        },
    ))
}

pub fn evaluate_index(
    state: EnvironmentState,
    i: Index,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (state, index_value) = map_expression_to_value(state, *i.index)?;
    let (state, collection_value) = map_expression_to_value(state, *i.collection)?;

    match collection_value {
        Value::ValueLiteral(ValueLiteral::Array(a)) => index_array(state, index_value, a),
        Value::ValueLiteral(ValueLiteral::Dictionary(d)) => index_dict(state, index_value, d),
        _ => Err(EvaluatorError::NotACollection),
    }
}

pub fn index_dict(
    state: EnvironmentState,
    value: Value,
    d: HashMap<DictionaryKey, Expression>,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (state, dict_key) = map_value_to_dictionary_key(state, value)?;
    let result = d.get(&dict_key);
    if let Some(expr) = result {
        map_expression_to_value(state, expr.clone())
    } else {
        Ok((state, Value::Void))
    }
}

pub fn index_array(
    state: EnvironmentState,
    value: Value,
    a: Vec<Expression>,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    if let Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(i))) = value {
        let result = a.get(i as usize);
        if let Some(v) = result {
            let (state, value) = map_expression_to_value(state, v.clone())?;
            Ok((state, value))
        } else {
            Err(EvaluatorError::IndexOutOfRange)
        }
    } else {
        Err(EvaluatorError::InvalidIndex)
    }
}

pub fn evaluate_for(
    state: EnvironmentState,
    f: For,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let mut new_state = state.clone();
    let for_scope = Scope::new(Some(state.current_scope.clone()));
    new_state.current_scope = for_scope;

    let (mut new_state, result) = map_expression_to_value(new_state, *f.iterable)?;
    match result {
        Value::ValueLiteral(ValueLiteral::Array(a)) => {
            let (maybe_state, _) = iterate_array(new_state, a, f.scoped_variable, f.body.to_vec())?;
            new_state = maybe_state;
        }
        Value::ValueLiteral(ValueLiteral::Dictionary(d)) => {
            let (maybe_state, _) = iterate_dict(state, d, f.scoped_variable, f.body.to_vec())?;
            new_state = maybe_state;
        }
        Value::ValueLiteral(ValueLiteral::CharacterBased(CharacterBasedLiteral::String(s))) => {
            let (maybe_state, _) = iterate_array(
                state,
                s.chars()
                    .map(|c| Expression::new_char(&c))
                    .collect::<Vec<Expression>>(),
                f.scoped_variable,
                f.body,
            )?;
            new_state = maybe_state;
        }
        _ => return Err(EvaluatorError::InvalidIterable),
    }

    Ok((new_state, Value::Void))
}

pub fn evaluate_while(
    state: EnvironmentState,
    w: While,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let mut new_state = state.clone();
    let while_scope = Scope::new(Some(state.current_scope.clone()));
    new_state.current_scope = while_scope;

    loop {
        let (maybe_state, v) = match w.condition.as_deref() {
            Some(Expression::Evaluation(e)) => evaluate_evaluation(new_state, e.clone())?,
            _ => (new_state, Value::new_bool(true)),
        };

        match v {
            Value::ValueLiteral(ValueLiteral::Bool(true)) => {
                let (maybe_state, _) = evaluate_body(maybe_state, w.body.clone())?;
                new_state = maybe_state;
            }
            _ => return Ok((state, Value::Void)),
        }
    }
}

pub fn evaluate_if(
    state: EnvironmentState,
    if_expression: crate::language::If,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    if let Expression::Evaluation(e) = *if_expression.evaluation {
        let (state, v) = evaluate_evaluation(state, e)?;
        if let Value::ValueLiteral(ValueLiteral::Bool(is_true_eval)) = v {
            match (is_true_eval, if_expression.on_false_evaluation) {
                (true, _) => {
                    let mut state = state.clone();
                    let (mut state, result) =
                        evaluate(state.push_scope(), *if_expression.on_true_evaluation)?;
                    let state = state.pop_scope();
                    return Ok((state, result));
                }
                (false, Some(e)) => return evaluate(state.clone(), *e),
                (false, None) => return Ok((state, Value::Void)),
            }
        }
    }
    Err(EvaluatorError::InvalidEvaluation)
}

pub fn evaluate_evaluation(
    state: EnvironmentState,
    e: Evaluation,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (state, lhs_binding) = map_expression_to_binding(state, *e.lhs)?;

    let (state, rhs_binding) = if let Some(expr) = e.rhs {
        let (s, b) = map_expression_to_binding(state, *expr)?;
        (s, Some(b))
    } else {
        (state, None)
    };

    let (state, b) = evaluate_bindings(state, lhs_binding, rhs_binding, e.evaluation_op)?;
    Ok((state, Value::new_bool(b)))
}

pub fn evaluate_body(
    state: EnvironmentState,
    body: Body,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (mut state, mut value) = (state.clone(), Value::Void);
    for expr in body {
        (state, value) = evaluate(state, expr)?;
    }
    Ok((state, value))
}

pub fn evaluate_assignment(
    state: EnvironmentState,
    a: Assignment,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (state, binding) = match *a.rhs {
        Expression::ValueLiteral(v) => (state, (EnvironmentBinding::Value(Value::ValueLiteral(v)))),
        Expression::Identifier(i) => {
            let get_identifier_binding = get_identifier_binding_recursively(&state, &i)?;

            (state, EnvironmentBinding::Value(get_identifier_binding))
        }
        Expression::Statement(_) => return Err(EvaluatorError::InvalidAssignment),
        Expression::MathOperation(math_op) => {
            let (state, value) = evaluate_operation(state, math_op)?;
            let (state, _) = add_binding_or_error(
                state,
                &a.identifier,
                EnvironmentBinding::Value(value.clone()),
            )?;
            (state, EnvironmentBinding::Value(value))
        }
        Expression::Evaluation(eval) => {
            let (state, lhs) = map_expression_to_binding(state, *eval.lhs)?;
            let (state, rhs) = if let Some(s) = eval.rhs {
                let (state, binding) = map_expression_to_binding(state, *s)?;
                (state, Some(binding))
            } else {
                (state, None)
            };

            let (state, eval) = evaluate_bindings(state, lhs, rhs, eval.evaluation_op)?;
            (state, EnvironmentBinding::new_bool(eval))
        }
        Expression::If(i) => {
            let (state, v) = evaluate_if(state, i)?;
            (state, EnvironmentBinding::Value(v))
        }
        Expression::Body(b) => evaluate_body(state, b)
            .map(|(state, value)| (state, EnvironmentBinding::Value(value)))?,
        Expression::Range(r) => {
            let (state, value) = evaluate_range(state, r)?;
            (state, EnvironmentBinding::Value(value))
        }
        Expression::FunctionCall(fc) => {
            let (state, value) = evaluate_function_call(state, fc)?;
            (state, EnvironmentBinding::Value(value))
        }
        Expression::Index(index) => {
            let (state, value) = evaluate_index(state, index)?;
            (state, EnvironmentBinding::Value(value))
        }
        _ => return Err(EvaluatorError::InvalidAssignment),
    };

    let (state, _) = add_binding_or_error(state, &a.identifier, binding)?;

    Ok((state, Value::Void))
}

pub fn evaluate_function(
    state: EnvironmentState,
    f: Function,
    args: Vec<Expression>,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    if args.len() != f.params.len() {
        return Err(EvaluatorError::InvalidNumberOfArguments);
    }

    let mut new_state = state.clone().push_scope();

    for (arg, parameter) in args.into_iter().zip(f.params) {
        let (mut maybe_state, binding) = map_expression_to_binding(new_state, arg)?;

        let _ = maybe_state.add_or_mutate_symbols(&parameter.ident, binding)?;
        new_state = maybe_state;
    }
    let (mut new_state, result) = evaluate(new_state, *f.body)?;

    Ok((new_state.pop_scope(), result))
}

pub fn evaluate_identifier(
    state: EnvironmentState,
    ident: &str,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let maybe_rust_binding = state.std_lib_symbols.get(ident).cloned();
    if let Some(std) = maybe_rust_binding {
        return handle_rust_binding_with_args(state, &std, vec![]);
    }

    let symbol = state.get_local_binding(ident);

    match symbol {
        Some(binding) => match binding {
            EnvironmentBinding::Value(v) => Ok((state, v)),
            EnvironmentBinding::Identifier(i) => evaluate_identifier(state, &i),
            EnvironmentBinding::Function(f) => evaluate_function(state, f, vec![]),
        },
        None => Err(EvaluatorError::NoSuchIdentifier {
            ident: ident.to_string(),
        }),
    }
}

pub fn evaluate_statement(
    state: EnvironmentState,
    s: Statement,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match s.operation {
        StatementOperator::Break => Ok((state, Value::Void)),
        StatementOperator::Continue => Ok((state, Value::Void)),
        StatementOperator::Return => evaluate_return(state, s.expression.as_deref().cloned()),
        StatementOperator::Include => Err(EvaluatorError::NotYetImplemented {
            msg: "include statements are not yet supported".to_string(),
        }),
    }
}

pub fn evaluate_return(
    state: EnvironmentState,
    body: Option<Expression>,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let expr = if let Some(b) = body {
        b
    } else {
        return Ok((state, Value::Void));
    };
    let (state, value) = map_expression_to_value(state, expr)?;
    Ok((state, value))
}

pub fn handle_rust_binding_with_args(
    state: EnvironmentState,
    rb: &RustBinding,
    args: Vec<Expression>,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match rb {
        RustBinding::Print(std_print) => {
            let (new_state, args) = map_expression_vec_to_string(state, args)?;
            std_print(args);
            Ok((new_state, Value::Void))
        }
        RustBinding::Len => {
            if args.len() > 1 {
                return Err(EvaluatorError::InvalidNumberOfArguments);
            }
            let first = args.first().cloned();
            if let Some(expr) = first {
                let (new_state, value) = map_expression_to_value(state, expr)?;
                let len = match value {
                    Value::ValueLiteral(ValueLiteral::Array(a)) => a.len(),
                    Value::ValueLiteral(ValueLiteral::Dictionary(d)) => d.len(),
                    Value::ValueLiteral(ValueLiteral::CharacterBased(
                        CharacterBasedLiteral::String(s),
                    )) => s.len(),
                    _ => return Err(EvaluatorError::InvalidArgumentType),
                };
                Ok((
                    new_state,
                    Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(len as i32))),
                ))
            } else {
                Ok((
                    state,
                    Value::ValueLiteral(ValueLiteral::Numeric(NumericLiteral::Int(0))),
                ))
            }
        }
        RustBinding::Append => {
            let list = args.first();
            let item = args.last();
            match (list, item) {
                (Some(l), Some(i)) => {
                    let (state, array_value) = map_expression_to_value(state, l.clone())?;

                    if let Value::ValueLiteral(ValueLiteral::Array(mut a)) = array_value {
                        let (state, value) = map_expression_to_value(state, i.clone())?;

                        let new_expr = if let Value::ValueLiteral(vl) = value {
                            Expression::ValueLiteral(vl)
                        } else {
                            return Err(EvaluatorError::InvalidArgumentType);
                        };

                        a.push(new_expr);
                        Ok((state, Value::ValueLiteral(ValueLiteral::Array(a))))
                    } else {
                        Err(EvaluatorError::InvalidArgumentType)
                    }
                }
                _ => Err(EvaluatorError::InvalidNumberOfArguments),
            }
        }
    }
}

pub fn evaluate_function_call(
    state: EnvironmentState,
    fc: FunctionCall,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    if let Some(std) = state.std_lib_symbols.get(&fc.identifier).cloned() {
        let (state, result) = handle_rust_binding_with_args(state, &std, fc.args)?;
        return Ok((state, result));
    }

    if let Some(EnvironmentBinding::Function(f)) = state.get_local_binding(&fc.identifier) {
        evaluate_function(state, f, fc.args)
    } else {
        dbg!(&state);
        Err(EvaluatorError::InvalidFunctionCall {
            ident: fc.identifier,
        })
    }
}

pub fn add_binding_or_error(
    state: EnvironmentState,
    symbol: &str,
    binding: EnvironmentBinding,
) -> Result<(EnvironmentState, EnvironmentBinding), EvaluatorError> {
    let mut state = state.clone();
    let binding = state.add_or_mutate_symbols(symbol, binding)?;
    dbg!("Add binding or error", &state);
    Ok((state, binding))
}
