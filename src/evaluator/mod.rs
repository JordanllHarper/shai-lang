pub mod bindings;
pub mod environment;
pub mod evaluation;
pub mod iterate;
pub mod operation;
pub mod util;

use std::collections::HashMap;

use crate::language::*;
use bindings::*;
use environment::*;
use evaluation::*;
use iterate::{iterate_array, iterate_dict};
use operation::*;
use util::*;

#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    InvalidRedeclaration,
    NoSuchIdentifier,
    InvalidFunctionCall,
    InvalidEvaluation,
    NotYetImplemented,
    NotABooleanValue,
    InvalidNumberOfArguments,
    InvalidOperationValue,
    InvalidIterable,
    InvalidForLoopIdentifier,
    IndexOutOfRange,
    InvalidIndex,
    NotACollection,
    InvalidDictionaryKey,
}

pub fn evaluate(
    state: EnvironmentState,
    ast: Expression,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match ast {
        Expression::ValueLiteral(v) => Ok((state, Value::ValueLiteral(v))),
        Expression::Identifier(ident) => evaluate_identifier(state, &ident),
        Expression::Statement(s) => evaluate_statement(state, s),
        Expression::MathOperation(m) => evaluate_operation(state, m),
        Expression::Evaluation(b) => evaluate_evaluation(state, b),
        Expression::Function(f) => {
            add_binding_or_error(state, &f.ident.clone(), EnvironmentBinding::Function(f))
        }
        Expression::If(if_expression) => evaluate_if(state, if_expression),
        Expression::While(w) => evaluate_while(state, w),
        Expression::For(f) => evaluate_for(state, f),
        Expression::Body(b) => evaluate_body(state, b),
        Expression::Range(r) => evaluate_range(state, r),
        Expression::Assignment(a) => evaluate_assignment(state, a),
        Expression::FunctionCall(f) => evaluate_function_call(state, f),
        Expression::Index(i) => evaluate_index(state, i),
    }
}

fn evaluate_range(
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

fn evaluate_index(
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

fn index_dict(
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

fn index_array(
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

fn evaluate_for(
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

fn evaluate_while(
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

fn evaluate_if(
    state: EnvironmentState,
    if_expression: crate::language::If,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    if let Expression::Evaluation(e) = *if_expression.evaluation {
        let (state, v) = evaluate_evaluation(state, e)?;
        if let Value::ValueLiteral(ValueLiteral::Bool(is_true_eval)) = v {
            match (is_true_eval, if_expression.on_false_evaluation) {
                (true, _) => return evaluate(state.clone(), *if_expression.on_true_evaluation),
                (false, None) => return Ok((state, Value::Void)),
                (false, Some(e)) => return evaluate(state.clone(), *e),
            }
        }
    }
    Err(EvaluatorError::InvalidEvaluation)
}

fn evaluate_evaluation(
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

fn evaluate_body(
    state: EnvironmentState,
    body: Body,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let mut next_state = state.clone();
    let mut next_value = Value::Void;
    for expr in body {
        let (maybe_state, result) = evaluate(next_state, expr)?;
        next_state = maybe_state;
        next_value = result;
    }
    Ok((next_state, next_value))
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

fn evaluate_assignment_expression(
    state: EnvironmentState,
    expr: Expression,
) -> Result<(EnvironmentState, EnvironmentBinding), EvaluatorError> {
    match expr {
        Expression::ValueLiteral(v) => {
            Ok((state, (EnvironmentBinding::Value(Value::ValueLiteral(v)))))
        }
        Expression::Identifier(i) => {
            let get_identifier_binding = get_identifier_binding(&state, &i)?;
            Ok((state, get_identifier_binding))
        }
        Expression::Statement(_) => todo!(),
        Expression::MathOperation(math_op) => {
            let (state, value) = evaluate_operation(state, math_op)?;
            Ok((state, EnvironmentBinding::Value(value)))
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
            Ok((state, EnvironmentBinding::new_bool(eval)))
        }
        Expression::Function(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::While(_) => todo!(),
        Expression::For(_) => todo!(),
        Expression::Body(_) => todo!(),
        Expression::Range(r) => {
            let (state, value) = evaluate_range(state, r)?;
            Ok((state, EnvironmentBinding::Value(value)))
        }
        Expression::Assignment(_) => todo!(),
        Expression::FunctionCall(fc) => {
            let (state, value) = evaluate_function_call(state, fc)?;
            Ok((state, EnvironmentBinding::Value(value)))
        }
        Expression::Index(index) => {
            let (state, value) = evaluate_index(state, index)?;
            Ok((state, EnvironmentBinding::Value(value)))
        }
    }
}

fn evaluate_assignment(
    state: EnvironmentState,
    a: Assignment,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (new_state, binding) = evaluate_assignment_expression(state, *a.rhs)?;
    add_binding_or_error(new_state, &a.identifier, binding)
}

fn evaluate_function(
    state: EnvironmentState,
    f: Function,
    args: Vec<Expression>,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    if args.len() != f.params.len() {
        return Err(EvaluatorError::InvalidNumberOfArguments);
    }

    let mut new_state = state.clone();

    for (arg, parameter) in args.into_iter().zip(f.params) {
        let (mut maybe_state, binding) = map_expression_to_binding(new_state, arg)?;

        let error = maybe_state.add_or_mutate_symbols(&parameter.ident, binding);
        if let Some(e) = error {
            return Err(match e {
                MutateBindingError::InvalidRedeclaration => EvaluatorError::InvalidRedeclaration,
                MutateBindingError::NoIdentifier => EvaluatorError::NoSuchIdentifier,
            });
        };
        new_state = maybe_state;
    }
    let (new_state, result) = evaluate(new_state, *f.body)?;

    Ok((new_state, result))
}

fn evaluate_identifier(
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
        None => Err(EvaluatorError::NoSuchIdentifier),
    }
}

fn evaluate_statement(
    state: EnvironmentState,
    s: Statement,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match s.operation {
        StatementOperator::Break => todo!(),
        StatementOperator::Continue => todo!(),
        StatementOperator::Return => evaluate_return(state, s.expression.as_deref().cloned()),
        StatementOperator::Include => todo!(),
    }
}

fn evaluate_return(
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
    std: &RustBinding,
    args: Vec<Expression>,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match std {
        RustBinding::Print(std_print) => {
            let (new_state, args) = resolve_function_arguments_to_string(state, args)?;
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
                    _ => todo!(),
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
    }
}

fn resolve_function_arguments_to_string(
    state: EnvironmentState,
    args: Vec<Expression>,
) -> Result<(EnvironmentState, Vec<String>), EvaluatorError> {
    let mut new_state = state.clone();
    let mut s_args: Vec<String> = vec![];
    for arg in args {
        let (maybe_state, value) = map_expression_to_value(new_state, arg)?;
        let (maybe_state, s) = map_value_to_string(maybe_state, &value)?;
        s_args.push(s);
        new_state = maybe_state;
    }
    Ok((new_state, s_args))
}

fn evaluate_function_call(
    state: EnvironmentState,
    fc: FunctionCall,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let maybe_std_fn = state.std_lib_symbols.get(&fc.identifier).cloned();

    if let Some(std) = maybe_std_fn {
        let (state, result) = handle_rust_binding_with_args(state, &std, fc.args)?;
        return Ok((state, result));
    }

    if let Some(f) = state.get_local_binding(&fc.identifier) {
        match f {
            EnvironmentBinding::Function(f) => evaluate_function(state, f, fc.args),
            _ => Err(EvaluatorError::InvalidFunctionCall),
        }
    } else {
        Err(EvaluatorError::InvalidFunctionCall)
    }
}

fn add_binding_or_error(
    state: EnvironmentState,
    symbol: &str,
    binding: EnvironmentBinding,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let mut maybe_state = state.clone();
    let error = maybe_state.add_or_mutate_symbols(symbol, binding);
    match error {
        Some(e) => Err(match e {
            MutateBindingError::InvalidRedeclaration => EvaluatorError::InvalidRedeclaration,
            MutateBindingError::NoIdentifier => EvaluatorError::NoSuchIdentifier,
        }),

        None => Ok((maybe_state, Value::Void)),
    }
}
