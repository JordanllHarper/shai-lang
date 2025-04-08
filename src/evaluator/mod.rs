pub mod bindings;
pub mod environment;
pub mod evaluation;
pub mod iterate;
pub mod operation;
pub mod util;

use crate::language::*;
use bindings::*;
use environment::*;
use evaluation::*;
use iterate::iterate_array;
use operation::*;
use util::value_to_string;

#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    InvalidRedeclaration,
    NoSuchIdentifier,
    InvalidFunctionCall,
    InvalidEvaluation,
    NotABooleanValue,
    InvalidNumberOfArguments,
    InvalidOperationValue,
    InvalidIterable,
    InvalidForLoopIdentifier,
}

pub fn evaluate(
    state: EnvironmentState,
    ast: Expression,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    match ast {
        Expression::ValueLiteral(v) => Ok((state, Value::ValueLiteral(v))),
        Expression::Identifier(ident) => evaluate_identifier(state, &ident),
        Expression::Statement(s) => evaluate_statement(state, s),
        Expression::MathOperation(_) => todo!(),
        Expression::Evaluation(_) => todo!(),
        Expression::Function(f) => {
            add_binding_or_error(state, &f.ident.clone(), EnvironmentBinding::Function(f))
        }
        Expression::If(if_expression) => evaluate_if(state, if_expression),
        Expression::While(w) => evaluate_while(state, w),
        Expression::For(f) => evaluate_for(state, f),
        Expression::Body(b) => evaluate_body(state, b),
        Expression::Range(_) => todo!(),
        Expression::Assignment(a) => evaluate_assignment(state, a),
        Expression::FunctionCall(f) => evaluate_function_call(state, f),
    }
}

fn evaluate_for(
    state: EnvironmentState,
    f: For,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let mut new_state = state.clone();
    let for_scope = Scope::new(Some(state.current_scope.clone()));
    new_state.current_scope = for_scope;

    let iterable_binding = get_binding_from_expression(&new_state, *f.iterable)?;
    let (mut new_state, result) = get_values_from_binding(new_state, iterable_binding)?;
    match result {
        Value::ValueLiteral(ValueLiteral::Array(a)) => {
            let (maybe_state, _) = iterate_array(new_state, a, f.scoped_variable, f.body)?;
            new_state = maybe_state;
        }
        Value::ValueLiteral(ValueLiteral::Dictionary(d)) => {
            todo!()
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
        let (maybe_state, b) = match w.condition {
            Some(ref v) => evaluate_evaluation(new_state, *v.clone())?,
            None => (new_state, true),
        };

        if b {
            let (maybe_state, value) = evaluate_body(maybe_state, w.body.clone())?;
            new_state = maybe_state;
        } else {
            return Ok((state, Value::Void));
        }
    }
}

fn evaluate_if(
    state: EnvironmentState,
    if_expression: crate::language::If,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let (state, is_true_eval) = evaluate_evaluation(state, *if_expression.evaluation)?;
    match (is_true_eval, if_expression.on_false_evaluation) {
        (true, _) => evaluate(state.clone(), *if_expression.on_true_evaluation),
        (false, None) => Ok((state, Value::Void)),
        (false, Some(e)) => evaluate(state.clone(), *e),
    }
}

fn evaluate_evaluation(
    state: EnvironmentState,
    evaluation: Expression,
) -> Result<(EnvironmentState, bool), EvaluatorError> {
    if let Expression::Evaluation(e) = evaluation {
        let lhs_binding = match get_binding_from_expression(&state, *e.lhs) {
            Ok(binding) => binding,
            Err(e) => return Err(e),
        };

        let rhs_binding = if let Some(expr) = e.rhs {
            Some(get_binding_from_expression(&state, *expr)?)
        } else {
            None
        };

        evaluate_bindings(state, lhs_binding, rhs_binding, e.evaluation_op)
    } else {
        Err(EvaluatorError::InvalidEvaluation)
    }
}

fn get_values_from_binding(
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
        EnvironmentBinding::Range(_) => todo!(),
    }
}

fn get_binding_from_expression(
    state: &EnvironmentState,
    expr: Expression,
) -> Result<EnvironmentBinding, EvaluatorError> {
    let binding = match expr {
        Expression::ValueLiteral(v) => EnvironmentBinding::Value(Value::ValueLiteral(v)),
        Expression::Identifier(i) => get_identifier_binding(state, &i)?,
        Expression::Evaluation(_) => todo!(),
        Expression::FunctionCall(_) => todo!(),
        Expression::Body(e) => todo!(),
        Expression::Range(r) => todo!(),
        _ => return Err(EvaluatorError::InvalidEvaluation),
    };
    Ok(binding)
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
                EnvironmentBinding::Range(_) => todo!(),
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
        Expression::MathOperation(math_op) => evaluate_operation(state, math_op),
        Expression::Evaluation(eval) => {
            let lhs = get_binding_from_expression(&state, *eval.lhs)?;
            let rhs = if let Some(s) = eval.rhs {
                Some(get_binding_from_expression(&state, *s)?)
            } else {
                None
            };

            let (state, eval) = evaluate_bindings(state, lhs, rhs, eval.evaluation_op)?;
            Ok((state, EnvironmentBinding::new_bool(eval)))
        }
        Expression::Function(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::While(_) => todo!(),
        Expression::For(_) => todo!(),
        Expression::Body(_) => todo!(),
        Expression::Range(_) => todo!(),
        Expression::Assignment(_) => todo!(),
        Expression::FunctionCall(_) => todo!(),
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
        let binding = get_binding_from_expression(&state, arg)?;

        let error = new_state.add_or_mutate_symbols(&parameter.ident, binding);
        if let Some(e) = error {
            return Err(match e {
                MutateBindingError::InvalidRedeclaration => EvaluatorError::InvalidRedeclaration,
                MutateBindingError::NoIdentifier => EvaluatorError::NoSuchIdentifier,
            });
        }
    }
    let (_, result) = evaluate(new_state, *f.body)?;

    Ok((state, result))
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
            EnvironmentBinding::Range(_) => todo!(),
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
        StatementOperator::Return => todo!(),
        StatementOperator::Include => todo!(),
    }
}

fn get_string_from_binding(
    state: EnvironmentState,
    binding: &EnvironmentBinding,
) -> Result<(EnvironmentState, String), EvaluatorError> {
    let new_state = state.clone();
    match binding {
        EnvironmentBinding::Value(v) => value_to_string(state, v),
        EnvironmentBinding::Function(f) => todo!(),
        EnvironmentBinding::Identifier(i) => match state.get_local_binding(i) {
            Some(binding) => get_string_from_binding(new_state, &binding),
            None => Err(EvaluatorError::NoSuchIdentifier),
        },
        EnvironmentBinding::Range(_) => todo!(),
    }
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
    }
}

fn resolve_function_arguments_to_string(
    state: EnvironmentState,
    args: Vec<Expression>,
) -> Result<(EnvironmentState, Vec<String>), EvaluatorError> {
    let mut new_state = state.clone();
    let mut s_args: Vec<String> = vec![];
    for arg in args {
        let s = match arg {
            Expression::ValueLiteral(v) => v.to_string(),
            Expression::Identifier(i) => match new_state.get_local_binding(&i) {
                Some(binding) => {
                    let (maybe_state, s) = get_string_from_binding(new_state, &binding)?;
                    new_state = maybe_state;
                    s
                }
                None => return Err(EvaluatorError::NoSuchIdentifier),
            },
            Expression::Statement(_) => todo!(),
            Expression::MathOperation(_) => todo!(),
            Expression::Evaluation(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::While(_) => todo!(),
            Expression::For(_) => todo!(),
            Expression::Body(b) => {
                let (maybe_state, s) = get_body_string_value(new_state, b)?;
                new_state = maybe_state;
                s
            }
            Expression::Range(_) => todo!(),
            Expression::Assignment(_) => todo!(),
            Expression::FunctionCall(_) => todo!(),
        };
        s_args.push(s);
    }
    Ok((new_state, s_args))
}

fn get_body_string_value(
    state: EnvironmentState,
    b: Vec<Expression>,
) -> Result<(EnvironmentState, String), EvaluatorError> {
    let mut new_state = state.clone();
    let mut str = String::new();
    for expr in b {
        let (state, value) = evaluate(new_state, expr)?;

        str = match value {
            Value::ValueLiteral(v) => v.to_string(),
            Value::Void => "".to_string(),
        };
        new_state = state;
    }

    Ok((new_state, str))
}

fn evaluate_function_call(
    state: EnvironmentState,
    fc: FunctionCall,
) -> Result<(EnvironmentState, Value), EvaluatorError> {
    let maybe_std_fn = state.std_lib_symbols.get(&fc.identifier).cloned();

    if let Some(std) = maybe_std_fn {
        let (state, result) = handle_rust_binding_with_args(state, &std, fc.args)?;
        return Ok((state, Value::Void));
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
