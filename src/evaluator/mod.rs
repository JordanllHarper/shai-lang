pub mod bindings;
pub mod environment;
pub mod evaluation;
pub mod operation;
pub mod util;

use crate::language::*;
use bindings::*;
use environment::*;
use evaluation::*;
use operation::*;
use util::value_literal_to_string;
//
//
// TODO: Think about scopes

#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    InvalidRedeclaration,
    NoSuchIdentifier,
    InvalidFunctionCall,
    InvalidFunctionCallArgument,
    EmptyBody,
    InvalidEvaluation,
    NotABooleanValue,
    InvalidSubtract,
    InvalidAddition,
    InvalidDivide,
    InvalidMultiplication,
    NoSuchValue,
    InvalidNumberOfArguments,
    InvalidIdentifier,
    InvalidOperationValue,
    InvalidIterable,
}

pub fn evaluate(
    state: EnvironmentState,
    ast: Expression,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    match ast {
        Expression::ValueLiteral(v) => (state, Ok(Value::ValueLiteral(v))),
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
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let mut new_state = state.clone();
    let for_scope = Scope::new(Some(state.current_scope.clone()));
    new_state.current_scope = for_scope;

    let iterable_binding = match get_binding_from_expression(&new_state, *f.iterable) {
        Ok(b) => b,
        Err(e) => return (state, Err(e)),
    };
    let (new_state, result) = get_values_from_binding(new_state, iterable_binding);
    match result {
        Ok(Value::ValueLiteral(vl)) => match vl {
            ValueLiteral::CharacterBased(_) => todo!(),
            ValueLiteral::Numeric(_) => todo!(),
            ValueLiteral::Bool(_) => todo!(),
            ValueLiteral::Array(_) => todo!(),
            ValueLiteral::Dictionary(_) => todo!(),
            ValueLiteral::Function => todo!(),
        },
        Ok(Value::Void) => return (new_state, Err(EvaluatorError::InvalidIterable)),
        Err(e) => todo!(),
    }

    todo!()
}

fn evaluate_while(
    state: EnvironmentState,
    w: While,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let mut new_state = state.clone();
    let while_scope = Scope::new(Some(state.current_scope.clone()));
    new_state.current_scope = while_scope;

    loop {
        let (maybe_state, result) = match w.condition {
            Some(ref v) => evaluate_evaluation(new_state, *v.clone()),
            None => (new_state, Ok(true)),
        };

        match result {
            Ok(b) => {
                if b {
                    let (maybe_state, result) = evaluate_body(maybe_state, w.body.clone());
                    match result {
                        Ok(v) => new_state = maybe_state,
                        Err(e) => return (state, Err(e)),
                    }
                } else {
                    return (state, Ok(Value::Void));
                }
            }
            Err(e) => return (state, Err(e)),
        }
    }
}

fn evaluate_if(
    state: EnvironmentState,
    if_expression: crate::language::If,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let (state, is_true_eval) = evaluate_evaluation(state, *if_expression.evaluation);
    match is_true_eval {
        Ok(eval) => match (eval, if_expression.on_false_evaluation) {
            (true, _) => evaluate(state.clone(), *if_expression.on_true_evaluation),
            (false, None) => (state, Ok(Value::Void)),
            (false, Some(e)) => evaluate(state.clone(), *e),
        },
        Err(e) => (state, Err(e)),
    }
}

fn evaluate_evaluation(
    state: EnvironmentState,
    evaluation: Expression,
) -> (EnvironmentState, Result<bool, EvaluatorError>) {
    if let Expression::Evaluation(e) = evaluation {
        let lhs_binding = match get_binding_from_expression(&state, *e.lhs) {
            Ok(binding) => binding,
            Err(e) => return (state, Err(e)),
        };

        let rhs_binding = if let Some(expr) = e.rhs {
            let result = get_binding_from_expression(&state, *expr);
            match result {
                Ok(v) => Some(v),
                Err(e) => return (state, Err(e)),
            }
        } else {
            None
        };

        evaluate_bindings(state, lhs_binding, rhs_binding, e.evaluation_op)
    } else {
        (state, Err(EvaluatorError::InvalidEvaluation))
    }
}

fn get_values_from_binding(
    state: EnvironmentState,
    binding: EnvironmentBinding,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    match binding {
        EnvironmentBinding::Value(v) => (state, Ok(v)),
        EnvironmentBinding::Function(f) => evaluate_function(state, f, vec![]),
        EnvironmentBinding::Identifier(i) => {
            let get_identifier_binding_recursively = get_identifier_binding_recursively(&state, &i);
            (state, get_identifier_binding_recursively)
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
        Expression::Identifier(i) => {
            let result = get_identifier_binding(state, &i);
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
    Ok(binding)
}

fn evaluate_body(
    state: EnvironmentState,
    body: Body,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let mut next_state = state.clone();
    let mut next_value = Value::Void;
    for expr in body {
        let (maybe_state, result) = evaluate(next_state, expr);
        match result {
            Ok(v) => {
                next_state = maybe_state;
                next_value = v;
            }
            Err(e) => {
                return (state, Err(e));
            }
        }
    }
    (next_state, Ok(next_value))
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
) -> (EnvironmentState, Result<EnvironmentBinding, EvaluatorError>) {
    match expr {
        Expression::ValueLiteral(v) => {
            (state, Ok(EnvironmentBinding::Value(Value::ValueLiteral(v))))
        }
        Expression::Identifier(i) => {
            let get_identifier_binding = get_identifier_binding(&state, &i);
            (state, get_identifier_binding)
        }
        Expression::Statement(_) => todo!(),
        Expression::MathOperation(math_op) => evaluate_operation(state, math_op),
        Expression::Evaluation(eval) => {
            let lhs = match get_binding_from_expression(&state, *eval.lhs) {
                Ok(v) => v,
                Err(e) => return (state, Err(e)),
            };
            let rhs = match eval.rhs {
                Some(s) => match get_binding_from_expression(&state, *s) {
                    Ok(v) => Some(v),
                    Err(error) => return (state, Err(error)),
                },
                None => None,
            };

            let (state, result) = evaluate_bindings(state, lhs, rhs, eval.evaluation_op);
            let eval_result = match result {
                Ok(b) => b,
                Err(e) => return (state, Err(e)),
            };
            (state, Ok(EnvironmentBinding::new_bool(eval_result)))
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
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    match evaluate_assignment_expression(state, *a.rhs) {
        (new_state, Ok(binding)) => add_binding_or_error(new_state, &a.identifier, binding),
        (new_state, Err(e)) => (new_state, Err(e)),
    }
}

fn evaluate_function(
    state: EnvironmentState,
    f: Function,
    args: Vec<Expression>,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    if args.len() != f.params.len() {
        return (state, Err(EvaluatorError::InvalidNumberOfArguments));
    }

    let mut new_state = state.clone();

    for (arg, parameter) in args.into_iter().zip(f.params) {
        let expr = get_binding_from_expression(&state, arg);
        match expr {
            Ok(b) => {
                let error = new_state.add_local_symbols(&parameter.ident, b);
                if let Some(e) = error {
                    return (
                        state,
                        Err(match e {
                            MutateBindingError::InvalidRedeclaration => {
                                EvaluatorError::InvalidRedeclaration
                            }
                            MutateBindingError::NoIdentifier => EvaluatorError::NoSuchIdentifier,
                        }),
                    );
                }
            }
            Err(e) => return (state, Err(e)),
        }
    }
    let (_, result) = evaluate(new_state, *f.body);

    (state, result)
}

fn evaluate_identifier(
    state: EnvironmentState,
    ident: &str,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let maybe_rust_binding = state.std_lib_symbols.get(ident).cloned();
    if let Some(std) = maybe_rust_binding {
        return handle_rust_binding_with_args(state, &std, vec![]);
    }

    let symbol = state.get_local_binding(ident);

    match symbol {
        Some(binding) => match binding {
            EnvironmentBinding::Value(v) => (state, Ok(v)),
            EnvironmentBinding::Identifier(i) => evaluate_identifier(state, &i),
            EnvironmentBinding::Function(f) => evaluate_function(state, f, vec![]),
            EnvironmentBinding::Range(_) => todo!(),
        },
        None => (state, Err(EvaluatorError::NoSuchIdentifier)),
    }
}

fn evaluate_statement(
    state: EnvironmentState,
    s: Statement,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
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
) -> (EnvironmentState, Result<String, EvaluatorError>) {
    let new_state = state.clone();
    match binding {
        EnvironmentBinding::Value(v) => match v {
            Value::ValueLiteral(vl) => value_literal_to_string(new_state, vl),
            Value::Void => (new_state, Ok(String::new())),
        },
        EnvironmentBinding::Function(f) => todo!(),
        EnvironmentBinding::Identifier(i) => match state.get_local_binding(i) {
            Some(binding) => get_string_from_binding(new_state, &binding),
            None => (state, Err(EvaluatorError::NoSuchIdentifier)),
        },
        EnvironmentBinding::Range(_) => todo!(),
    }
}

pub fn handle_rust_binding_with_args(
    state: EnvironmentState,
    std: &RustBinding,
    args: Vec<Expression>,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    match std {
        RustBinding::Print(std_print) => {
            let result = resolve_function_arguments_to_string(state, args);
            match result {
                (state, Ok(v)) => {
                    std_print(v);
                    (state, Ok(Value::Void))
                }
                (state, Err(v)) => (state, Err(v)),
            }
        }
    }
}

fn resolve_function_arguments_to_string(
    state: EnvironmentState,
    args: Vec<Expression>,
) -> (EnvironmentState, Result<Vec<String>, EvaluatorError>) {
    let mut new_state = state.clone();
    let mut s_args: Vec<String> = vec![];
    for arg in args {
        let s = match arg {
            Expression::ValueLiteral(v) => v.to_string(),
            Expression::Identifier(i) => match new_state.get_local_binding(&i) {
                Some(binding) => match get_string_from_binding(new_state, &binding) {
                    (maybe_state, Ok(v)) => {
                        new_state = maybe_state;
                        v
                    }
                    (_, Err(e)) => return (state, Err(e)),
                },
                None => return (state, Err(EvaluatorError::NoSuchIdentifier)),
            },
            Expression::Statement(_) => todo!(),
            Expression::MathOperation(_) => todo!(),
            Expression::Evaluation(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::While(_) => todo!(),
            Expression::For(_) => todo!(),
            Expression::Body(b) => {
                let result = get_body_string_value(new_state, b);
                match result {
                    (maybe_state, Ok(s)) => {
                        new_state = maybe_state;
                        s
                    }
                    (state, Err(e)) => {
                        return (state, Err(e));
                    }
                }
            }
            Expression::Range(_) => todo!(),
            Expression::Assignment(_) => todo!(),
            Expression::FunctionCall(_) => todo!(),
        };
        s_args.push(s);
    }
    (new_state, Ok(s_args))
}

fn get_body_string_value(
    state: EnvironmentState,
    b: Vec<Expression>,
) -> (EnvironmentState, Result<String, EvaluatorError>) {
    let mut new_state = state.clone();
    let mut s = String::new();
    for expr in b {
        match evaluate(new_state, expr) {
            (st, Ok(v)) => {
                s = match v {
                    Value::ValueLiteral(v) => v.to_string(),
                    Value::Void => "".to_string(),
                };
                new_state = st
            }
            (state, Err(e)) => return (state, Err(e)),
        }
    }

    (new_state, Ok(s))
}

fn evaluate_function_call(
    state: EnvironmentState,
    fc: FunctionCall,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let maybe_std_fn = state.std_lib_symbols.get(&fc.identifier).cloned();

    if let Some(std) = maybe_std_fn {
        let (state, result) = handle_rust_binding_with_args(state, &std, fc.args);
        match result {
            Ok(_) => (),
            Err(e) => {
                return (state, Err(e));
            }
        }
        return (state, Ok(Value::Void));
    }

    if let Some(f) = state.get_local_binding(&fc.identifier) {
        match f {
            EnvironmentBinding::Function(f) => evaluate_function(state, f, fc.args),
            _ => (state, Err(EvaluatorError::InvalidFunctionCall)),
        }
    } else {
        (state, Err(EvaluatorError::InvalidFunctionCall))
    }
}

fn add_binding_or_error(
    state: EnvironmentState,
    symbol: &str,
    binding: EnvironmentBinding,
) -> (EnvironmentState, Result<Value, EvaluatorError>) {
    let mut maybe_state = state.clone();
    let error = maybe_state.add_local_symbols(symbol, binding);
    match error {
        Some(e) => (
            state,
            Err(match e {
                MutateBindingError::InvalidRedeclaration => EvaluatorError::InvalidRedeclaration,
                MutateBindingError::NoIdentifier => EvaluatorError::NoSuchIdentifier,
            }),
        ),
        None => (maybe_state, Ok(Value::Void)),
    }
}
