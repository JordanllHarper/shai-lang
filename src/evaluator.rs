use crate::{
    environment::{EnvironmentBinding, EnvironmentState, Value},
    language::{
        Expression, Function, FunctionArguments, FunctionCall, Statement, StatementOperator,
    },
    std_lib::RustBinding,
};

#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    InvalidRedeclaration,
    NoSuchIdentifier,
    InvalidFunctionCall,
    InvalidFunctionCallArgument,
    EmptyBody,
}

pub fn evaluate(state: EnvironmentState, ast: Expression) -> EvaluatorState {
    match ast {
        Expression::ValueLiteral(v) => (state, None, Value::ValueLiteral(v)),
        Expression::Identifier(ident) => evaluate_identifier(state, &ident),
        Expression::MultipleValues(_) => (state, None, todo!()),
        Expression::Statement(s) => evaluate_statement(state, s),
        Expression::MathOperation(_) => (state, None, todo!()),
        Expression::Evaluation(_) => todo!(),
        Expression::Function(f) => {
            add_binding_or_error(state, &f.ident.clone(), EnvironmentBinding::Function(f))
        }
        Expression::If(_) => todo!(),
        Expression::While(_) => todo!(),
        Expression::For(_) => todo!(),
        Expression::Body(_) => todo!(),
        Expression::Range(_) => todo!(),
        Expression::Assignment(a) => evaluate_assignment(state, a),
        Expression::FunctionCall(f) => evaluate_function_call(state, f),
    }
}

fn get_identifier_binding(
    state: &EnvironmentState,
    symbol: &str,
) -> Result<EnvironmentBinding, EvaluatorError> {
    state
        .local_symbols
        .get(symbol)
        .map_or(Err(EvaluatorError::NoSuchIdentifier), |binding| {
            Ok(binding.clone())
        })
}

fn evaluate_assignment_expression(
    state: &EnvironmentState,
    expr: Expression,
) -> Result<EnvironmentBinding, EvaluatorError> {
    match expr {
        Expression::ValueLiteral(v) => Ok(EnvironmentBinding::Value(Value::ValueLiteral(v))),
        Expression::Identifier(i) => {
            let binding = get_identifier_binding(&state, &i)?;
            Ok(binding)
        }
        Expression::MultipleValues(_) => todo!(),
        Expression::Statement(_) => todo!(),
        Expression::MathOperation(_) => todo!(),
        Expression::Evaluation(_) => todo!(),
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

fn evaluate_assignment(state: EnvironmentState, a: crate::language::Assignment) -> EvaluatorState {
    match evaluate_assignment_expression(&state, *a.rhs) {
        Ok(binding) => add_binding_or_error(state, &a.identifier, binding),
        Err(e) => (state, Some(e), Value::Void),
    }
}

fn evaluate_function(state: EnvironmentState, f: &Function) -> EvaluatorState {
    todo!()
}

fn evaluate_value(state: EnvironmentState, v: &Value) -> EvaluatorState {
    todo!();
}

type EvaluatorState = (EnvironmentState, Option<EvaluatorError>, Value);
fn evaluate_identifier(state: EnvironmentState, ident: &str) -> EvaluatorState {
    let maybe_std_fn = state.std_lib_symbols.get(ident).cloned();
    if let Some(std) = maybe_std_fn {
        let (state, error) = handle_rust_binding_with_args(state, &std, vec![]);
        return match error {
            Some(e) => (state, Some(e), Value::Void),
            None => (state, None, Value::Void),
        };
    }

    let symbol = state.get_local_binding(ident);

    match symbol {
        Some(v) => match v {
            EnvironmentBinding::Value(v) => evaluate_value(state, &v),
            EnvironmentBinding::Function(f) => evaluate_function(state, &f),
            EnvironmentBinding::Identifier(i) => evaluate_identifier(state, &i),
            EnvironmentBinding::Range(_) => todo!(),
        },
        None => (state, Some(EvaluatorError::NoSuchIdentifier), Value::Void),
    }
}

fn evaluate_statement(state: EnvironmentState, s: Statement) -> EvaluatorState {
    match s.operation {
        StatementOperator::Break => todo!(),
        StatementOperator::Continue => todo!(),
        StatementOperator::Return => todo!(),
        StatementOperator::Include => todo!(),
    }
}

fn resolve_binding_to_string(
    state: &EnvironmentState,
    binding: &EnvironmentBinding,
) -> Result<String, EvaluatorError> {
    match binding {
        EnvironmentBinding::Value(v) => match v {
            Value::ValueLiteral(vl) => Ok(vl.to_string()),
            Value::Void => Ok("".to_string()),
        },
        EnvironmentBinding::Function(f) => todo!(),
        EnvironmentBinding::Identifier(i) => match state.local_symbols.get(i) {
            Some(binding) => resolve_binding_to_string(state, binding),
            None => Err(EvaluatorError::NoSuchIdentifier),
        },
        EnvironmentBinding::Range(_) => todo!(),
    }
}

pub fn handle_rust_binding_with_args(
    state: EnvironmentState,
    std: &RustBinding,
    args: FunctionArguments,
) -> (EnvironmentState, Option<EvaluatorError>) {
    match std {
        RustBinding::Print(std_print) => {
            let (state, result) = resolve_function_arguments_to_string(state, args);
            match result {
                Ok(v) => {
                    std_print(v);
                    (state, None)
                }
                Err(v) => (state, Some(v)),
            }
        }
    }
}

fn resolve_function_arguments_to_string(
    state: EnvironmentState,
    args: FunctionArguments,
) -> (EnvironmentState, Result<Vec<String>, EvaluatorError>) {
    let mut new_state = state.clone();
    let mut s_args: Vec<String> = vec![];
    for arg in args {
        let s = match arg {
            Expression::ValueLiteral(v) => v.to_string(),
            Expression::Identifier(i) => match new_state.local_symbols.get(&i) {
                Some(binding) => match resolve_binding_to_string(&new_state, binding) {
                    Ok(v) => v,
                    Err(e) => return (state, Err(e)),
                },
                None => return (state, Err(EvaluatorError::NoSuchIdentifier)),
            },
            Expression::MultipleValues(_) => todo!(),
            Expression::Statement(_) => todo!(),
            Expression::MathOperation(_) => todo!(),
            Expression::Evaluation(_) => todo!(),
            Expression::Function(_) => todo!(),
            Expression::If(_) => todo!(),
            Expression::While(_) => todo!(),
            Expression::For(_) => todo!(),
            Expression::Body(b) => {
                let (state, _, value) = resolve_body_string_value(new_state, b);
                if let Some(s) = value {
                    new_state = state;
                    s
                } else {
                    return (state, Err(EvaluatorError::NoSuchIdentifier));
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

fn resolve_body_string_value(
    state: EnvironmentState,
    b: Vec<Expression>,
) -> (EnvironmentState, Option<EvaluatorError>, Option<String>) {
    let first = b.first().cloned();
    if let Some(e) = first {
        let (state, error, return_value) = evaluate(state, e);
        if error.is_some() {
            return (state, error, None);
        }
        let s = match return_value {
            Value::ValueLiteral(v) => v.to_string(),
            Value::Void => "".to_string(),
        };

        (state, None, Some(s))
    } else {
        (state, Some(EvaluatorError::EmptyBody), None)
    }
}

fn evaluate_function_call(state: EnvironmentState, fc: FunctionCall) -> EvaluatorState {
    let maybe_std_fn = state.std_lib_symbols.get(&fc.identifier).cloned();

    if let Some(std) = maybe_std_fn {
        let (state, error) = handle_rust_binding_with_args(state, &std, fc.args);

        return (state, error, Value::Void);
    }

    if let Some(f) = state.local_symbols.get(&fc.identifier) {
        match f {
            EnvironmentBinding::Value(v) => (
                state,
                Some(EvaluatorError::InvalidFunctionCall),
                Value::Void,
            ),
            EnvironmentBinding::Function(f) => todo!(),
            EnvironmentBinding::Identifier(_) => todo!(),
            EnvironmentBinding::Range(_) => todo!(),
        }
    } else {
        (
            state,
            Some(EvaluatorError::InvalidFunctionCall),
            Value::Void,
        )
    }
}

fn add_binding_or_error(
    state: EnvironmentState,
    symbol: &str,
    binding: EnvironmentBinding,
) -> EvaluatorState {
    let (state, binding) = state.add_local_symbols(symbol, binding);
    match binding {
        Some(e) => (
            state,
            Some(EvaluatorError::InvalidRedeclaration),
            Value::Void,
        ),
        None => (state, None, Value::Void),
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::{
        environment::{EnvironmentBinding, EnvironmentState, Value},
        evaluator::evaluate,
        language::{Expression, ValueLiteral},
    };

    #[test]
    fn assignment() {
        let ast = Expression::new_assignment("x", Expression::new_int(5), None, None, false);
        let state = EnvironmentState::new(HashMap::new());
        let (state, error, return_value) = evaluate(state, ast);

        assert_eq!(
            state.local_symbols.get("x"),
            Some(&EnvironmentBinding::Value(Value::ValueLiteral(
                ValueLiteral::Int(5)
            )))
        );
        assert_eq!(error, None);
    }
}
