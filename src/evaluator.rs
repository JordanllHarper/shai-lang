use crate::{
    environment::{EnvironmentBinding, EnvironmentState, Value},
    language::{
        Expression, Function, FunctionArguments, FunctionCall, Statement, StatementOperator,
    },
    std_lib::Stdlib,
};

#[derive(Debug, PartialEq)]
pub enum EvaluatorError {
    InvalidRedeclaration,
    NoSuchIdentifier,
    InvalidFunctionCall,
    InvalidFunctionCallArgument,
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
    let maybe_std_fn = state.std_lib_symbols.get(ident);
    if let Some(std) = maybe_std_fn {
        let result = handle_std_lib_arg(&state, std, vec![]);
        return match result {
            Ok(_) => (state, None, Value::Void),
            Err(e) => (
                state,
                Some(EvaluatorError::InvalidFunctionCall),
                Value::Void,
            ),
        };
    }

    let symbol = state.get_local_binding(ident);

    match symbol {
        Some(v) => match v {
            EnvironmentBinding::Value(v) => evaluate_value(state, &v),
            EnvironmentBinding::Function(f) => evaluate_function(state, &f),
            EnvironmentBinding::Identifier(i) => evaluate_identifier(state, &i),
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

#[derive(Debug)]
pub enum StringArgumentsError {
    InvalidArgument,
    InvalidIdentifier,
}

fn resolve_binding_to_string(
    state: &EnvironmentState,
    binding: &EnvironmentBinding,
) -> Result<String, StringArgumentsError> {
    match binding {
        EnvironmentBinding::Value(v) => match v {
            Value::ValueLiteral(vl) => Ok(vl.to_string()),
            Value::Range(_) => todo!(),
            Value::Void => Ok("".to_string()),
        },
        EnvironmentBinding::Function(f) => todo!(),
        EnvironmentBinding::Identifier(i) => match state.local_symbols.get(i) {
            Some(binding) => resolve_binding_to_string(state, binding),
            None => Err(StringArgumentsError::InvalidIdentifier),
        },
    }
}

pub fn handle_std_lib_arg(
    state: &EnvironmentState,
    std: &Stdlib,
    args: FunctionArguments,
) -> Result<(), StringArgumentsError> {
    match std {
        Stdlib::Print(std_print) => {
            let s = resolve_function_arguments_to_string(state, args)?;
            std_print(s);
            Ok(())
        }
    }
}

fn resolve_function_arguments_to_string(
    state: &EnvironmentState,
    args: FunctionArguments,
) -> Result<Vec<String>, StringArgumentsError> {
    let mut s_args: Vec<String> = vec![];
    for arg in args {
        let s = match arg {
            Expression::ValueLiteral(v) => v.to_string(),
            Expression::Identifier(i) => match state.local_symbols.get(&i) {
                Some(binding) => resolve_binding_to_string(state, binding)?,
                None => return Err(StringArgumentsError::InvalidIdentifier),
            },
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
        };
        s_args.push(s);
    }
    Ok(s_args)
}

fn evaluate_function_call(state: EnvironmentState, fc: FunctionCall) -> EvaluatorState {
    let maybe_std_fn = state.std_lib_symbols.get(&fc.identifier);

    if let Some(std) = maybe_std_fn {
        let result = handle_std_lib_arg(&state, std, fc.args).map_err(|e| match e {
            StringArgumentsError::InvalidArgument => EvaluatorError::InvalidFunctionCallArgument,
            StringArgumentsError::InvalidIdentifier => EvaluatorError::NoSuchIdentifier,
        });

        return (
            state,
            match result {
                Ok(_) => None,
                Err(e) => Some(e),
            },
            Value::Void,
        );
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
