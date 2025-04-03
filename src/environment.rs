use std::collections::HashMap;

use crate::{
    language::{Expression, Function, FunctionArguments, FunctionCall, Range, ValueLiteral},
    std_lib::{std_print, Print, Stdlib},
};

/// Represents the current Environment State of the program. This is for resolving symbols that a user will reference in their program,
/// including the standard library bindings.
///
/// An EnvironmentState is immutable, so adding or removing symbols from the current state requires
/// the production of a new EnvironmentState.
pub struct EnvironmentState {
    pub local_symbols: HashMap<EnvironmentSymbol, EnvironmentBinding>,
    pub std_lib_symbols: HashMap<EnvironmentSymbol, Stdlib>,
}

#[derive(Debug, Clone)]
pub struct InvalidRedeclaration;

#[derive(Debug, Clone)]
pub struct NoSuchBinding;

/// A simple type alias for handling symbols.
///
/// To give context, a symbol can be a variable or function.
///
/// Example:
///
/// `x = 5` produces a symbol "x"
/// `square (num) = num * num ` produces a symbol "square".
type EnvironmentSymbol = String;

/// A binding in the Environment. This represents all possible values a symbol can resolve to.
///
/// In this case, either a value or a function.
///
/// Example:
///
/// `x = 5` produces a binding of Value::ValueLiteral::Int(5)
#[derive(Debug, Clone)]
pub enum EnvironmentBinding {
    Value(Value),
    Function(Function),
}

/// The variations an Environment binding value can be.
///
/// Note that ranges are also considered first class in the language.
#[derive(Debug, Clone)]
pub enum Value {
    ValueLiteral(ValueLiteral),
    Identifier(String),
    Range(Range),
}

impl EnvironmentState {
    pub fn new(local_symbols: HashMap<EnvironmentSymbol, EnvironmentBinding>) -> Self {
        let std_lib = HashMap::from([("print".to_string(), Stdlib::Print(std_print))]);

        Self {
            local_symbols,
            std_lib_symbols: std_lib,
        }
    }

    pub fn get_local_binding(&self, symbol: &str) -> Result<EnvironmentBinding, NoSuchBinding> {
        let maybe_symbol = self.local_symbols.get(symbol);
        if let Some(v) = maybe_symbol {
            Ok(v.clone())
        } else {
            Err(NoSuchBinding)
        }
    }

    pub fn add_binding(
        mut self,
        symbol: EnvironmentSymbol,
        binding: EnvironmentBinding,
    ) -> Result<EnvironmentState, InvalidRedeclaration> {
        let already_contained = self.local_symbols.contains_key(&symbol);
        if let EnvironmentBinding::Function(_) = binding {
            if already_contained {
                return Err(InvalidRedeclaration);
            }
        }
        self.local_symbols.insert(symbol, binding);
        Ok(EnvironmentState::new(self.local_symbols))
    }
}

///
///
/// Evaluator
///
///

pub enum EnvironmentError {
    InvalidRedeclaration,
    NoSuchIdentifier,
}

fn evaluate_function(state: EnvironmentState, f: Function) -> EvaluatorState {
    todo!()
}

fn evaluate_value(state: EnvironmentState, v: Value) -> EvaluatorState {
    todo!();
}

fn handle_std_lib_arg(std: &Stdlib, arg: FunctionArguments) {
    match std {
        Stdlib::Print(p) => p(match arg.first() {
            Some(Expression::ValueLiteral(ValueLiteral::String(s))) => s.to_string(),
            _ => todo!(),
        }),
    }
}

fn handle_std_lib_no_arg(std: &Stdlib) {
    match std {
        Stdlib::Print(p) => p(String::new()),
    }
}

type EvaluatorState = (EnvironmentState, Option<EnvironmentError>);
fn evaluate_identifier(state: EnvironmentState, ident: &str) -> EvaluatorState {
    //
    //
    // print
    //
    // should output newline
    let maybe_std_fn = state.std_lib_symbols.get(ident);
    if let Some(std) = maybe_std_fn {
        handle_std_lib_no_arg(std);
        return (state, None);
    }

    match state.get_local_binding(ident) {
        Ok(v) => match v {
            EnvironmentBinding::Value(v) => evaluate_value(state, v),
            EnvironmentBinding::Function(f) => evaluate_function(state, f),
        },
        Err(_) => (state, Some(EnvironmentError::NoSuchIdentifier)),
    }
}

pub fn evaluate(state: EnvironmentState, ast: Expression) -> EvaluatorState {
    match ast {
        Expression::ValueLiteral(_) => (state, None),
        Expression::Identifier(ident) => evaluate_identifier(state, &ident),
        Expression::MultipleValues(_) => todo!(),
        Expression::Statement(_) => todo!(),
        Expression::MathOperation(_) => (state, None),
        Expression::Evaluation(_) => todo!(),
        Expression::Function(_) => todo!(),
        Expression::If(_) => todo!(),
        Expression::While(_) => todo!(),
        Expression::For(_) => todo!(),
        Expression::Body(_) => todo!(),
        Expression::Range(_) => todo!(),
        Expression::Assignment(_) => todo!(),
        Expression::FunctionCall(f) => evaluate_function_call(state, f),
    }
}

fn evaluate_function_call(state: EnvironmentState, f: FunctionCall) -> EvaluatorState {
    let maybe_std_fn = state.std_lib_symbols.get(&f.identifier);

    if let Some(std) = maybe_std_fn {
        handle_std_lib_arg(std, f.args);
        return (state, None);
    }
    todo!()
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;

    use crate::language::{Expression, ValueLiteral};

    use super::{evaluate, EnvironmentState};

    #[test]
    fn print() {
        let state = EnvironmentState::new(HashMap::new());
        let ast = Expression::new_identifier("print");
        let (state, maybe_error) = evaluate(state, ast);
    }

    #[test]
    fn print_hello_world() {
        let state = EnvironmentState::new(HashMap::new());
        let ast = Expression::new_function_call(
            "print",
            vec![Expression::ValueLiteral(ValueLiteral::String(
                "Hello, World".to_string(),
            ))],
        );
        let (state, maybe_error) = evaluate(state, ast);
    }
}
