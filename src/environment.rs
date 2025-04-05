use std::collections::HashMap;

use crate::{
    language::{Function, NumericLiteral, Range, ValueLiteral},
    rust_bindings::{std_rust_print, RustBinding},
};

/// Represents the current Environment State of the program. This is for resolving symbols that a user will reference in their program,
/// including the standard library bindings.
///
/// An EnvironmentState is immutable, so adding or removing symbols from the current state requires
/// the production of a new EnvironmentState.
#[derive(Debug, Clone)]
pub struct EnvironmentState {
    pub local_symbols: HashMap<String, EnvironmentBinding>,
    pub std_lib_symbols: HashMap<String, RustBinding>,
}

#[derive(Debug, Clone)]
pub struct InvalidRedeclaration;

#[derive(Debug, Clone)]
pub struct NoSuchBinding;

/// A binding in the Environment. This represents all possible values a symbol can resolve to.
///
/// In this case, either a value or a function.
///
/// Example:
///
/// `x = 5` produces a binding of Value::ValueLiteral::Int(5)
#[derive(Debug, Clone, PartialEq)]
pub enum EnvironmentBinding {
    Value(Value),
    Function(Function),
    Identifier(String),
    Range(Range),
}

impl EnvironmentBinding {
    pub fn new_string(s: &str) -> EnvironmentBinding {
        EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::String(s.to_string())))
    }
    pub fn new_numeric(n: NumericLiteral) -> EnvironmentBinding {
        EnvironmentBinding::Value(Value::ValueLiteral(ValueLiteral::Numeric(n)))
    }
}

/// The variations an Environment binding value can be.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    ValueLiteral(ValueLiteral),
    Void,
}

impl EnvironmentState {
    pub fn new(local_symbols: HashMap<String, EnvironmentBinding>) -> Self {
        Self {
            local_symbols,
            std_lib_symbols: HashMap::from([(
                "print".to_string(),
                RustBinding::Print(std_rust_print),
            )]),
        }
    }

    pub fn get_local_binding(&self, symbol: &str) -> Option<EnvironmentBinding> {
        self.local_symbols.get(symbol).cloned()
    }
    pub fn add_local_symbols(
        mut self,
        symbol: &str,
        binding: EnvironmentBinding,
    ) -> (EnvironmentState, Option<InvalidRedeclaration>) {
        let already_contained = self.local_symbols.contains_key(symbol);
        if let EnvironmentBinding::Function(_) = binding {
            if already_contained {
                return (self, Some(InvalidRedeclaration));
            }
        }
        self.local_symbols.insert(symbol.to_string(), binding);
        (self, None)
    }
}
