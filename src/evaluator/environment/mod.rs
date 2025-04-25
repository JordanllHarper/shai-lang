mod environment_impl;
use std::collections::HashMap;

use environment_impl::{get_local_binding_recursive, mutate_local_binding_recursive};

use crate::language::*;

use super::{std_rust_print, EvaluatorError, RustBinding};

/// Represents the current Environment State of the program.
///
/// This is for resolving symbols that a user will reference in their program, including the standard library bindings.
#[derive(Debug, Clone)]
pub struct EnvironmentState {
    pub current_scope: Scope,
    pub std_lib_symbols: HashMap<String, RustBinding>,
}

impl EnvironmentState {
    pub fn new(local_symbols: HashMap<String, EnvironmentBinding>) -> Self {
        Self {
            current_scope: Scope::new_with_symbols(local_symbols, None),
            std_lib_symbols: HashMap::from([
                ("print".to_string(), RustBinding::Print(std_rust_print)),
                ("len".to_string(), RustBinding::Len),
                ("append".to_string(), RustBinding::Append),
            ]),
        }
    }

    pub fn get_local_binding(&self, symbol: &str) -> Option<EnvironmentBinding> {
        get_local_binding_recursive(symbol, &self.current_scope)
    }

    pub fn add_or_mutate_symbols(
        &mut self,
        symbol: &str,
        binding: EnvironmentBinding,
    ) -> Result<EnvironmentBinding, EvaluatorError> {
        mutate_local_binding_recursive(&mut self.current_scope, symbol, binding)
    }
    pub fn push_scope(&mut self) -> Self {
        let current_scope = self.current_scope.clone();
        let new = Scope::new(Some(current_scope));
        self.current_scope = new;
        self.to_owned()
    }
    pub fn pop_scope(&mut self) -> Self {
        if let Some(parent) = self.current_scope.parent.clone() {
            self.current_scope = *parent;
            self.to_owned()
        } else {
            panic!("Attempted to pop top level scope")
        }
    }
}

/// The user's local scope, where variables will be looked up and mutated accordingly.
///
/// Mutations start at the child level and move up to the parent
#[derive(Debug, Clone)]
pub struct Scope {
    pub local_symbols: HashMap<String, EnvironmentBinding>,
    pub parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new(parent: Option<Scope>) -> Self {
        Self {
            local_symbols: HashMap::new(),
            parent: parent.map(Box::new),
        }
    }

    pub fn new_with_symbols(
        local_symbols: HashMap<String, EnvironmentBinding>,
        parent: Option<Scope>,
    ) -> Self {
        Self {
            local_symbols,
            parent: parent.map(Box::new),
        }
    }
}

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
}

/// The variations an Environment binding value can be.
///
/// NOTE: Void is also regarded as a binding, as a function can return a Void.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    ValueLiteral(ValueLiteral),
    Range(RangeValue),
    Void,
}

/// Value-ing a Range value to something concrete.
#[derive(Debug, Clone, PartialEq)]
pub struct RangeValue {
    pub from: i32,
    pub to: i32,
    pub inclusive: bool,
}

/// The error variants returned from mutating a binding.
#[derive(Debug, Clone)]
pub enum MutateBindingError {
    InvalidRedeclaration,
    NoIdentifier,
}
