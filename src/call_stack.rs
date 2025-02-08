use std::collections::BTreeMap;

use crate::language::Function;

#[derive(Debug, PartialEq)]
pub enum StackFrameValue {
    Void,
    Int(i32),
    String(String),
    Char(char),
    Array(Vec<StackFrameValue>),
    Dictionary(BTreeMap<StackFrameValue, StackFrameValue>),
    Bool(bool),
    Float(f32),
    Function(Function),
}

#[derive(Debug, PartialEq)]
pub struct StackFrameReference {
    value: StackFrameValue,
    is_mutable: bool,
}

impl StackFrameReference {
    pub fn new(value: StackFrameValue, is_mutable: bool) -> Self {
        Self { value, is_mutable }
    }
}

#[derive(Debug, PartialEq)]
pub struct StackFrame {
    //            name of reference -> the value
    //            e.g. x            -> 5
    //            e.g. print        -> Function that prints to stdout
    values: BTreeMap<String, StackFrameReference>,

    // frames that can still be referenced from this frame (such as in the case of a sub scope)
    inner_frames: Vec<StackFrame>,
}

impl StackFrame {
    pub fn new_init() -> Self {
        Self {
            values: BTreeMap::new(),
            inner_frames: Vec::new(),
        }
    }

    pub fn new(
        values: BTreeMap<String, StackFrameReference>,
        inner_frames: Vec<StackFrame>,
    ) -> Self {
        Self {
            values,
            inner_frames,
        }
    }

    pub fn add_or_mutate_reference(mut self, key: String, reference: StackFrameReference) -> Self {
        self.values.insert(key, reference);
        self
    }

    pub fn get_reference(&self, ident: &str) -> Option<&StackFrameReference> {
        self.values.get(ident)
    }
}
