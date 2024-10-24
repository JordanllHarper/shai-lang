use crate::language::*;
use crate::lexer::*;

impl Literal {
    pub fn to_single_value(&self) -> SingleValue {
        match self.clone() {
            Literal::Bool(b) => SingleValue::new_bool(&b.to_string()),
            Literal::Int(i) => SingleValue::new_int(&i.to_string()),
            Literal::String(s) => SingleValue::new_string(&s.to_string()),
            Literal::Float(f) => SingleValue::new_float(&f.to_string()),
        }
    }
}
