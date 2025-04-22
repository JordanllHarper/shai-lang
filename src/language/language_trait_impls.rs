use std::fmt::Display;

use super::NumericLiteral;
use super::*;

impl PartialEq for NumericLiteral {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Float(l0), Self::Int(r0)) => *l0 == *r0 as f64,
            (Self::Int(l0), Self::Float(r0)) => *l0 as f64 == *r0,
        }
    }
}

impl Eq for NumericLiteral {}

impl PartialEq for ValueLiteral {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                ValueLiteral::CharacterBased(CharacterBasedLiteral::Char(c1)),
                ValueLiteral::CharacterBased(CharacterBasedLiteral::Char(c2)),
            ) => c1 == c2,
            (ValueLiteral::Numeric(n1), ValueLiteral::Numeric(n2)) => n1 == n2,
            (
                ValueLiteral::CharacterBased(CharacterBasedLiteral::String(s1)),
                ValueLiteral::CharacterBased(CharacterBasedLiteral::String(s2)),
            ) => s1 == s2,
            (ValueLiteral::Bool(b1), ValueLiteral::Bool(b2)) => b1 == b2,
            (ValueLiteral::Array(a1), ValueLiteral::Array(a2)) => a1 == a2,
            (ValueLiteral::Dictionary(d1), ValueLiteral::Dictionary(d2)) => {
                if d1.len() != d2.len() {
                    return false;
                }

                for (key_1, key_2) in d1.keys().zip(d2.keys()) {
                    if key_1 != key_2 {
                        return false;
                    }
                }

                true
            }
            (ValueLiteral::Function, ValueLiteral::Function) => false,
            _ => false,
        }
    }
}

impl Display for CharacterBasedLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            CharacterBasedLiteral::Char(c) => c.to_string(),
            CharacterBasedLiteral::String(s) => s.to_string(),
        };
        f.write_str(&s)
    }
}

impl Display for NumericLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            NumericLiteral::Int(i) => &i.to_string(),
            NumericLiteral::Float(f) => &f.to_string(),
        };
        f.write_str(s)
    }
    // add code here
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = format!(
            "{} ({}) {}",
            self.ident,
            {
                if let Some((head_list, l)) = self.params.split_last() {
                    let l = l
                        .iter()
                        .fold(String::new(), |acc, p| format!("{}, {}", acc, p));
                    format!("{}{}", l, head_list)
                } else {
                    "".to_string()
                }
            },
            match &self.return_type {
                Some(nt) => format!("-> {}", nt),
                None => "".to_string(),
            }
        );
        f.write_str(&s)
    }
}

impl Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = format!(
            "{}{}",
            self.ident,
            match self.native_type {
                Some(ref n) => format!(":{}", n),
                None => "".to_string(),
            }
        );

        f.write_str(&s)
    }
}

impl Display for NativeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            NativeType::Void => "void",
            NativeType::Char => "char",
            NativeType::Int => "int",
            NativeType::String => "string",
            NativeType::Float => "float",
            NativeType::Bool => "bool",
            NativeType::Array => "arr",
            NativeType::Dictionary => "dict",
            NativeType::Function => "func",
        };

        f.write_str(s)
    }
}
