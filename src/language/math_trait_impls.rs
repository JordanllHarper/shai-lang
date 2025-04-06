use std::ops::{Add, Div, Mul, Sub};

use crate::{
    environment::{EnvironmentBinding, EnvironmentState, Value},
    evaluator::EvaluatorError,
    language::{Body, FunctionCall, If, Math, MathOperation, NumericLiteral, ValueLiteral},
};

impl Add for NumericLiteral {
    type Output = NumericLiteral;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumericLiteral::Int(i1), NumericLiteral::Int(i2)) => NumericLiteral::Int(i1 + i2),
            (NumericLiteral::Float(f1), NumericLiteral::Float(f2)) => {
                NumericLiteral::Float(f1 + f2)
            }
            (NumericLiteral::Int(i1), NumericLiteral::Float(f1)) => {
                NumericLiteral::Float(i1 as f64 + f1)
            }
            (NumericLiteral::Float(f1), NumericLiteral::Int(i1)) => {
                NumericLiteral::Float(f1 + i1 as f64)
            }
        }
    }
}
impl Sub for NumericLiteral {
    type Output = NumericLiteral;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumericLiteral::Int(i1), NumericLiteral::Int(i2)) => NumericLiteral::Int(i1 - i2),
            (NumericLiteral::Int(i1), NumericLiteral::Float(f1)) => {
                NumericLiteral::Float(i1 as f64 - f1)
            }
            (NumericLiteral::Float(f1), NumericLiteral::Int(i1)) => {
                NumericLiteral::Float(f1 - i1 as f64)
            }
            (NumericLiteral::Float(f1), NumericLiteral::Float(f2)) => {
                NumericLiteral::Float(f1 - f2)
            }
        }
    }
}

impl Mul for NumericLiteral {
    type Output = NumericLiteral;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumericLiteral::Int(i1), NumericLiteral::Int(i2)) => NumericLiteral::Int(i1 * i2),
            (NumericLiteral::Int(i1), NumericLiteral::Float(f1)) => {
                NumericLiteral::Float(i1 as f64 * f1)
            }
            (NumericLiteral::Float(f1), NumericLiteral::Int(i1)) => {
                NumericLiteral::Float(f1 * i1 as f64)
            }
            (NumericLiteral::Float(f1), NumericLiteral::Float(f2)) => {
                NumericLiteral::Float(f1 * f2)
            }
        }
    }
}

impl Div for NumericLiteral {
    type Output = NumericLiteral;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (NumericLiteral::Int(i1), NumericLiteral::Int(i2)) => {
                NumericLiteral::Float(i1 as f64 / i2 as f64)
            }
            (NumericLiteral::Int(i1), NumericLiteral::Float(f1)) => {
                NumericLiteral::Float(i1 as f64 / f1)
            }
            (NumericLiteral::Float(f1), NumericLiteral::Int(i1)) => {
                NumericLiteral::Float(f1 / i1 as f64)
            }
            (NumericLiteral::Float(f1), NumericLiteral::Float(f2)) => {
                NumericLiteral::Float(f1 / f2)
            }
        }
    }
}
