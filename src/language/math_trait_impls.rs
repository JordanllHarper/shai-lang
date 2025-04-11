use std::{
    cmp::Ordering,
    ops::{Add, Div, Mul, Sub},
};

use super::{NumericLiteral, Operator};

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

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Operator {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Operator::Divide, Operator::Divide) => Ordering::Equal,
            (Operator::Divide, _) => Ordering::Greater,
            (Operator::Multiply, Operator::Multiply) => Ordering::Equal,
            (Operator::Multiply, Operator::Divide) => Ordering::Less,
            (Operator::Multiply, Operator::Add) => Ordering::Greater,
            (Operator::Multiply, Operator::Subtract) => Ordering::Greater,
            (Operator::Add, Operator::Add) => Ordering::Equal,
            (Operator::Add, Operator::Subtract) => Ordering::Greater,
            (Operator::Add, Operator::Multiply) => Ordering::Less,
            (Operator::Add, Operator::Divide) => Ordering::Less,
            (Operator::Subtract, Operator::Add) => Ordering::Less,
            (Operator::Subtract, Operator::Subtract) => Ordering::Less,
            (Operator::Subtract, Operator::Multiply) => Ordering::Less,
            (Operator::Subtract, Operator::Divide) => Ordering::Less,
        }
    }
    // add code here
}
