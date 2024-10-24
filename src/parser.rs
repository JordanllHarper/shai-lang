use crate::{language::*, lexer::*};

fn on_floating_point<'a, I>(tokens: &mut I, before_decimal: i32) -> ValueLiteral
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    // Advance past whitespace given the input is 3.   41
    let after_decimal = tokens.next();
    match after_decimal {
        Some(Token::Literal(Literal::Int(i))) => ValueLiteral::new(
            NativeType::Float,
            &(before_decimal.to_string() + "." + &i.to_string()),
        ),
        _ => {
            // invalid syntax
            todo!()
        }
    }
}
fn on_integer_literal<'a, I>(tokens: &mut I, i: i32) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let num = if let Some(Token::Symbol(Symbol::Period)) = tokens.next() {
        on_floating_point(tokens, i)
    } else {
        ValueLiteral::new(NativeType::Int, &i.to_string())
    };

    Expression::SingleValue(SingleValue::ValueLiteral(num))
}

fn on_function<'a, I>(tokens: &mut I, ident: String) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let mut args: FunctionParameters = Vec::new();

    // Parse args
    while let Some(t) = tokens.next() {
        match t {
            Token::Ident(arg_ident) => {
                let native_type = match tokens.next() {
                    Some(Token::Kwd(Kwd::DataType(d))) => Some(NativeType::from_datatype_kwd(d)),
                    Some(Token::Symbol(Symbol::ParenClose)) => {
                        let parameter = Parameter::new(arg_ident, None);
                        args.push(parameter);
                        break;
                    }
                    _ => None,
                };

                let parameter = Parameter::new(arg_ident, native_type);
                args.push(parameter);
            }
            // TODO: We break twice eek
            Token::Symbol(Symbol::ParenClose) => break,
            _ => {
                // TODO: Invalid syntax
                todo!()
            }
        };
    }
    match tokens.next() {
        Some(t) => {
            match t {
                Token::Symbol(Symbol::Arrow) => {
                    // Declaration with return type
                    let ret_type = match tokens.next() {
                        Some(Token::Kwd(Kwd::DataType(d))) => {
                            Some(NativeType::from_datatype_kwd(d))
                        }
                        _ => {
                            // TODO: Invalid syntax
                            todo!()
                        }
                    };
                    on_function_declaration(tokens, ret_type);
                    todo!()
                }
                Token::Symbol(Symbol::BraceOpen) => {
                    // Declaration without return type
                    on_function_declaration(tokens, None)
                }
                Token::Symbol(Symbol::Equals) => {
                    let expression = on_expression(tokens, None);
                    if let Some(e) = expression {
                        Expression::Function(Box::new(Function::new(&ident, args, None, e)))
                    } else {
                        // TODO: Invalid syntax
                        todo!("{:?} Invalid syntax", expression)
                    }
                }
                _ => {
                    // TODO: Invalid syntax
                    todo!()
                }
            }
        }
        None => {
            // TODO: Invalid syntax
            todo!()
        }
    }
}

fn on_function_declaration<'a, I>(tokens: &mut I, ret_type: Option<ReturnType>) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    todo!();
}

/// TODO: Handle various types of operations, not just function calls
fn on_operation<'a, I>(tokens: &mut I, ident: String, expression: Expression) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    Expression::Operation {
        lhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
            ValueLiteral::new(NativeType::Function, &ident),
        ))),
        rhs: Box::new(expression),
        operation: Operation::FunctionCall,
    }
}

fn on_evaluation<'a, I>(
    tokens: &mut I,
    ident: String,
    evaluation_op: EvaluationOperator,
) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    todo!();
}

fn on_assignment<'a, I>(tokens: &mut I, ident: String, previous: Option<Expression>) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let t = tokens.next();
    match t {
        Some(Token::Literal(Literal::Int(x))) => {
            let num = on_integer_literal(tokens, *x);
            let expr = match tokens.next() {
                Some(Token::Symbol(Symbol::Newline)) | None => num,

                // This is not the end of the assignment woooooo...
                // TODO: Expression operation assignment
                // math assignment
                Some(
                    Token::Symbol(Symbol::Plus)
                    | Token::Symbol(Symbol::Minus)
                    | Token::Symbol(Symbol::Asterisk)
                    | Token::Symbol(Symbol::FwdSlash),
                ) => {
                    // TODO: Clean up
                    let op = MathOperation::from_token(t.unwrap().clone()).unwrap();
                    let expr = Expression::Operation {
                        lhs: Box::new(previous.expect("Unexpected syntax")),
                        rhs: Box::new(on_expression(tokens, None).unwrap()),
                        operation: Operation::Math(op),
                    };
                    println!("Got here");
                    expr
                }
                _ => todo!(),
            };
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(ident))),
                rhs: Box::new(expr),
                operation: Operation::Assignment(None),
            }
        }
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            // TODO: Expression assignment
            let expression = on_expression(tokens, None);
            todo!()
        }
        _ => todo!(),
    }
    // TODO: Function assignment
    // E.g. fname (...) = ...
}

// Identifier options
//
// Variable usage
// y = *x* + 3
//      |- here is usage
fn on_identifier<'a, I>(tokens: &mut I, ident: String, previous: Option<Expression>) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    // TODO: Expression (empty statement) e.g. x + y
    // TODO: Variable Usage e.g. y = x + 3
    let t = tokens.next();
    println!("{:?}", t);
    match t {
        // Assignment
        Some(Token::Symbol(Symbol::Equals)) => Some(on_assignment(tokens, ident, previous)),

        // Evaluation
        Some(Token::Symbol(Symbol::Equality)) => {
            (on_evaluation(tokens, ident, EvaluationOperator::Eq))
        }
        Some(Token::Symbol(Symbol::NotEquality)) => {
            (on_evaluation(tokens, ident, EvaluationOperator::Neq))
        }

        Some(Token::Symbol(Symbol::GzEq)) => on_evaluation(tokens, ident, EvaluationOperator::GzEq),
        Some(Token::Symbol(Symbol::LzEq)) => on_evaluation(tokens, ident, EvaluationOperator::LzEq),

        Some(Token::Symbol(Symbol::ChevOpen)) => {
            on_evaluation(tokens, ident, EvaluationOperator::LzEq)
        }
        Some(Token::Symbol(Symbol::ChevClose)) => {
            on_evaluation(tokens, ident, EvaluationOperator::GzEq)
        }
        Some(
            Token::Symbol(Symbol::Plus)
            | Token::Symbol(Symbol::Minus)
            | Token::Symbol(Symbol::Asterisk)
            | Token::Symbol(Symbol::FwdSlash),
        ) => {
            let current = Expression::SingleValue(SingleValue::Identifier(ident));
            // TODO: Clean up
            let op = MathOperation::from_token(t.unwrap().clone()).unwrap();
            let rhs = Box::new(on_expression(tokens, Some(current.clone())));
            println!("Rhs: {:?}", rhs);
            let expr = Expression::Operation {
                lhs: Box::new(current),
                rhs,
                operation: Operation::Math(op),
            };
            println!("Got here");
            expr
        }

        // Operations
        // Function call if value literal or string
        // TODO: Clean up these expects
        Some(Token::Literal(_)) | Some(Token::Symbol(Symbol::Quote)) => {
            todo!()
        }
        Some(Token::Symbol(Symbol::ParenOpen)) => Some(on_function(tokens, ident)),
        _ => Some(Expression::SingleValue(SingleValue::Identifier(ident))),
    }
}

fn on_expression<'a, I>(tokens: &mut I, previous: Option<Expression>) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let t = tokens.next();
    println!("Token: {:?}", t);
    match t {
        Some(Token::Ident(ident)) => on_identifier(tokens, ident.to_string(), previous),
        Some(Token::Kwd(k)) => on_keyword(tokens, k),
        Some(Token::Literal(l)) => match l {
            Literal::Bool(_) => (Expression::SingleValue(l.to_vl())),
            Literal::Int(i) => (on_integer_literal(tokens, *i)),
            Literal::String(s) => Some(Expression::SingleValue(SingleValue::ValueLiteral(
                ValueLiteral::new(NativeType::String, s),
            ))),
        },
        Some(
            Token::Symbol(Symbol::Plus)
            | Token::Symbol(Symbol::Minus)
            | Token::Symbol(Symbol::Asterisk)
            | Token::Symbol(Symbol::FwdSlash),
        ) => {
            // TODO: Clean up
            let prev = previous.clone();
            let op = MathOperation::from_token(t.unwrap().clone()).unwrap();
            let expr = Expression::Operation {
                lhs: Box::new(previous.expect("Unexpected syntax")),
                rhs: Box::new(on_expression(tokens, prev)),
                operation: Operation::Math(op),
            };
            (expr)
        }
        None => None,
        _ => todo!(),
    }
}

fn on_keyword<'a, I>(tokens: &mut I, k: &Kwd) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let remaining_expression = on_expression(tokens, None);
    match k {
        Kwd::Return => Expression::Statement {
            expression: Box::new(remaining_expression),
            operation: Operation::Return,
        },
        Kwd::DataType(_) => todo!(),
        Kwd::While => todo!(),
        Kwd::For => todo!(),
        Kwd::If => todo!(),
        Kwd::In => todo!(),
        Kwd::Break => todo!(),
        Kwd::Include => todo!(),
        Kwd::Else => todo!(),
    }
}

pub fn parse(tokens: &[Token]) -> Expression {
    on_expression(
        &mut tokens.iter().filter(|t| **t == Token::whitespace()),
        None,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn test(input: Vec<Token>, expected: Expression) {
        let actual = parse(&input);
        assert_eq!(expected, actual);
    }

    #[test]
    fn function_declaration_no_args() {
        // do_nothing () {\n
        // \n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("add".to_string()),
                Token::whitespace(),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Symbol(Symbol::ParenClose),
                // End of args
                // Start of return type
                Token::whitespace(),
                Token::Symbol(Symbol::BraceOpen),
                // body (assume 4 spaces)
                Token::whitespace(),
                Token::whitespace(),
                Token::whitespace(),
                Token::whitespace(),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![],
                None,
                Expression::Body(vec![]),
            ))),
        );
    }

    #[test]
    fn function_expression() {
        // No specified types
        // add (numOne, numTwo) = num_one + num_two\n
        test(
            vec![
                // Signature
                Token::Ident("add".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Symbol(Symbol::ParenClose),
                // End of args
                // Start of return type
                Token::Symbol(Symbol::Equals),
                // End of return type
                // body (assume 4 spaces)
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::Plus),
                Token::Ident("numTwo".to_string()),
                Token::Symbol(Symbol::Newline),
                // end
            ],
            Expression::Function(Box::new(Function::new(
                "add",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                None,
                Expression::Operation {
                    lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                        "numOne".to_string(),
                    ))),
                    rhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                        "numTwo".to_string(),
                    ))),
                    operation: Operation::Math(MathOperation::Add),
                },
            ))),
        );
    }

    #[test]
    fn function_call() {
        // print "Hello"
        test(
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                    ValueLiteral::new(NativeType::Function, "print"),
                ))),
                rhs: Box::new(Expression::MultipleValues(vec![Expression::SingleValue(
                    SingleValue::ValueLiteral(ValueLiteral::new(NativeType::String, "Hello")),
                )])),
                operation: Operation::FunctionCall,
            },
        );
    }

    #[test]
    fn floating_point_nums_in_assignment() {
        // x=3.5\n
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(3)),
                Token::Symbol(Symbol::Period),
                Token::Literal(Literal::Int(5)),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "x".to_string(),
                ))),
                rhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                    ValueLiteral::new(NativeType::Float, "3.5"),
                ))),
                operation: Operation::Assignment(None),
            },
        );
    }

    #[test]
    fn function_call_multiple_values() {
        // print "Hello" "World"
        test(
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Literal(Literal::String("World".to_string())),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                    ValueLiteral::new(NativeType::Function, "print"),
                ))),
                rhs: Box::new(Expression::MultipleValues(vec![
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new_string(
                        "Hello",
                    ))),
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new_string(
                        "World",
                    ))),
                ])),
                operation: Operation::FunctionCall,
            },
        );
    }

    #[test]
    fn function_call_multiple_values_with_identifier() {
        // print "Hello" x
        test(
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Ident("x".to_string()),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                    ValueLiteral::new(NativeType::Function, "print"),
                ))),
                rhs: Box::new(Expression::MultipleValues(vec![
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new_string(
                        "Hello",
                    ))),
                    Expression::SingleValue(SingleValue::Identifier("x".to_string())),
                ])),
                operation: Operation::FunctionCall,
            },
        );
    }

    #[test]
    fn function_call_multiple_values_with_literal() {
        // print "Hello" true
        test(
            vec![
                Token::Ident("print".to_string()),
                Token::Symbol(Symbol::Quote),
                Token::Ident("Hello".to_string()),
                Token::Symbol(Symbol::Quote),
                Token::Literal(Literal::Bool(true)),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                    ValueLiteral::new(NativeType::Function, "print"),
                ))),
                rhs: Box::new(Expression::MultipleValues(vec![
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new_string(
                        "Hello",
                    ))),
                    Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new(
                        NativeType::Bool,
                        "true",
                    ))),
                ])),
                operation: Operation::FunctionCall,
            },
        );
    }

    #[test]
    fn literal_assignment() {
        // "x=5"
        let expected = Expression::Operation {
            lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                "x".to_string(),
            ))),
            rhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                ValueLiteral::new(NativeType::Int, "5"),
            ))),
            operation: Operation::Assignment(None),
        };
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected.clone(),
        );
        // Whitespace handling
        // "x = 5"
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected.clone(),
        );

        // Uneven whitespace handling (unlike swift)
        // "x= 5"

        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Literal(Literal::Int(5)),
            ],
            expected,
        );
    }
}
