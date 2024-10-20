use crate::language::*;
use crate::lexer::*;
use crate::parser_helpers::*;

fn on_floating_point<'a, I>(tokens: &mut I, before_decimal: i32) -> ValueLiteral
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    // Advance past whitespace given the input is 3.   41
    let after_decimal = advance_past_whitespace(tokens);
    match after_decimal {
        Some(Token::Literal(Literal::IntLiteral(i))) => ValueLiteral::new(
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
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    let num = if let Some(Token::Symbol(Symbol::Period)) = advance_past_whitespace(tokens) {
        on_floating_point(tokens, i)
    } else {
        ValueLiteral::new(NativeType::Int, &i.to_string())
    };

    Expression::SingleValue(SingleValue::ValueLiteral(num))
}

fn on_function_declaration<'a, I>(tokens: &mut I, ident: String) -> Expression
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    let mut args: FunctionParameters = Vec::new();

    // Parse args
    while let Some(t) = advance_past_whitespace(tokens) {
        match t {
            Token::Ident(arg_ident) => {
                let native_type = match advance_past_whitespace(tokens) {
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
            _ => {
                // TODO: Invalid syntax
                todo!()
            }
        };
    }
    let return_type: Option<ReturnType> = match advance_past_whitespace(tokens) {
        Some(t) => {
            match t {
                Token::Symbol(Symbol::Arrow) => {
                    match advance_past_whitespace(tokens) {
                        Some(Token::Kwd(Kwd::DataType(d))) => {
                            Some(NativeType::from_datatype_kwd(d))
                        }
                        _ => {
                            // TODO: Invalid syntax
                            todo!()
                        }
                    }
                }
                Token::Symbol(Symbol::BraceOpen) => None,
                _ => {
                    // TODO: Invalid syntax
                    todo!()
                }
            }
        }
        None => None,
    };

    let body = on_expression(tokens, None);
    if let Some(body) = body {
        Expression::FunctionDeclaration(Box::new(FunctionDeclaration::new(
            &ident,
            args,
            return_type,
            body,
        )))
    } else {
        // TODO: Invalid syntax
        todo!("{:?} Invalid syntax", body)
    }
}

/// TODO: Handle various types of operations, not just function calls
fn on_operation<'a, I>(tokens: &mut I, ident: String, expression: Expression) -> Expression
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
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
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    todo!();
}

fn on_assignment<'a, I>(tokens: &mut I, ident: String, previous: Option<Expression>) -> Expression
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    let t = advance_past_whitespace(tokens);
    match t {
        Some(Token::Literal(Literal::IntLiteral(x))) => {
            let num = on_integer_literal(tokens, *x);
            let expr = match advance_past_whitespace(tokens) {
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
fn on_identifier<'a, I>(
    tokens: &mut I,
    ident: String,
    previous: Option<Expression>,
) -> Option<Expression>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    // TODO: Expression (empty statement) e.g. x + y
    // TODO: Variable Usage e.g. y = x + 3
    let t = advance_past_multiple(
        tokens,
        &[
            Token::Symbol(Symbol::Newline),
            Token::Symbol(Symbol::Whitespace),
        ],
    );
    println!("{:?}", t);
    match t {
        // Assignment
        Some(Token::Symbol(Symbol::Equals)) => Some(on_assignment(tokens, ident, previous)),

        // Evaluation
        Some(Token::Symbol(Symbol::Equality)) => {
            Some(on_evaluation(tokens, ident, EvaluationOperator::Eq))
        }
        Some(Token::Symbol(Symbol::NotEquality)) => {
            Some(on_evaluation(tokens, ident, EvaluationOperator::Neq))
        }

        Some(Token::Symbol(Symbol::GzEq)) => {
            Some(on_evaluation(tokens, ident, EvaluationOperator::GzEq))
        }
        Some(Token::Symbol(Symbol::LzEq)) => {
            Some(on_evaluation(tokens, ident, EvaluationOperator::LzEq))
        }

        Some(Token::Symbol(Symbol::ChevOpen)) => {
            Some(on_evaluation(tokens, ident, EvaluationOperator::LzEq))
        }
        Some(Token::Symbol(Symbol::ChevClose)) => {
            Some(on_evaluation(tokens, ident, EvaluationOperator::GzEq))
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
            let rhs = Box::new(on_expression(tokens, Some(current.clone()))?);
            println!("Rhs: {:?}", rhs);
            let expr = Expression::Operation {
                lhs: Box::new(current),
                rhs,
                operation: Operation::Math(op),
            };
            println!("Got here");
            Some(expr)
        }

        // Operations
        // Function call if value literal or string
        // TODO: Clean up these expects
        Some(Token::Literal(_)) | Some(Token::Symbol(Symbol::Quote)) => {
            let args = parse_arguments(tokens, t.expect("This isn't None at this point"))
                .expect("If this is None, aaaaaaaaaa idk");
            let expression = Expression::MultipleValues(args);
            Some(on_operation(tokens, ident, expression))
        }
        Some(Token::Symbol(Symbol::ParenOpen)) => Some(on_function_declaration(tokens, ident)),
        _ => Some(Expression::SingleValue(SingleValue::Identifier(ident))),
    }
}

fn on_expression<'a, I>(tokens: &mut I, previous: Option<Expression>) -> Option<Expression>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    let t = advance_past_multiple(
        tokens,
        &[
            Token::Symbol(Symbol::Newline),
            Token::Symbol(Symbol::Whitespace),
        ],
    );
    println!("Token: {:?}", t);
    match t {
        Some(Token::Ident(ident)) => {
            let node = on_identifier(tokens, ident.to_string(), previous)?;
            Some(node)
        }
        Some(Token::Kwd(k)) => {
            let node = on_keyword(tokens, k);
            Some(node)
        }
        Some(Token::Literal(l)) => match l {
            Literal::BoolLiteral(_) => Some(Expression::SingleValue(l.to_vl())),
            Literal::IntLiteral(i) => Some(on_integer_literal(tokens, *i)),
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
                rhs: Box::new(on_expression(tokens, prev)?),
                operation: Operation::Math(op),
            };
            println!("Got here");
            Some(expr)
        }
        None => None,
        _ => todo!(),
    }
}

fn on_keyword<'a, I>(tokens: &mut I, k: &Kwd) -> Expression
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    let remaining_expression = on_expression(tokens, None);
    if let Some(e) = remaining_expression {
        match k {
            Kwd::Return => Expression::Statement {
                expression: Box::new(e),
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
    } else {
        // TODO: Invalid syntax, incomplete function
        todo!()
    }
}

pub fn parse(tokens: &[Token]) -> Option<Expression> {
    on_expression(&mut tokens.iter(), None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn test(input: Vec<Token>, expected: Expression) {
        let actual = parse(&input).unwrap();
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
            Expression::FunctionDeclaration(Box::new(FunctionDeclaration::new(
                "do_nothing",
                vec![],
                None,
                Expression::Body { body: vec![] }
            ))),
        );
    }

    #[test]
    fn function_declaration() {
        // No specified types
        // add (numOne, numTwo) {\n
        //     return numOne + numTwo\n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("add".to_string()),
                Token::whitespace(),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::Comma),
                Token::whitespace(),
                Token::Ident("numTwo".to_string()),
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
                Token::Kwd(Kwd::Return),
                Token::whitespace(),
                Token::Ident("numOne".to_string()),
                Token::whitespace(),
                Token::Symbol(Symbol::Plus),
                Token::whitespace(),
                Token::Ident("numTwo".to_string()),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::FunctionDeclaration(Box::new(FunctionDeclaration::new(
                "add",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                None,
                Expression::Statement {
                    expression: Box::new(Expression::Operation {
                        lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                            "numOne".to_string(),
                        ))),
                        rhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                            "numTwo".to_string(),
                        ))),
                        operation: Operation::Math(MathOperation::Add),
                    }),

                    operation: Operation::Return,
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
                Token::Symbol(Symbol::Quote),
                Token::Ident("Hello".to_string()),
                Token::Symbol(Symbol::Quote),
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
                Token::Literal(Literal::IntLiteral(3)),
                Token::Symbol(Symbol::Period),
                Token::Literal(Literal::IntLiteral(5)),
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
                Token::whitespace(),
                Token::Symbol(Symbol::Quote),
                Token::Ident("Hello".to_string()),
                Token::Symbol(Symbol::Quote),
                Token::whitespace(),
                Token::Symbol(Symbol::Quote),
                Token::Ident("World".to_string()),
                Token::Symbol(Symbol::Quote),
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
                Token::whitespace(),
                Token::Symbol(Symbol::Quote),
                Token::Ident("Hello".to_string()),
                Token::Symbol(Symbol::Quote),
                Token::whitespace(),
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
                Token::whitespace(),
                Token::Symbol(Symbol::Quote),
                Token::Ident("Hello".to_string()),
                Token::Symbol(Symbol::Quote),
                Token::whitespace(),
                Token::Literal(Literal::BoolLiteral(true)),
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
                Token::Literal(Literal::IntLiteral(5)),
            ],
            expected.clone(),
        );
        // Whitespace handling
        // "x = 5"
        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Whitespace),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::Whitespace),
                Token::Literal(Literal::IntLiteral(5)),
            ],
            expected.clone(),
        );

        // Uneven whitespace handling (unlike swift)
        // "x= 5"

        test(
            vec![
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Symbol(Symbol::Whitespace),
                Token::Literal(Literal::IntLiteral(5)),
            ],
            expected,
        );
    }
}
