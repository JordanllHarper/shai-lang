use crate::{language::*, lexer::*};

fn on_arguments<'a, I>(tokens: &mut I, first_arg: SingleValue) -> FunctionArguments
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let mut args: FunctionArguments = vec![Expression::SingleValue(first_arg)];

    while let Some(t) = tokens.next() {
        let expr = match t {
            Token::Literal(l) => {
                let value_literal = l.to_single_value();
                Expression::SingleValue(value_literal)
            }
            Token::Ident(i) => SingleValue::new_identifier_expression(i),
            Token::Symbol(Symbol::BraceOpen) => {
                // TODO: Handle no expression returned (invalid syntax)
                let t = tokens.next().expect("There should be a function body");
                on_expression(t, tokens, None)
            }
            _ => todo!(),
        };
        args.push(expr);
    }

    args
}

fn on_parameters<'a, I>(tokens: &mut I) -> (FunctionParameters, &mut I)
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let mut parameters: FunctionParameters = Vec::new();

    while let Some(t) = tokens.next() {
        match t {
            Token::Ident(param_ident) => {
                let native_type = match tokens.next() {
                    Some(Token::Kwd(Kwd::DataType(d))) => Some(NativeType::from_datatype_kwd(d)),
                    Some(Token::Symbol(Symbol::ParenClose)) => {
                        let parameter = Parameter::new(param_ident, None);
                        parameters.push(parameter);
                        break;
                    }
                    _ => None,
                };

                let parameter = Parameter::new(param_ident, native_type);
                parameters.push(parameter);
            }
            Token::Symbol(Symbol::Comma) => continue,
            Token::Symbol(Symbol::ParenClose) => break,
            _ => {
                // TODO: Invalid syntax
                todo!()
            }
        };
    }
    (parameters, tokens)
}

fn on_function<'a, I>(tokens: &mut I, ident: &str) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let (args, tokens) = on_parameters(tokens);
    let mut peekable = tokens.peekable();
    let (return_type, mut tokens) = on_return_type(&mut peekable);

    match tokens.next() {
        Some(t) => {
            match t {
                // function declaration
                Token::Symbol(Symbol::BraceOpen) => {
                    // Body
                    let body = on_function_body(&mut tokens);
                    let f = Function::new(ident, args, return_type, body);
                    Expression::Function(Box::new(f))
                }
                // Function expression
                Token::Symbol(Symbol::Equals) => {
                    let t = tokens.next().expect("Syntax error");
                    let expression = on_expression(t, &mut tokens, None);
                    let x = Function::new(ident, args, return_type, expression);
                    Expression::Function(Box::new(x))
                }
                t => {
                    println!("{:?}", t);
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

fn on_return_type<'a, I>(
    tokens: I,
) -> (Option<ReturnType>, Box<dyn Iterator<Item = &'a Token> + 'a>)
where
    I: std::iter::IntoIterator<Item = &'a Token>,
{
    let mut tokens = tokens.into_iter().peekable();
    if let Some(Token::Symbol(Symbol::Arrow)) = tokens.peek() {
        tokens.next();
        // Declaration with return type
        let ret_type = match tokens.next() {
            Some(Token::Kwd(Kwd::DataType(d))) => Some(NativeType::from_datatype_kwd(d)),
            _ => {
                // TODO: Invalid syntax
                todo!()
            }
        };
        let iter = tokens.collect::<Vec<&Token>>().into_iter();
        return (ret_type, Box::new(iter));
    }
    let tokens = tokens.collect::<Vec<&Token>>().into_iter();
    (None, Box::new(tokens))
}

fn on_function_body<'a, I>(tokens: &mut I) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let mut body: Body = Vec::new();
    while let Some(t) = tokens.next() {
        println!("Function body token: {:?}", t);
        match t {
            Token::Symbol(Symbol::BraceClose) => break,
            Token::Symbol(Symbol::Newline) => continue,
            _ => {
                let expression = on_expression(t, tokens, None);
                body.push(expression);
            }
        }
    }
    Expression::Body(body)
}

fn on_evaluation<'a, I>(
    tokens: &mut I,
    ident: &str,
    evaluation_op: EvaluationOperator,
) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    todo!();
}

fn on_assignment<'a, I>(tokens: &mut I, ident: &str) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let t = tokens.next();
    match t {
        Some(Token::Literal(l)) => {
            let sv = l.to_single_value();
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    ident.to_string(),
                ))),
                rhs: Box::new(Expression::SingleValue(sv)),
                operation: Operation::Assignment(None),
            }
        }
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            // TODO: Expression assignment
            todo!()
        }
        Some(Token::Ident(i)) => {
            let lhs = SingleValue::new_identifier_expression(ident);
            let rhs = on_identifier(tokens, i, Some(SingleValue::new_identifier_expression(i)));
            Expression::Operation {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                operation: Operation::Assignment(None),
            }
        }
        _ => todo!(),
    }
}

// Identifier options
//
// Variable usage
// y = *x* + 3
//      |- here is usage
fn on_identifier<'a, I>(tokens: &mut I, ident: &str, previous: Option<Expression>) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    // TODO: Expression (empty statement) e.g. x + y
    // TODO: Variable Usage e.g. y = x + 3
    let t = tokens.next();
    println!("{:?}", t);
    match t {
        // Assignment
        Some(Token::Symbol(Symbol::Equals)) => on_assignment(tokens, ident),

        // Evaluation
        Some(Token::Symbol(Symbol::Equality)) => {
            on_evaluation(tokens, ident, EvaluationOperator::Eq)
        }
        Some(Token::Symbol(Symbol::NotEquality)) => {
            on_evaluation(tokens, ident, EvaluationOperator::Neq)
        }

        Some(Token::Symbol(Symbol::GzEq)) => on_evaluation(tokens, ident, EvaluationOperator::GzEq),
        Some(Token::Symbol(Symbol::LzEq)) => on_evaluation(tokens, ident, EvaluationOperator::LzEq),

        Some(Token::Symbol(Symbol::ChevOpen)) => {
            on_evaluation(tokens, ident, EvaluationOperator::LzEq)
        }
        Some(Token::Symbol(Symbol::ChevClose)) => {
            on_evaluation(tokens, ident, EvaluationOperator::GzEq)
        }
        Some(Token::Symbol(Symbol::MathSymbol(t))) => {
            let operation = MathOperation::from_token(t);
            on_math_expression(
                tokens,
                operation,
                previous.expect("There should be a previous expresssion this is applying to."),
            )
        }

        // Operations
        // Function call if value literal or string
        Some(Token::Literal(l)) => on_function_call(tokens, ident, l),
        Some(Token::Symbol(Symbol::ParenOpen)) => on_function(tokens, ident),
        _ => Expression::SingleValue(SingleValue::Identifier(ident.to_string())),
    }
}

fn on_function_call<'a, I>(tokens: &mut I, ident: &str, first_arg: &Literal) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let args = on_arguments(tokens, first_arg.to_single_value());

    Expression::Operation {
        lhs: Box::new(SingleValue::new_identifier_expression(ident)),
        rhs: Box::new(Expression::MultipleValues(args)),
        operation: Operation::FunctionCall,
    }
}

fn on_math_expression<'a, I>(
    tokens: &mut I,
    operation: MathOperation,
    previous: Expression,
) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let prev = previous.clone();

    let t = tokens.next().expect("There should be more tokens");
    let rhs = on_expression(t, tokens, Some(prev));
    Expression::Operation {
        lhs: Box::new(previous),
        rhs: Box::new(rhs),
        operation: Operation::Math(operation),
    }
}

fn on_expression<'a, I>(t: &Token, tokens: &mut I, previous: Option<Expression>) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    println!("Token: {:?}", t);
    
    match t {
        Token::Ident(ident) => on_identifier(
            tokens,
            ident,
            Some(SingleValue::new_identifier_expression(ident)),
        ),
        Token::Kwd(k) => on_keyword(tokens, k),
        Token::Literal(l) => Expression::SingleValue(l.to_single_value()),

        Token::Symbol(Symbol::MathSymbol(t)) => on_math_expression(
            tokens,
            MathOperation::from_token(t),
            previous.expect("Given a math expression, there should be a lhs."),
        ),

        _ => todo!(),
    }
}

fn on_keyword<'a, I>(tokens: &mut I, k: &Kwd) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let remaining_expression = on_expression(tokens.next().expect("There should be an expression after a keyword."), tokens, None);
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
        Kwd::Const => todo!(),
    }
}

pub fn parse<'a, I>(tokens: I) -> Expression
where
    I: std::iter::IntoIterator<Item = &'a Token>,
{
    let mut iter = tokens.into_iter();
    on_expression(iter.next().expect("There should be a token that exists in the program."), &mut iter, None)
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
    fn variable_assignment_with_another_variable() {
        // y = x + 3
        test(
            vec![
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Ident("x".to_string()),
            ],
            Expression::Operation {
                lhs: Box::new(SingleValue::new_identifier_expression("y")),
                rhs: Box::new(SingleValue::new_identifier_expression("x")),
                operation: Operation::Assignment(None),
            },
        )
    }

    #[test]
    fn variable_usage_in_expressions() {
        // y = x + 3
        test(
            vec![
                Token::Ident("y".to_string()),
                Token::Symbol(Symbol::Equals),
                Token::Ident("x".to_string()),
                Token::Symbol(Symbol::MathSymbol(MathSymbol::Plus)),
                Token::Literal(Literal::Int(3)),
            ],
            Expression::Operation {
                lhs: Box::new(SingleValue::new_identifier_expression("y")),
                rhs: Box::new(Expression::Operation {
                    lhs: Box::new(SingleValue::new_identifier_expression("x")),
                    rhs: Box::new(SingleValue::new_value_literal_expression(
                        ValueLiteral::new(NativeType::Int, "3"),
                    )),
                    operation: Operation::Math(MathOperation::Add),
                }),
                operation: Operation::Assignment(None),
            },
        )
    }

    #[test]
    fn function_declaration_no_params_no_return() {
        // do_nothing () {\n
        // \n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Symbol(Symbol::ParenClose),
                // End of args
                Token::Symbol(Symbol::BraceOpen),
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
    fn function_declaration_no_params_return() {
        // do_nothing () -> int {\n
        // \n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Symbol(Symbol::ParenClose),
                // End of Args
                //Return type
                Token::Symbol(Symbol::Arrow),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                //End of Return Type
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            ))),
        );
        // with body
        test(
            vec![
                // Signature
                Token::Ident("add".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Symbol(Symbol::ParenClose),
                // End of Args
                //Return type
                Token::Symbol(Symbol::Arrow),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                //End of Return Type
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // body return numOne + numTwo
                Token::Kwd(Kwd::Return),
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::MathSymbol(MathSymbol::Plus)),
                Token::Ident("numTwo".to_string()),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "add",
                vec![],
                Some(NativeType::Int),
                Expression::Body(vec![Expression::Statement {
                    expression: Box::new(Expression::Operation {
                        lhs: Box::new(SingleValue::new_identifier_expression("numOne")),
                        rhs: Box::new(SingleValue::new_identifier_expression("numTwo")),
                        operation: Operation::Math(MathOperation::Add),
                    }),
                    operation: Operation::Return,
                }]),
            ))),
        )
    }

    #[test]
    fn function_declaration_params_no_return() {
        // do_nothing (numOne, numTwo) {\n
        // \n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Symbol(Symbol::ParenClose),
                // End of args
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                None,
                Expression::Body(vec![]),
            ))),
        );
        // typed arguments
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::ParenClose),
                // End of args
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![
                    Parameter::new("numOne", Some(NativeType::Int)),
                    Parameter::new("numTwo", Some(NativeType::Int)),
                ],
                None,
                Expression::Body(vec![]),
            ))),
        )
    }

    #[test]
    fn function_declaration_params_return() {
        // do_nothing (numOne, numTwo) -> int {\n
        // \n
        // }\n
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Symbol(Symbol::ParenClose),
                // End of Args
                //Return type
                Token::Symbol(Symbol::Arrow),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                //End of Return Type
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![
                    Parameter::new("numOne", None),
                    Parameter::new("numTwo", None),
                ],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            ))),
        );
        // Typed Parameters
        test(
            vec![
                // Signature
                Token::Ident("do_nothing".to_string()),
                // Args
                Token::Symbol(Symbol::ParenOpen),
                Token::Ident("numOne".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::Comma),
                Token::Ident("numTwo".to_string()),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                Token::Symbol(Symbol::ParenClose),
                // End of Params
                //Return type
                Token::Symbol(Symbol::Arrow),
                Token::Kwd(Kwd::DataType(DataTypeKwd::Int)),
                //End of Return Type
                Token::Symbol(Symbol::BraceOpen),
                Token::Symbol(Symbol::Newline),
                // end
                Token::Symbol(Symbol::BraceClose),
                Token::Symbol(Symbol::Newline),
            ],
            Expression::Function(Box::new(Function::new(
                "do_nothing",
                vec![
                    Parameter::new("numOne", Some(NativeType::Int)),
                    Parameter::new("numTwo", Some(NativeType::Int)),
                ],
                Some(NativeType::Int),
                Expression::Body(vec![]),
            ))),
        )
    }

    #[test]
    fn function_expression_params_no_return() {
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
                Token::Symbol(Symbol::Equals),
                // End of return type
                // body (assume 4 spaces)
                Token::Ident("numOne".to_string()),
                Token::Symbol(Symbol::MathSymbol(MathSymbol::Plus)),
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
    fn function_call_no_values() {
        // print "Hello"
        test(
            vec![
                Token::Ident("print".to_string()),
                Token::Literal(Literal::String("Hello".to_string())),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "print".to_string(),
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
                Token::Literal(Literal::Float(3.5)),
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
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "print".to_string(),
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
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "print".to_string(),
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
                Token::Literal(Literal::String("Hello".to_string())),
                Token::Literal(Literal::Bool(true)),
            ],
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(
                    "print".to_string(),
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
