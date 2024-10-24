use std::iter::Peekable;

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
                on_expression(tokens.next().unwrap(), tokens, None).unwrap()
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
    let mut args: FunctionParameters = Vec::new();

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

            Token::Symbol(Symbol::ParenClose) => break,
            _ => {
                // TODO: Invalid syntax
                todo!()
            }
        };
    }
    (args, tokens)
}
fn on_function<'a, I>(tokens: &mut I, ident: String) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let (args, tokens) = on_parameters(tokens);
    let mut peekable = tokens.peekable();
    let (return_type, peekable) = on_return_type(&mut peekable);
    let mut tokens = peekable.collect::<Vec<&Token>>().into_iter();

    match tokens.next() {
        Some(t) => {
            match t {
                // function declaration
                Token::Symbol(Symbol::BraceOpen) => {
                    // Body
                    let body = on_function_body(&mut tokens);
                    let f = Function::new(&ident, args, return_type, body);
                    Expression::Function(Box::new(f))
                }
                // Function expression
                Token::Symbol(Symbol::Equals) => {
                    let expression = on_expression(tokens.next().unwrap(), &mut tokens, None);
                    if let Some(expression) = expression {
                        let x = Function::new(&ident, args, return_type, expression);
                        Expression::Function(Box::new(x))
                    } else {
                        // TODO: Invalid syntax
                        todo!()
                    }
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

fn on_return_type<'a, I>(tokens: &mut Peekable<I>) -> (Option<ReturnType>, &mut Peekable<I>)
where
    I: std::iter::Iterator<Item = &'a Token>,
{
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
        return (ret_type, tokens);
    }
    (None, tokens)
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
                let expression = on_expression(t, tokens, None).unwrap();
                body.push(expression);
            }
        }
    }
    Expression::Body(body)
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
        Some(Token::Literal(l)) => {
            let sv = l.to_single_value();
            Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::Identifier(ident))),
                rhs: Box::new(Expression::SingleValue(sv)),
                operation: Operation::Assignment(None),
            }
        }
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            // TODO: Expression assignment
            let expression = on_expression(tokens.next().unwrap(), tokens, None);
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
        Some(Token::Symbol(Symbol::Equals)) => on_assignment(tokens, ident, previous),

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
        Some(
            Token::Symbol(Symbol::Plus)
            | Token::Symbol(Symbol::Minus)
            | Token::Symbol(Symbol::Asterisk)
            | Token::Symbol(Symbol::FwdSlash),
        ) => on_math_expression(tokens, t.unwrap(), previous.unwrap()).unwrap(),

        // Operations
        // Function call if value literal or string
        Some(Token::Literal(l)) => on_function_call(tokens, &ident, l),
        Some(Token::Symbol(Symbol::ParenOpen)) => on_function(tokens, ident),
        None => {
            // TODO: Invalid syntax
            todo!()
        }
        _ => Expression::SingleValue(SingleValue::Identifier(ident)),
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
    operation: &Token,
    previous: Expression,
) -> Option<Expression>
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let prev = previous.clone();
    let op = MathOperation::from_token(operation).unwrap();

    let rhs = on_expression(tokens.next()?, tokens, Some(prev))?;
    Some(Expression::Operation {
        lhs: Box::new(previous),
        rhs: Box::new(rhs),
        operation: Operation::Math(op),
    })
}

fn on_expression<'a, I>(
    t: &Token,
    tokens: &mut I,
    previous: Option<Expression>,
) -> Option<Expression>
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    println!("Token: {:?}", t);
    let expr = match t {
        Token::Ident(ident) => on_identifier(
            tokens,
            ident.to_string(),
            Some(SingleValue::new_identifier_expression(ident)),
        ),
        Token::Kwd(k) => on_keyword(tokens, k),
        Token::Literal(l) => Expression::SingleValue(l.to_single_value()),

        Token::Symbol(Symbol::Plus)
        | Token::Symbol(Symbol::Minus)
        | Token::Symbol(Symbol::Asterisk)
        | Token::Symbol(Symbol::FwdSlash) => on_math_expression(tokens, t, previous?)?,

        _ => todo!(),
    };
    Some(expr)
}

fn on_keyword<'a, I>(tokens: &mut I, k: &Kwd) -> Expression
where
    I: std::iter::Iterator<Item = &'a Token>,
{
    let remaining_expression = on_expression(tokens.next().unwrap(), tokens, None);
    match k {
        Kwd::Return => Expression::Statement {
            expression: Box::new(remaining_expression.unwrap()),
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
    let mut iter = tokens.iter();
    on_expression(iter.next().unwrap(), &mut iter, None).unwrap()
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
