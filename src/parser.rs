use crate::language::*;
use crate::lexer::*;
use crate::parser_helpers::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    NativeType(NativeType),
    Expression(Expression),
    Operation(Expression),
    Function(Function),
    Assignment(Assignment),
}

fn parse_floating_point<'a, I>(tokens: &mut I, before_decimal: i32) -> ValueLiteral
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    let after_decimal = tokens.next();
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
fn handle_integer_literal<'a, I>(tokens: &mut I, i: i32) -> Expression
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    let num = if let Some(Token::Symbol(Symbol::Period)) = advance_past_whitespace(tokens) {
        parse_floating_point(tokens, i)
    } else {
        ValueLiteral::new(NativeType::Int, &i.to_string())
    };

    Expression::SingleValue(SingleValue::ValueLiteral(num))
}

/// TODO: Handle various types of operations, not just function calls
fn on_operation<'a, I>(tokens: &mut I, ident: String, expression: Expression) -> Node
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    Node::Operation(Expression::Operation {
        lhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
            ValueLiteral::new(NativeType::Function, &ident),
        ))),
        rhs: Box::new(expression),
        operation: Operation::FunctionCall,
    })
}

fn on_evaluation<'a, I>(tokens: &mut I, ident: String, evaluation_op: EvaluationOperator) -> Node
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    todo!();
}

fn on_assignment<'a, I>(tokens: &mut I, ident: String) -> Node
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    let current = advance_past_whitespace(tokens);
    match current {
        Some(Token::Literal(Literal::IntLiteral(x))) => {
            let num = handle_integer_literal(tokens, *x);
            let expr = match advance_past_whitespace(tokens) {
                Some(Token::Symbol(Symbol::Newline)) | None => num,
                // This is not the end of the assignment woooooo...
                // TODO: Expression operation assignment
                Some(t) => todo!(),
            };
            let assign = Assignment::new(&ident, expr);
            Node::Assignment(assign)
        }
        Some(Token::Symbol(Symbol::BraceOpen)) => {
            // TODO: Expression assignment
            let expression = on_expression(tokens);
            todo!()
        }
        _ => todo!(),
    }
    // TODO: Function assignment
    // E.g. fname (...) = ...
}

// Identifier options
//
// Function assignment [ ]
// add (...) { ... }
//
// Function call [x]
// print "Hello World"
//
// Variable assignment [x]
// x = 3
//
// Variable usage
// y = *x* + 3
//      |- here is usage
fn on_identifier<'a, I>(tokens: &mut I, ident: String) -> Node
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    // TODO: Function call e.g. print "Hello, World" [x]
    // TODO: Function call with multiple parameters e.g. print "Hello" "World"
    // TODO: Function call with identifier e.g. print y
    // TODO: Expression (empty statement) e.g. x + y
    // TODO: Variable Usage e.g. y = x + 3
    let t = advance_past_whitespace(tokens);
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

        // Operations
        // Function call if value literal or string
        Some(Token::Literal(_)) | Some(Token::Symbol(Symbol::Quote)) => {
            let args = parse_arguments(tokens, t.expect("This isn't None at this point"))
                .expect("If this is None, aaaaaaaaaa idk");
            let expression = Expression::MultipleValues(args);
            on_operation(tokens, ident, expression)
        }
        _ => todo!(),
    }
}

fn on_expression<'a, I>(tokens: &mut I) -> Option<Node>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    match tokens.next().to_owned()? {
        Token::Ident(ident) => {
            let node = on_identifier(tokens, ident.to_string());
            Some(node)
        }
        _ => {
            todo!()
        }
    }
}

pub fn parse(tokens: &[Token]) -> Option<Node> {
    on_expression(&mut tokens.iter())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn test(input: Vec<Token>, expected: Node) {
        let actual = parse(&input).unwrap();
        assert_eq!(expected, actual);
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
            Node::Operation(Expression::Operation {
                lhs: Box::new(Expression::SingleValue(SingleValue::ValueLiteral(
                    ValueLiteral::new(NativeType::Function, "print"),
                ))),
                rhs: Box::new(Expression::MultipleValues(vec![
                    Expression::SingleValue(
                    SingleValue::ValueLiteral(
                    ValueLiteral::new(NativeType::String, "Hello"),
                ))

                ])),
                operation: Operation::FunctionCall,
            }),
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
            Node::Operation(Expression::Operation {
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
            }),
        );
    }

    #[test]
    fn function_call_multiple_values_with_identifier() {
        // print "Hello" true
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
            Node::Operation(Expression::Operation {
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
            }),
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
            Node::Operation(Expression::Operation {
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
            }),
        );
    }

    #[test]
    fn literal_assignment() {
        // "x=5"
        let expected = Node::Assignment(Assignment::new(
            "x",
            Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new(
                NativeType::Int,
                "5",
            ))),
        ));
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
            Node::Assignment(Assignment::new(
                "x",
                Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new(
                    NativeType::Int,
                    "5",
                ))),
            )),
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
            Node::Assignment(Assignment::new(
                "x",
                Expression::SingleValue(SingleValue::ValueLiteral(ValueLiteral::new(
                    NativeType::Int,
                    "5",
                ))),
            )),
        );
    }
}
