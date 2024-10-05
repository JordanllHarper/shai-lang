use crate::language::*;
use crate::lexer::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Node {
    NativeType(NativeType),
    Expression(Expression),
    Operation(Expression),
    Function(Function),
    Assignment(Assignment),
}

/// Advances `tokens` till it finds a token that is not contained in `test`.
fn advance_past_multiple<'a, I>(tokens: &mut I, test: &[Token]) -> Option<&'a Token>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    tokens.find(|t| {
        for t_token in test {
            if *t != t_token {
                return true;
            }
        }
        false
    })
}

fn advance_past<'a, I>(tokens: &mut I, test: Token) -> Option<&'a Token>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    tokens.find(|t| **t != test)
}

fn advance_past_whitespace<'a, I>(tokens: &mut I) -> Option<&'a Token>
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    advance_past(tokens, Token::Symbol(Symbol::Whitespace))
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

    Expression::SingleValue(num)
}

/// TODO: Handle various types of operations, not just function calls
fn on_operation<'a, I>(tokens: &mut I, ident: String, expression: Expression) -> Node
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    // build operation
    Node::Operation(Expression::Operation {
        lhs: Box::new(Expression::SingleValue(ValueLiteral::new(
            NativeType::Function,
            &ident,
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
// Function call [ ]
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
    // TODO: Function call e.g. print y
    // TODO: Expression (empty statement) e.g. x + y
    // TODO: Variable Usage e.g. y = x + 3
    match advance_past_whitespace(tokens) {
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
        // Function if value literal (arg)
        Some(Token::Literal(l)) => {
            let vl = to_vl(l);
            on_operation(tokens, ident, Expression::SingleValue(vl))
        }
        // Function if quote
        // E.g. case print "Hello"
        Some(Token::Symbol(Symbol::Quote)) => {
            let str_literal = ValueLiteral::new(
                NativeType::String,
                &tokens
                    .take_while(|t: &&Token| **t != Token::Symbol(Symbol::Quote))
                    .fold(String::new(), |acc, e| acc + &e.to_string()),
            );
            on_operation(tokens, ident, Expression::SingleValue(str_literal))
        }

        _ => todo!(),
    }
}

fn to_vl(l: &Literal) -> ValueLiteral {
    match l.clone() {
        Literal::BoolLiteral(b) => ValueLiteral::new(NativeType::Bool, &b.to_string()),
        Literal::IntLiteral(i) => ValueLiteral::new(NativeType::Int, &i.to_string()),
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
    use crate::parser::*;

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
                lhs: Box::new(Expression::SingleValue(ValueLiteral::new(
                    NativeType::Function,
                    "print",
                ))),
                rhs: Box::new(Expression::SingleValue(ValueLiteral::new(
                    NativeType::String,
                    "Hello",
                ))),
                operation: Operation::FunctionCall,
            }),
        );
    }

    #[test]
    fn literal_assignment() {
        // Literal input => "x=5"
        let expected = Node::Assignment(Assignment::new(
            "x",
            Expression::SingleValue(ValueLiteral::new(NativeType::Int, "5")),
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
        // Literal input => "x = 5"
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
                Expression::SingleValue(ValueLiteral::new(NativeType::Int, "5")),
            )),
        );
    }
}
