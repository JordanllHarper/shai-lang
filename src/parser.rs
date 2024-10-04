use crate::language::*;
use crate::lexer::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
    NativeType(NativeType),
    Expression(Expression),
    Function(Function),
    Assignment(Assignment),
}

fn handle_integer_literal(i: i32) -> Expression {
    // TODO: Handle floating point nums and operations
    // E.g.
    // 3_+_4
    // or
    // 3.14
    Expression::SingleValue(ValueLiteral::new(NativeType::Int, &i.to_string()))
}

fn on_assignment<'a, I>(tokens: &mut I, ident: String) -> Node 
where
    I: IntoIterator<Item = &'a Token> + std::iter::Iterator<Item = &'a Token>,
{
    if let Some(Token::Literal(Literal::IntLiteral(x))) = tokens.find(|t| **t != Token::Symbol(Symbol::Whitespace)) {
        // TODO: This could still be a float or a calculation
        let expr = handle_integer_literal(*x);
        let assign = Assignment::new(&ident, expr);
        let node = Node::Assignment(assign);
        return node;
    }
    // TODO: Function assignment
    // TODO: Expression assignment
    todo!()

    // SingleValue(ValueLiteral),
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
    if tokens.find(|t| **t != Token::Symbol(Symbol::Whitespace)) == Some(&Token::Symbol(Symbol::Equals)) {
        return on_assignment(tokens, ident);
    }
    // TODO: Function call e.g. print "Hello, World"
    // TODO: Expression (empty statement) e.g. x + y 
    // TODO: Variable Usage e.g. y = x + 3
    todo!()
}

pub fn parse(tokens: &[Token]) -> Option<Node> {
    // TODO: Review filtering all whitespace, some might be necessary (like in strings)
    let mut token_iter = tokens.iter();

    if let Token::Ident(ident) = token_iter.next().to_owned()? {
        let node = on_identifier(&mut token_iter, ident.to_string());
        return Some(node);
    }
    println!("Didn't evaluate as ident");
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn assignment_tree() {
        // input = "x=5"
        //
        // -> lexer transforms
        // ->
        let tokens = vec![
            Token::Ident("x".to_string()),
            Token::Symbol(Symbol::Equals),
            Token::Literal(Literal::IntLiteral(5)),
        ];

        let expected = Node::Assignment(Assignment::new(
            "x",
            Expression::SingleValue(ValueLiteral::new(NativeType::Int, "5")),
        ));

        let actual = parse(&tokens).unwrap();

        assert_eq!(expected, actual);
        // ----

        // input = "x = 5"
        let tokens = vec![
            Token::Ident("x".to_string()),
            Token::Symbol(Symbol::Whitespace),
            Token::Symbol(Symbol::Equals),
            Token::Symbol(Symbol::Whitespace),
            Token::Literal(Literal::IntLiteral(5)),
        ];

        let actual = parse(&tokens).unwrap();

        assert_eq!(expected, actual);
    }
}
