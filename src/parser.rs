use crate::language::*;
use crate::lexer::Token;

/// The model which holds the generated AST.
///
/// Implements the [Iterator] trait to provide an updated 'state' of the AST on each call to
/// .next()
#[derive(Debug)]
pub struct Parser {
    position: usize,
    tokens: Vec<Token>,
    // The internal parse state of the current generated AST.
    // This will be returned after every call to .next(),
    // representing another parsing step.
    tree_state: Box<Node>,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        let tree_state = Node::new_base_node();
        Self {
            tokens: tokens.to_vec(),
            tree_state,
            position: 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
    Base(Option<Box<Node>>),
    NativeType(NativeType),
    Expression(Expression),
    Function(Function),
    Assignment(Assignment),
}

impl Node {
    fn new_base_node() -> Box<Node> {
        Box::new(Node::Base(None))
    }
}

impl Iterator for Parser {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        let current = &self.tokens[self.position];

        self.position += 1;
        todo!()
    }
    // add code here
}

#[cfg(test)]
mod tests {
    use crate::language::{Assignment, ValueLiteral};
    use crate::lexer::*;
    use crate::parser::{Node, Parser};

    #[test]
    fn assignment_tree() {
        // let input = "x = 5"
        //
        // -> lexer transforms
        // ->
        let tokens = vec![
            Token::Ident("x".to_string()),
            Token::Symbol(Symbol::Whitespace),
            Token::Symbol(Symbol::Equals),
            Token::Symbol(Symbol::Whitespace),
            Token::Literal(Literal::IntLiteral(5)),
        ];

        let expected = Node::Base(Some(Box::new(Node::Assignment(
            Assignment::new(
                "x",
                crate::language::Expression::SingleValue(ValueLiteral::new(
                    crate::language::NativeType::Int,
                    "5",
                )),
            ),
        ))));
        let actual = Parser::new(&tokens).last().unwrap();

        assert_eq!(expected, actual);
        todo!();
    }
}
