use crate::language::*;
use crate::lexer::*;

// /// The model which holds the generated AST.
// #[derive(Debug)]
// pub struct Parser {
//     position: usize,
//     tokens: Vec<Token>,
//     // The internal parse state of the current generated AST.
//     tree_state: Box<Node>,
// }

// impl Parser {
//     pub fn new(tokens: &[Token]) -> Self {
//         let tree_state = Node::new_base_node();
//         Self {
//             tokens: tokens.to_vec(),
//             tree_state,
//             position: 0,
//         }
//     }
// }

#[derive(Debug, PartialEq, Eq)]
pub enum Node {
    NativeType(NativeType),
    Expression(Expression),
    Function(Function),
    Assignment(Assignment),
}


fn handle_integer_literal(i : i32) -> Expression { 
    // TODO: Handle floating point nums and operations
    Expression::SingleValue(ValueLiteral::new(NativeType::Int, &i.to_string()))
}

fn on_assignment(tokens : &[Token], position : usize, ident : String) -> Node { 
    let next_token = &tokens[position];
    println!("Testing {:?}", next_token);
    if let Token::Literal(Literal::IntLiteral(x)) = next_token {
        // this could still be a float
        let expr = handle_integer_literal(*x);
        let assign = Assignment::new(&ident, expr);
        let node = Node::Assignment(assign);
        return node;
    }
    todo!()

    // SingleValue(ValueLiteral),
}

fn on_identifier(tokens : &[Token], position : usize, ident : String) -> Node {
    let next_token = &tokens[position];
    println!("Testing {:?}", next_token);
    if *next_token == Token::Symbol(Symbol::Equals) {
        return on_assignment(tokens, position + 1, ident);
    }
    todo!()
}

pub fn parse(tokens : &[Token], position : usize) -> Option<Node> {
    let tokens = tokens.to_vec();
    let current = &tokens[position];

    println!("Testing {:?}", current);
    if let Token::Ident(i) = current {
        let node = on_identifier(&tokens, position + 1, i.to_string());
        // Following options:
        //
        // Function assignment
        // add (...) { ... }
        //
        // Function call
        // print "Hello World"
        //
        // Variable assignment
        // x = 3
        //
        // Variable usage
        // y = *x* + 3
        //      |- here is usage
        return Some(node);
    }
    println!("Didn't evaluate as ident");
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::language::*;
    use crate::lexer::*;
    use crate::parser::*;

    #[test]
    fn assignment_tree() {
        // let input = "x=5"
        //
        // -> lexer transforms
        // ->
        let tokens = vec![
            Token::Ident("x".to_string()),
            Token::Symbol(Symbol::Equals),
            Token::Literal(Literal::IntLiteral(5)),
        ];

        let expected = Node::Assignment(
            Assignment::new(
                "x",
                Expression::SingleValue(ValueLiteral::new(
                    NativeType::Int,
                    "5",
                )),
            ),
        );
        let actual = parse(&tokens, 0).unwrap();

        assert_eq!(expected, actual);
    }
}
