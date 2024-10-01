// Parser Ideas
//
// Iterator API: each call to `.next()` will generate a new state of the AST
//
// Nodes:
// x = 5
// ->
// [
//      ident: x
//      value: ILiteral(5)
//      type:  int
// ]
//
// y = x + 5
// Like
// y = {x + 5}
//
// [
//      ident: y
//      value: EXPR [
//          lhs: x,
//          op: add
//          rhs: ILiteral(5)
//      ]
//      type:  int
// ]


use crate::lexer::Token;


#[derive(Debug)]
enum ExprIdent{
    Anonymous,
    Known(String)
}


#[derive(Debug)]
struct Node {
    ident: ExprIdent,
    // TODO: Figure out value
    value : Token,
}

#[derive(Debug)]
struct Parser {
    tokens         : Vec<Token>,
    // The internal parse state of the current generated AST.
    // This will be returned after every call to .next(), 
    // representing another parsing step.
    internal_tree  : Box<Node>
}

