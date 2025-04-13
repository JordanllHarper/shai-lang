use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Kwd(Kwd),
    Ident(String),
    Symbol(Symbol),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(i32),
    Float(f64),
    String(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OpSymbol {
    Minus,
    Plus,
    Asterisk,
    FwdSlash,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EvaluationSymbol {
    Equality,
    NotEquality,
    LzEq,
    GzEq,
    Lz,
    Gz,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Symbol {
    ParenOpen,
    ParenClose,
    ChevOpen,
    ChevClose,
    AngOpen,
    AngClose,
    BraceOpen,
    BraceClose,
    Equals,
    Modulus,
    Quote,
    Apstr,
    Comma,
    Period,
    // operation symbols
    Op(OpSymbol),
    //
    // comparison
    Evaluation(EvaluationSymbol),
    //
    BckSlash,
    Dollar,
    Colon,
    Underscore,
    Ampsnd,
    Pipe,
    Whitespace,
    Bang,
    Newline,
    // 2 char symbols
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    Arrow,
    And,
    Or,
    MultilineComment(String),
    Comment(String),
    // Escape chars
    EscapeQuote,
    EscapeApos,
    Range,
    RangeEq,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DataTypeKwd {
    Bool,
    Float,
    Int,
    String,
    Void,
    Dict,
    Arr,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Kwd {
    DataType(DataTypeKwd),
    While,
    For,
    If,
    Return,
    In,
    Break,
    Include,
    Else,
    Const,
}

impl Token {
    pub fn whitespace() -> Token {
        Token::Symbol(Symbol::Whitespace)
    }
    pub fn new_ident(str: &str) -> Token {
        Token::Ident(str.to_string())
    }
}

impl Display for DataTypeKwd {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let representation = match self {
            DataTypeKwd::Bool => "bool",
            DataTypeKwd::Float => "float",
            DataTypeKwd::Int => "int",
            DataTypeKwd::String => "string",
            DataTypeKwd::Dict => "dict",
            DataTypeKwd::Arr => "arr",
            DataTypeKwd::Void => "void",
        };
        f.write_str(representation)
    }
}

impl Display for Kwd {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Kwd::DataType(d) => &d.to_string(),
            Kwd::While => "while",
            Kwd::For => "for",
            Kwd::If => "if",
            Kwd::Return => "return",
            Kwd::In => "in",
            Kwd::Break => "break",
            Kwd::Include => "include",
            Kwd::Else => "else",
            Kwd::Const => "const",
        };
        f.write_str(str)
    }
}

impl Display for EvaluationSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            EvaluationSymbol::Equality => "==",
            EvaluationSymbol::NotEquality => "!=",
            EvaluationSymbol::LzEq => "<=",
            EvaluationSymbol::GzEq => ">=",
            EvaluationSymbol::Lz => "<",
            EvaluationSymbol::Gz => ">",
        };
        f.write_str(str)
    }
}
impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Symbol::ParenOpen => "(",
            Symbol::ParenClose => ")",
            Symbol::ChevOpen => "<",
            Symbol::ChevClose => ">",
            Symbol::AngOpen => "[",
            Symbol::AngClose => "]",
            Symbol::BraceOpen => "{",
            Symbol::BraceClose => "}",
            Symbol::Equals => "=",
            Symbol::Modulus => "%",
            Symbol::Quote => "\"",
            Symbol::Apstr => "\'",
            Symbol::Comma => ",",
            Symbol::Period => ".",
            Symbol::BckSlash => "\\",
            Symbol::Dollar => "$",
            Symbol::Colon => ":",
            Symbol::Underscore => "_",
            Symbol::Ampsnd => "&",
            Symbol::Pipe => "|",
            Symbol::Whitespace => " ",
            Symbol::Bang => "!",
            Symbol::Newline => "\n",
            Symbol::PlusAssign => "+=",
            Symbol::MinusAssign => "-=",
            Symbol::MultiplyAssign => "*=",
            Symbol::DivideAssign => "/=",
            Symbol::Arrow => "->",
            Symbol::And => "&&",
            Symbol::Or => "||",
            Symbol::Evaluation(e) => &e.to_string(),
            Symbol::EscapeQuote => "\\\"",
            Symbol::EscapeApos => "\\\'",
            Symbol::Comment(c) => c,
            Symbol::MultilineComment(c) => c,
            Symbol::Op(s) => &s.to_string(),
            Symbol::Range => "..",
            Symbol::RangeEq => "..=",
        };
        f.write_str(str)
    }
}

impl Display for OpSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            OpSymbol::Minus => "-",
            OpSymbol::Plus => "+",
            OpSymbol::Asterisk => "*",
            OpSymbol::FwdSlash => "/",
        };

        f.write_str(str)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str_rep = match self {
            Token::Kwd(k) => &k.to_string(),
            Token::Ident(i) => i,
            Token::Symbol(s) => &s.to_string(),
            Token::Literal(l) => &l.to_string(),
        };
        f.write_str(str_rep)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str_rep = match self {
            Literal::Bool(b) => b.to_string(),
            Literal::Int(i) => i.to_string(),
            Literal::String(s) => s.to_string(),
            Literal::Float(f) => f.to_string(),
        };
        f.write_str(&str_rep)
    }
}
