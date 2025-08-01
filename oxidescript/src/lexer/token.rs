#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token<'src> {
    Illegal,
    EOF,

    Ident(&'src str),
    // Literals
    StringLiteral(&'src str),
    NumberLiteral(&'src str),
    BooleanLiteral(bool),

    // Operators
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    LogicalNot,
    BitwiseNot,
    LogicalAnd,
    LogicalOr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Assign,

    // Punctuation
    Period,
    Comma,
    Colon,
    SemiColon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    LBracket,
    RBracket,

    // Keywords
    Const,
    Let,
    Function,
    Return,
    If,
    Else,
    While,
    For,
    In,
    Break,
    Continue,
    Struct,
    Enum,
    Type,
    Trait,
    Impl,
    Use,
    Pub,
    Mod,
    Extern,
}
