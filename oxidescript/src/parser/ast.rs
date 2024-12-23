use std::{fmt::Display, num::ParseFloatError};

pub type Program = Vec<Statement>;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Statement {
    ExpressionStatement {
        expression: Expression,
        has_semicolon: bool,
    },
    DeclarationStatement(Declaration),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expression {
    IdentifierExpression(Identifier),
    LiteralExpression(Literal),
    UnaryExpression(UnaryExpr),
    InfixExpression(InfixExpr),
    ArrayExpression(Vec<Expression>),
    IfExpression(IfExpr),
    // ForExpression {
    //     // TODO
    // },
    // MatchExpression {
    //     // TODO
    // },
    BlockExpression(Box<Block>),
    CallExpression(CallExpr),
    IndexExpression(IndexExpr),
    MemberAccessExpression(MemberAccessExpr),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct MemberAccessExpr {
    pub lhs: Box<Expression>,
    pub ident: Identifier,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IndexExpr {
    pub lhs: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CallExpr {
    pub lhs: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct UnaryExpr {
    pub op: UnaryOperator,
    pub rhs: Box<Expression>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct InfixExpr {
    pub op: InfixOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Declaration {
    ConstDeclaration(Identifier, Expression),
    LetDeclaration(Identifier, Expression),
    FunctionDeclaration {
        name: Identifier,
        parameters: Vec<Parameter>,
        body: Block,
    },
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IfExpr {
    pub condition: Box<Expression>,
    pub then_block: Box<Block>,
    pub else_if_blocks: Vec<ElseIfExpr>,
    pub else_block: Option<Box<Block>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ElseIfExpr {
    pub condition: Box<Expression>,
    pub then_block: Block,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_value: Option<Expression>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Parameter {
    pub name: Identifier,
    pub type_: Identifier,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Literal {
    StringLiteral(String),
    NumberLiteral(Number),
    BooleanLiteral(bool),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Number {
    F(String),
    I { base: NumberBase, value: i32 },
}
impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::F(s) => f.write_str(s),
            Number::I { base, value } => match base {
                NumberBase::Bin => f.write_str(&format!("{:#b}", value)),
                NumberBase::Hex => f.write_str(&format!("{:#x}", value)),
                NumberBase::Oct => f.write_str(&format!("{:#o}", value)),
                NumberBase::Dec => value.fmt(f),
            },
        }
    }
}
impl TryFrom<Number> for f64 {
    type Error = ParseFloatError;

    fn try_from(value: Number) -> Result<Self, Self::Error> {
        match value {
            Number::F(s) => s.parse(),
            Number::I { value, .. } => Ok(value as f64),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum NumberBase {
    Bin,
    Hex,
    Oct,
    Dec,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum UnaryOperator {
    LogicalNot,
    BitwiseNot,
    Minus,
    Plus,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum InfixOperator {
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    BitwiseLeftShift,
    BitwiseRightShift,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Identifier(pub String);

#[derive(Clone, Eq, PartialEq, Debug, PartialOrd)]
pub enum Precedence {
    PLowest,
    PLogicalOr,    // ||
    PLogicalAnd,   // &&
    PBitwiseOr,    // |
    PBitwiseXor,   // ^
    PBitwiseAnd,   // &
    PEquals,       // ==, !=
    PLessGreater,  // >, <, >=, <=
    PBitwiseShift, // <<, >>
    PSum,          // +, -
    PProduct,      // *, /, %
    PCall,         // function call e.g. foo()
    PMemberAccess, // e.g. foo.bar
    PIndex,        // e.g. foo[0]
}
