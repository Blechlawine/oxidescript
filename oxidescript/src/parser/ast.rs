use std::{fmt::Display, num::ParseFloatError};

use crate::checker::VariableType;

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
    ForExpression(ForExpr),
    // MatchExpression(MatchExpr), // TODO
    BlockExpression(Box<Block>),
    CallExpression(CallExpr),
    IndexExpression(IndexExpr),
    MemberAccessExpression(MemberAccessExpr),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ForExpr {
    pub lhs: Identifier,
    pub rhs: Box<Expression>,
    pub body: Box<Block>,
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
    StructDeclaration(StructDecl),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct StructDecl {
    pub ident: Identifier,
    pub fields: Vec<StructField>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct StructField {
    pub ident: Identifier,
    pub r#type: TypeExpression,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TypeExpression {
    Ident(Identifier),
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
    pub r#type: TypeExpression,
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

impl Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOperator::Equal => f.write_str("=="),
            InfixOperator::NotEqual => f.write_str("!="),
            InfixOperator::GreaterThan => f.write_str(">"),
            InfixOperator::LessThan => f.write_str("<"),
            InfixOperator::GreaterThanEqual => f.write_str(">="),
            InfixOperator::LessThanEqual => f.write_str("<="),
            InfixOperator::Plus => f.write_str("+"),
            InfixOperator::Minus => f.write_str("-"),
            InfixOperator::Multiply => f.write_str("*"),
            InfixOperator::Divide => f.write_str("/"),
            InfixOperator::Modulo => f.write_str("%"),
            InfixOperator::LogicalOr => f.write_str("||"),
            InfixOperator::LogicalAnd => f.write_str("&&"),
            InfixOperator::BitwiseOr => f.write_str("|"),
            InfixOperator::BitwiseXor => f.write_str("^"),
            InfixOperator::BitwiseAnd => f.write_str("&"),
            InfixOperator::BitwiseLeftShift => f.write_str("<<"),
            InfixOperator::BitwiseRightShift => f.write_str(">>"),
        }
    }
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
