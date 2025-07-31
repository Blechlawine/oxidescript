use std::{fmt::Display, num::ParseFloatError};

use crate::symbols::SymbolId;

pub type Program<'src> = Vec<Statement<'src>>;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum ModuleDeclaration<'src> {
    Extern {
        path: Path<'src>,
        content: Program<'src>,
    },
    Intern {
        path: Path<'src>,
        /// this is None for modules in separate files, only being populated with semantic analysis
        content: Option<Program<'src>>,
    },
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Path<'src> {
    pub elements: Vec<IdentifierReference<'src>>,
    /// this path is populated in semantic analysis
    pub full_path: Option<Vec<Identifier<'src>>>,
}

impl<'src> Path<'src> {
    pub fn new(elements: Vec<IdentifierReference<'src>>) -> Self {
        Self {
            elements,
            full_path: None,
        }
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }

    pub fn join(&self, other: &Path<'src>) -> Path<'src> {
        let mut elements = self.elements.clone();
        elements.extend(other.elements.clone());
        Path {
            elements,
            full_path: None,
        }
    }
}

impl Display for Path<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for element in &self.elements {
            f.write_str("::")?;
            f.write_str(&element.name)?;
        }
        Ok(())
    }
}

impl<'src> From<IdentifierReference<'src>> for Path<'src> {
    fn from(value: IdentifierReference<'src>) -> Self {
        Self {
            elements: vec![value],
            full_path: None,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Use<'src> {
    pub path: Path<'src>,
    pub resolved_module: Option<Path<'src>>,
    pub imported: Option<Imported<'src>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Imported<'src> {
    Module(IdentifierReference<'src>),
    Name(IdentifierReference<'src>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Statement<'src> {
    ExpressionStatement {
        expression: Expression<'src>,
        has_semicolon: bool,
    },
    DeclarationStatement(Declaration<'src>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expression<'src> {
    PathExpression(Path<'src>),
    LiteralExpression(Literal),
    UnaryExpression(UnaryExpr<'src>),
    InfixExpression(InfixExpr<'src>),
    ArrayExpression(Vec<Expression<'src>>),
    IfExpression(IfExpr<'src>),
    ForExpression(ForExpr<'src>),
    // MatchExpression(MatchExpr), // TODO
    BlockExpression(Box<Block<'src>>),
    CallExpression(CallExpr<'src>),
    IndexExpression(IndexExpr<'src>),
    MemberAccessExpression(MemberAccessExpr<'src>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ForExpr<'src> {
    pub lhs: Identifier<'src>,
    pub rhs: Box<Expression<'src>>,
    pub body: Box<Block<'src>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct MemberAccessExpr<'src> {
    pub lhs: Box<Expression<'src>>,
    pub ident: Identifier<'src>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IndexExpr<'src> {
    pub lhs: Box<Expression<'src>>,
    pub index: Box<Expression<'src>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CallExpr<'src> {
    pub lhs: Box<Expression<'src>>,
    pub arguments: Vec<Expression<'src>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct UnaryExpr<'src> {
    pub op: UnaryOperator,
    pub rhs: Box<Expression<'src>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct InfixExpr<'src> {
    pub op: InfixOperator,
    pub lhs: Box<Expression<'src>>,
    pub rhs: Box<Expression<'src>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Declaration<'src> {
    ConstDeclaration(Identifier<'src>, Expression<'src>),
    LetDeclaration(Identifier<'src>, Expression<'src>),
    FunctionDeclaration(FunctionDecl<'src>),
    StructDeclaration(StructDecl<'src>),
    ModDeclaration(ModuleDeclaration<'src>),
    UseDeclaration(Use<'src>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct FunctionDecl<'src> {
    pub name: Identifier<'src>,
    pub parameters: Vec<Parameter<'src>>,
    pub return_type: Option<TypeExpression<'src>>,
    pub body: Option<Block<'src>>,
    /// if the function actually has logic defined, or if it is just a declaration (fn(arg: ...);)
    pub has_body: bool,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct StructDecl<'src> {
    pub ident: Identifier<'src>,
    pub fields: Vec<StructField<'src>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct StructField<'src> {
    pub ident: Identifier<'src>,
    pub r#type: TypeExpression<'src>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TypeExpression<'src> {
    Path(Path<'src>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct IfExpr<'src> {
    pub condition: Box<Expression<'src>>,
    pub then_block: Box<Block<'src>>,
    pub else_if_blocks: Vec<ElseIfExpr<'src>>,
    pub else_block: Option<Box<Block<'src>>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ElseIfExpr<'src> {
    pub condition: Box<Expression<'src>>,
    pub then_block: Block<'src>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Block<'src> {
    pub statements: Vec<Statement<'src>>,
    pub return_value: Option<Expression<'src>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Parameter<'src> {
    pub name: Identifier<'src>,
    pub r#type: TypeExpression<'src>,
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

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Identifier<'src> {
    pub name: &'src str,
    pub id: Option<SymbolId>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct IdentifierReference<'src> {
    pub name: &'src str,
    pub id: Option<SymbolId>,
}

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
