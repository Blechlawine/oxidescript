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
    UnaryExpression(UnaryOperator, Box<Expression>),
    InfixExpression(InfixOperator, Box<Expression>, Box<Expression>),
    ArrayExpression(Vec<Expression>),
    IfExpression {
        condition: Box<Expression>,
        then_block: Box<Block>,
        else_if_blocks: Vec<(Expression, Block)>,
        else_block: Option<Box<Block>>,
    },
    // ForExpression {
    //     // TODO
    // },
    // MatchExpression {
    //     // TODO
    // },
    BlockExpression(Box<Block>),
    CallExpression(Box<Expression>, Vec<Expression>),
    IndexExpression(Box<Expression>, Box<Expression>),
    MemberAccessExpression(Box<Expression>, Identifier),
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
    NumberLiteral(String),
    BooleanLiteral(bool),
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
