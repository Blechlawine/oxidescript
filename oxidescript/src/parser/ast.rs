pub type Program = Vec<Statement>;

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Statement {
    // TODO: do we have expression statements in this language?
    ExpressionStatement(Expression),
    DeclarationStatement(Declaration),
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expression {
    IdentifierExpression(Identifier),
    LiteralExpression(Literal),
    UnaryExpression(UnaryOperator, Box<Expression>),
    InfixExpression(InfixOperator, Box<Expression>, Box<Expression>),
    ArrayExpression(Vec<Expression>),
    // IfExpression {
    //    condition: Box<Expression>,
    //    then_branch: Box<Expression>,
    //    else_branch: Option<Box<Expression>>,
    //},
    // ForExpression {
    //     // TODO
    // },
    // MatchExpression {
    //     // TODO
    // },
    // BlockExpression(Block),
    CallExpression(Identifier, Vec<Expression>),
    // IndexExpression,
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
    Not,
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
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Identifier(pub String);

#[derive(Clone, Eq, PartialEq, Debug, PartialOrd)]
pub enum Precedence {
    PLowest,
    PEquals,
    PLessGreater,
    PSum,
    PProduct,
    PCall,
    PIndex,
}
