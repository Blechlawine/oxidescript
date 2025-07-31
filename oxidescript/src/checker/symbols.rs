use crate::{
    parser::ast::{Identifier, Program, StructDecl},
    types::VariableType,
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SymbolInner<'src> {
    Module(ModuleSymbol<'src>),
    Type(TypeSymbol<'src>),
    Function(FunctionSymbol),
    Variable(VariableSymbol<'src>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ModuleSymbol<'src> {
    Extern { ast: Program<'src> },
    Intern { ast: Option<Program<'src>> },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionSymbol {
    // ast: Program,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableSymbol<'src> {
    pub r#type: Option<VariableType>,
    pub name: Identifier<'src>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeSymbol<'src> {
    Struct(StructDecl<'src>),
}
