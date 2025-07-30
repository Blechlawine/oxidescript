use crate::{
    parser::ast::{Identifier, Program, StructDecl},
    resolver::Scope,
};

use super::type_checker::VariableType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    pub scope: Scope,
    pub inner: SymbolInner,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum SymbolInner {
    Module(ModuleSymbol),
    Type(TypeSymbol),
    Function(FunctionSymbol),
    Variable(VariableSymbol),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ModuleSymbol {
    Extern { ast: Program },
    Intern { ast: Option<Program> },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionSymbol {
    // ast: Program,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableSymbol {
    pub r#type: Option<VariableType>,
    pub name: Identifier,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeSymbol {
    Struct(StructDecl),
}
