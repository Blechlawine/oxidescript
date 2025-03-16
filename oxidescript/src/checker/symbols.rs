use std::{
    fmt::Display,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::parser::ast::{Identifier, Program, StructDecl};

use super::{type_checker::VariableType, Scope};

#[derive(Ord, PartialEq, PartialOrd, Eq, Debug, Clone, Hash, Copy)]
pub struct SymbolId(pub usize);

impl Display for SymbolId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

impl SymbolId {
    pub fn new() -> Self {
        SymbolId(get_new_symbol_id())
    }
}

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

static SYMBOL_ID_COUNTER: AtomicUsize = AtomicUsize::new(0);
pub fn get_new_symbol_id() -> usize {
    SYMBOL_ID_COUNTER.fetch_add(1, Ordering::SeqCst)
}
