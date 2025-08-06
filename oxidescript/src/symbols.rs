use std::collections::HashMap;

use similar::TextDiff;

use crate::{parser::ast::Identifier, scope::Scope, types::VariableType};

#[derive(Default)]
pub struct SymbolTable<'src> {
    symbols: HashMap<(Identifier<'src>, Scope), Symbol<'src>>,
    symbol_counter: usize,
}

impl<'src> SymbolTable<'src> {
    pub fn insert(&mut self, symbol: Symbol<'src>) {
        self.symbols
            .insert((symbol.identifier.clone(), symbol.scope), symbol);
    }

    pub fn next_symbol_id(&mut self) -> SymbolId {
        let id = self.symbol_counter;
        self.symbol_counter += 1;
        SymbolId(id)
    }

    pub fn get(&self, identifier: Identifier<'src>, scope: Scope) -> Option<&Symbol> {
        self.symbols.get(&(identifier, scope))
    }

    pub fn get_by_id(&self, id: SymbolId) -> Option<&Symbol> {
        self.symbols.values().find(|symbol| symbol.id == id)
    }

    fn get_defined_symbols(&self, scopes: &[Scope]) -> Vec<&Symbol> {
        let mut output: Vec<&Symbol> = vec![];
        for scope in scopes {
            let scope_symbols = self
                .symbols
                .values()
                .filter(|symbol| symbol.scope == *scope);
            for symbol in scope_symbols {
                if output.iter().any(|s| s.id == symbol.id) {
                    continue;
                }
                output.push(symbol);
            }
        }
        output
    }

    pub fn find_similar_symbols(
        &self,
        identifier: Identifier,
        scope_tree: &[Scope],
    ) -> Option<&Symbol> {
        let mut most_similar = None;
        let mut ratio = 0.0;
        for &available in self.get_defined_symbols(scope_tree).iter() {
            let diff = TextDiff::from_chars(identifier.name, &available.identifier.name);
            let similarity = diff.ratio();
            if similarity > ratio {
                most_similar = Some(available);
                ratio = similarity;
            }
        }
        most_similar
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

pub struct Symbol<'src> {
    pub id: SymbolId,
    pub symbol_type: SymbolType,
    pub identifier: Identifier<'src>,
    pub scope: Scope,
    pub r#type: Option<VariableType>,
}

pub enum SymbolType {
    Value,
    Type,
    Module,
}
