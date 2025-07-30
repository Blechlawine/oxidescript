use std::collections::HashMap;

use similar::TextDiff;

use crate::{
    checker::type_checker::VariableType,
    parser::ast::{Identifier, Program},
};

#[derive(Default)]
pub struct ParentScopes {
    scopes: HashMap<Scope, Option<Scope>>,
    scope_counter: u64,
}

impl ParentScopes {
    pub fn global_scope(&self) -> Scope {
        Scope(0)
    }

    pub fn new_scope(&mut self) -> Scope {
        let scope = Scope(self.scope_counter);
        self.scope_counter += 1;
        scope
    }

    pub fn new_scope_with_parent(&mut self, parent: Scope) -> Scope {
        let scope = self.new_scope();
        self.scopes.insert(scope, Some(parent));
        scope
    }

    pub fn build_scope_tree(&self, scope: Scope) -> Vec<Scope> {
        let mut output = vec![scope];
        let mut current = scope;
        while let Some(Some(parent)) = self.scopes.get(&current) {
            current = *parent;
            output.push(current);
        }
        output
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Scope(u64);

#[derive(Default)]
pub struct SymbolTable {
    symbols: HashMap<(Identifier, Scope), Symbol>,
    symbol_counter: u64,
}

impl SymbolTable {
    pub fn insert(&mut self, symbol: Symbol) {
        self.symbols
            .insert((symbol.identifier.clone(), symbol.scope), symbol);
    }

    pub fn next_symbol_id(&mut self) -> SymbolId {
        let id = self.symbol_counter;
        self.symbol_counter += 1;
        SymbolId(id)
    }

    pub fn get(&self, identifier: Identifier, scope: Scope) -> Option<&Symbol> {
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
            let diff = TextDiff::from_chars(&identifier.name, &available.identifier.name);
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
pub struct SymbolId(u64);

pub struct Symbol {
    id: SymbolId,
    identifier: Identifier,
    scope: Scope,
    r#type: Option<VariableType>,
}

#[derive(Default)]
pub struct Resolver {
    scopes: ParentScopes,
    symbols: SymbolTable,
}

impl Resolver {
    pub fn resolve_program(source: Program) -> Self {
        let mut resolver = Self::default();
        let scope = resolver.scopes.global_scope();
        // TODO: resolve
        resolver
    }
}
