use std::collections::HashMap;

#[derive(Default)]
pub struct ParentScopes {
    /// a mapping from a scope to the parent scope it is nested in, if its a module scope it
    /// doesn't have a parent
    scopes: HashMap<Scope, Option<Scope>>,
    scope_counter: usize,
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Scope(pub usize);
