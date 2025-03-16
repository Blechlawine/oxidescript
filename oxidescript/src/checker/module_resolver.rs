use super::{
    symbols::{ModuleSymbol, SymbolId, SymbolInner},
    AstNode, Scope, SymbolTable,
};

pub struct SymbolsResolver<'s> {
    symbol_table: &'s mut SymbolTable,
    current_scope: Option<&'s Scope>,
}

impl<'s> SymbolsResolver<'s> {
    pub fn new(symbol_table: &'s mut SymbolTable) -> Self {
        Self {
            symbol_table,
            current_scope: None,
        }
    }

    pub fn start_resolving(&'s mut self, symbol_id: SymbolId) {
        let resolved = self.symbol_table.symbols.get(&symbol_id);
        if let Some(resolved) = resolved {
            self.current_scope = Some(&resolved.scope);
            // match &resolved.inner {
            //     SymbolInner::Module(module_symbol) => match module_symbol {
            //         ModuleSymbol::Extern { ast } => ast.visit(self),
            //         ModuleSymbol::Intern { ast } => {
            //             if let Some(ast) = ast {
            //                 ast.visit(self);
            //             } else {
            //                 todo!()
            //             }
            //         }
            //     },
            //     SymbolInner::Type(type_symbol) => todo!(),
            //     SymbolInner::Function(function_symbol) => todo!(),
            //     SymbolInner::Variable(variable_symbol) => todo!(),
            // }
        }
    }
}
