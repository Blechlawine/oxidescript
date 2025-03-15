use std::cell::RefCell;

use crate::{
    checker::Scope,
    parser::ast::{Module, Use},
};

use super::{Check, CheckContext};

impl Check for Module {
    fn check(&self, ctx: &CheckContext) -> super::VariableType {
        if let Some(content) = &self.content {
            // this ctx does not need to call .init, becaus it inherits the resolved tree from the
            // passed in ctx
            ctx.insert_declaration(self.path.clone(), crate::checker::Resolved::Module);
            let new_ctx = CheckContext {
                scope: RefCell::new(Scope::default()),
                errors: ctx.errors,
                resolved: ctx.resolved,
                current_resolved_path: self.path.clone(),
            };
            content.check(&new_ctx)
        } else {
            todo!()
        }
    }
}

impl Check for Use {
    fn check(&self, ctx: &CheckContext) -> super::VariableType {
        todo!()
    }
}
