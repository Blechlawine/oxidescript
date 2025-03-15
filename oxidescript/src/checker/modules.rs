use crate::parser::ast::{Module, Use};

use super::Check;

impl Check for Module {
    fn check(&self, ctx: &super::CheckContext) -> super::VariableType {
        todo!()
    }
}

impl Check for Use {
    fn check(&self, ctx: &super::CheckContext) -> super::VariableType {
        todo!()
    }
}
