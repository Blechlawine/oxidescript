use crate::parser::ast::{Program, Statement};

use super::{Check, CheckContext, VariableType};

impl Check for Program {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        for statement in self {
            statement.check(ctx);
        }
        VariableType::Void
    }
}

impl Check for Statement {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        match self {
            Statement::ExpressionStatement { expression, .. } => expression.check(ctx),
            Statement::DeclarationStatement(declaration) => declaration.check(ctx),
        };
        VariableType::Void
    }
}
