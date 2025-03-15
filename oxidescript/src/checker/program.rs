use crate::parser::ast::{Block, Program, Statement};

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

impl Check for Block {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        for statement in &self.statements {
            statement.check(ctx);
        }
        if let Some(return_value) = &self.return_value {
            return_value.check(ctx)
        } else {
            VariableType::Void
        }
    }
}
