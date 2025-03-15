use crate::parser::ast::{Declaration, StructDecl, StructField, TypeExpression};

use super::{Check, CheckContext, VariableType};

impl Check for Declaration {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        match self {
            Declaration::ConstDeclaration(identifier, expression) => {
                ctx.scope
                    .borrow_mut()
                    .variables
                    .insert(identifier.0.clone(), expression.check(ctx));
                VariableType::Void
            }
            Declaration::LetDeclaration(identifier, expression) => {
                ctx.scope
                    .borrow_mut()
                    .variables
                    .insert(identifier.0.clone(), expression.check(ctx));
                VariableType::Void
            }
            Declaration::FunctionDeclaration {
                name,
                parameters,
                body,
            } => todo!("function type check"),
            Declaration::StructDeclaration(decl) => decl.check(ctx),
        }
    }
}

impl Check for StructDecl {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        ctx.scope.borrow_mut().types.insert(
            self.ident.0.clone(),
            VariableType::Struct {
                fields: self
                    .fields
                    .iter()
                    .map(|f| (f.ident.0.clone(), f.check(ctx)))
                    .collect(),
            },
        );
        VariableType::Void
    }
}

impl Check for StructField {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        self.r#type.check(ctx);
        VariableType::Void
    }
}

impl Check for TypeExpression {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        match self {
            TypeExpression::Ident(identifier) => ctx.resolve_type(&identifier.0),
        }
    }
}
