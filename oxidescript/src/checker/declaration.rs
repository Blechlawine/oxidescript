use std::borrow::Borrow;

use crate::parser::ast::{Declaration, Parameter, Path, StructDecl, StructField, TypeExpression};

use super::{Check, CheckContext, Resolved, Variable, VariableType};

impl Check for Declaration {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        match self {
            Declaration::ConstDeclaration(identifier, expression) => {
                ctx.scope.borrow_mut().variables.insert(
                    identifier.0.clone(),
                    Variable {
                        r#type: expression.check(ctx),
                    },
                );
                VariableType::Void
            }
            Declaration::LetDeclaration(identifier, expression) => {
                ctx.scope.borrow_mut().variables.insert(
                    identifier.0.clone(),
                    Variable {
                        r#type: expression.check(ctx),
                    },
                );
                VariableType::Void
            }
            Declaration::FunctionDeclaration(decl) => {
                let parameter_types = decl
                    .parameters
                    .iter()
                    .map(|p| p.check(ctx))
                    .collect::<Vec<_>>();
                let expected_return_type = decl
                    .return_type
                    .as_ref()
                    .map(|v| v.check(ctx))
                    .unwrap_or_default();
                ctx.scope.borrow_mut().variables.insert(
                    decl.name.0.clone(),
                    Variable {
                        r#type: VariableType::Function {
                            parameters: parameter_types.clone(),
                            return_type: Box::new(expected_return_type.clone()),
                        },
                    },
                );
                if let Some(explicit_return_type) = &decl.return_type {
                    let explicit_return_type = explicit_return_type.check(ctx);
                    let inferred_return_type = decl.body.check(ctx);
                    if explicit_return_type != inferred_return_type {
                        panic!("Incorrect return type: {inferred_return_type}");
                    } else {
                        explicit_return_type
                    }
                } else {
                    VariableType::Void
                };
                VariableType::Function {
                    parameters: parameter_types,
                    return_type: Box::new(expected_return_type),
                }
            }
            Declaration::StructDeclaration(decl) => decl.check(ctx),
            Declaration::ModDeclaration(decl) => decl.check(ctx),
            Declaration::UseDeclaration(decl) => decl.check(ctx),
        }
    }
}

impl Check for Parameter {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        let r#type = self.r#type.check(ctx);
        ctx.scope.borrow_mut().variables.insert(
            self.name.0.clone(),
            Variable {
                r#type: r#type.clone(),
            },
        );
        r#type
    }
}

impl Check for StructDecl {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        ctx.insert_declaration(
            Path::from(self.ident.clone()),
            Resolved::Type(VariableType::Struct {
                fields: self
                    .fields
                    .iter()
                    .map(|f| (f.ident.0.clone(), f.check(ctx)))
                    .collect(),
            }),
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
            TypeExpression::Path(path) => {
                let resolved = ctx
                    .resolve(path)
                    .unwrap_or_else(|| panic!("Can't resolve type {path}"));
                if let Resolved::Type(r#type) = resolved {
                    r#type
                } else {
                    panic!("Can't resolve a module as a type");
                }
            }
        }
    }
}
