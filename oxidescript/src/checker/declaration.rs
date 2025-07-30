use crate::{
    parser::ast::{Declaration, Parameter, StructDecl, StructField, TypeExpression},
    resolver::SymbolId,
};

use super::{AstNode, AstVisitor, TypeChecker, VariableType};

impl AstNode for Declaration {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        match self {
            Declaration::ConstDeclaration(identifier, expression) => {
                expression.check_type(ctx);
                VariableType::Void
            }
            Declaration::LetDeclaration(identifier, expression) => {
                expression.check_type(ctx);
                VariableType::Void
            }
            Declaration::FunctionDeclaration(decl) => {
                let parameter_types = decl
                    .parameters
                    .iter()
                    .map(|p| p.check_type(ctx))
                    .collect::<Vec<_>>();
                let expected_return_type = decl
                    .return_type
                    .as_ref()
                    .map(|v| v.check_type(ctx))
                    .unwrap_or_default();
                // ctx.scope.borrow_mut().variables.insert(
                //     decl.name.name.clone(),
                //     Variable {
                //         r#type: VariableType::Function {
                //             parameters: parameter_types.clone(),
                //             return_type: Box::new(expected_return_type.clone()),
                //         },
                //     },
                // );
                if let Some(return_type) = &decl.return_type {
                    return_type.check_type(ctx);
                }
                // ctx.insert_declaration(
                //     decl.name.clone().into(),
                //     Resolved::Type(VariableType::Function {
                //         parameters: parameter_types.clone(),
                //         return_type: decl
                //             .return_type
                //             .clone()
                //             .map(|rt| Box::new(rt.check_type(ctx)))
                //             .unwrap_or_default(),
                //     }),
                // );
                if let Some(explicit_return_type) = &decl.return_type {
                    let explicit_return_type = explicit_return_type.check_type(ctx);
                    if decl.has_body {
                        if let Some(body) = &decl.body {
                            let inferred_return_type = body.check_type(ctx);
                            if explicit_return_type != inferred_return_type {
                                panic!("Incorrect return type: {inferred_return_type}");
                            } else {
                                explicit_return_type
                            }
                        } else {
                            panic!("Function with has_body: true should have a body");
                        }
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
            Declaration::StructDeclaration(decl) => decl.check_type(ctx),
            Declaration::ModDeclaration(decl) => decl.check_type(ctx),
            Declaration::UseDeclaration(decl) => decl.check_type(ctx),
        }
    }

    fn visit(&mut self, visitor: &mut dyn AstVisitor) {
        match self {
            Declaration::ConstDeclaration(identifier, _) => {
                let ident_symbol_id = SymbolId::new();
                identifier.id = Some(ident_symbol_id);
                todo!()
            }
            Declaration::LetDeclaration(identifier, _) => {
                let ident_symbol_id = SymbolId::new();
                identifier.id = Some(ident_symbol_id);
                todo!()
            }
            Declaration::FunctionDeclaration(function_decl) => {
                let ident_symbol_id = SymbolId::new();
                function_decl.name.id = Some(ident_symbol_id);
                todo!()
            }
            Declaration::StructDeclaration(struct_decl) => todo!(),
            Declaration::ModDeclaration(module_declaration) => {}
            Declaration::UseDeclaration(_) => todo!(),
        }
    }
}

impl AstNode for Parameter {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        let r#type = self.r#type.check_type(ctx);
        // ctx.scope.borrow_mut().variables.insert(
        //     self.name.name.clone(),
        //     Variable {
        //         r#type: r#type.clone(),
        //     },
        // );
        r#type
    }

    fn visit(&mut self, visitor: &mut dyn AstVisitor) {
        todo!()
    }
}

impl AstNode for StructDecl {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        // ctx.insert_declaration(
        //     Path::from(self.ident.clone()),
        //     Resolved::Type(VariableType::Struct {
        //         fields: self
        //             .fields
        //             .iter()
        //             .map(|f| (f.ident.0.clone(), f.check_type(ctx)))
        //             .collect(),
        //     }),
        // );
        VariableType::Void
    }

    fn visit(&mut self, visitor: &mut dyn AstVisitor) {
        todo!()
    }
}

impl AstNode for StructField {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        self.r#type.check_type(ctx);
        VariableType::Void
    }

    fn visit(&mut self, visitor: &mut dyn AstVisitor) {
        todo!()
    }
}

impl AstNode for TypeExpression {
    fn check_type(&self, ctx: &TypeChecker) -> VariableType {
        match self {
            TypeExpression::Path(path) => {
                // let resolved = ctx
                //     .resolve(path)
                //     .unwrap_or_else(|| panic!("Can't resolve type {path}"));
                // if let Resolved::Type(r#type) = resolved {
                //     r#type
                // } else {
                //     panic!("Can't resolve a module as a type");
                // }
                todo!()
            }
        }
    }

    fn visit(&mut self, visitor: &mut dyn AstVisitor) {
        todo!()
    }
}
