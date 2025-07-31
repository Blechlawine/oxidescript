// impl AstNode for Program {
//     fn check_type(&self, ctx: &TypeChecker) -> VariableType {
//         for statement in self {
//             statement.check_type(ctx);
//         }
//         VariableType::Void
//     }
//
//     fn visit(&mut self, visitor: &mut dyn AstVisitor) {
//         for statement in self {
//             statement.visit(visitor);
//         }
//     }
// }
//
// impl AstNode for Statement {
//     fn check_type(&self, ctx: &TypeChecker) -> VariableType {
//         match self {
//             Statement::ExpressionStatement { expression, .. } => expression.check_type(ctx),
//             Statement::DeclarationStatement(declaration) => declaration.check_type(ctx),
//         };
//         VariableType::Void
//     }
//
//     fn visit(&mut self, visitor: &mut dyn AstVisitor) {
//         match self {
//             Statement::ExpressionStatement { expression, .. } => expression.visit(visitor),
//             Statement::DeclarationStatement(declaration) => declaration.visit(visitor),
//         }
//     }
// }
//
// impl AstNode for Block {
//     fn check_type(&self, ctx: &TypeChecker) -> VariableType {
//         for statement in &self.statements {
//             statement.check_type(ctx);
//         }
//         if let Some(return_value) = &self.return_value {
//             return_value.check_type(ctx)
//         } else {
//             VariableType::Void
//         }
//     }
//
//     fn visit(&mut self, visitor: &mut dyn AstVisitor) {
//         todo!()
//     }
// }
