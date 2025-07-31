// impl AstNode for MemberAccessExpr {
//     fn check_type(&self, ctx: &TypeChecker) -> VariableType {
//         let lhs_type = self.lhs.check_type(ctx);
//         if let VariableType::Struct { fields } = lhs_type {
//             if let Some(field_type) = fields.get(&self.ident.name) {
//                 field_type.clone()
//             } else {
//                 panic!("Missing field {}", self.ident.name)
//             }
//         } else {
//             panic!("Cannot index type other than struct: {}", self.ident.name)
//         }
//     }
//
//     fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
//         todo!()
//     }
// }
//
// impl AstNode for IfExpr {
//     fn check_type(&self, ctx: &TypeChecker) -> VariableType {
//         let condition_type = self.condition.check_type(ctx);
//         if condition_type != VariableType::Bool {
//             panic!("Expected bool, found: {condition_type}");
//         }
//         let then_type = self.then_block.check_type(ctx);
//         if !self
//             .else_if_blocks
//             .iter()
//             .map(|b| b.check_type(ctx))
//             .all(|t| t == then_type)
//         {
//             panic!("Expected {then_type}");
//         }
//         let else_type = self.else_block.as_ref().map(|b| b.check_type(ctx));
//         if let Some(else_type) = else_type {
//             if else_type != then_type {
//                 panic!("Expected {then_type}, found: {else_type}");
//             }
//         }
//         then_type
//     }
//
//     fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
//         todo!()
//     }
// }
//
// impl AstNode for ElseIfExpr {
//     fn check_type(&self, ctx: &TypeChecker) -> VariableType {
//         let condition_type = self.condition.check_type(ctx);
//         if condition_type != VariableType::Bool {
//             panic!("Expected bool, found: {condition_type}");
//         }
//         self.then_block.check_type(ctx)
//     }
//
//     fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
//         todo!()
//     }
// }
//
// impl AstNode for CallExpr {
//     fn check_type(&self, ctx: &TypeChecker) -> VariableType {
//         let lhs_type = self.lhs.check_type(ctx);
//         if let VariableType::Function {
//             parameters,
//             return_type,
//         } = lhs_type
//         {
//             if !self
//                 .arguments
//                 .iter()
//                 .map(|a| a.check_type(ctx))
//                 .zip(parameters)
//                 .all(|(given, expected)| given == expected)
//             {
//                 panic!("Invalid argument type");
//             }
//             *return_type
//         } else {
//             panic!("Cannot call non-function type: {lhs_type}");
//         }
//     }
//
//     fn visit(&mut self, visitor: &mut dyn super::AstVisitor) {
//         todo!()
//     }
// }
