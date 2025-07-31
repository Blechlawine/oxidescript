use std::cell::RefCell;

use crate::{
    parser::ast::{Declaration, Expression, Program, Statement},
    resolver::Resolver,
    scope::{ParentScopes, Scope},
    symbols::SymbolTable,
};

pub mod declaration;
pub mod expression;
pub mod modules;
pub mod program;
pub mod symbols;

#[derive(Debug)]
pub enum CheckError {
    WrongType,
}

// 1. resolve modules and lex and parse them and include them in the ast, or if they have
//    inline content, Option::take that content and put it into the module tree
// 2. check types
// 3. check for unused things, and remove them from the ast
pub struct SemanticAnalyser {}

pub struct Checker<'src> {
    scopes: ParentScopes,
    symbols: SymbolTable<'src>,
    errors: RefCell<Vec<CheckError>>,
}

impl<'src> Checker<'src> {
    pub fn new(resolver: Resolver<'src>) -> Self {
        Self {
            scopes: resolver.scopes,
            symbols: resolver.symbols,
            errors: RefCell::new(vec![]),
        }
    }

    pub fn check_program(&self, program: Program) -> Result<(), Vec<CheckError>> {
        let scope = self.scopes.global_scope();
        let _ = self.check_vec_of_statements(program, scope);
        if self.errors.borrow().is_empty() {
            Ok(())
        } else {
            Err(self.errors.replace(Vec::new()))
        }
    }

    fn check_vec_of_statements(&self, statements: Vec<Statement>, scope: Scope) -> Result<(), ()> {
        for statement in statements {
            self.check_statement(statement, scope)?;
        }
        Ok(())
    }

    fn check_statement(&self, statement: Statement, scope: Scope) -> Result<(), ()> {
        match statement {
            Statement::ExpressionStatement { expression, .. } => {
                self.check_expression(expression, scope)
            }
            Statement::DeclarationStatement(declaration) => {
                self.check_declaration(declaration, scope)
            }
        }
    }

    fn check_expression(&self, expression: Expression, scope: Scope) -> Result<(), ()> {
        match expression {
            Expression::PathExpression(path) => todo!(),
            Expression::LiteralExpression(literal) => todo!(),
            Expression::UnaryExpression(unary_expr) => todo!(),
            Expression::InfixExpression(infix_expr) => todo!(),
            Expression::ArrayExpression(expressions) => todo!(),
            Expression::IfExpression(if_expr) => todo!(),
            Expression::ForExpression(for_expr) => todo!(),
            Expression::BlockExpression(block) => todo!(),
            Expression::CallExpression(call_expr) => todo!(),
            Expression::IndexExpression(index_expr) => todo!(),
            Expression::MemberAccessExpression(member_access_expr) => todo!(),
        }
    }

    fn check_declaration(&self, declaration: Declaration, scope: Scope) -> Result<(), ()> {
        match declaration {
            Declaration::ConstDeclaration(identifier, expression) => todo!(),
            Declaration::LetDeclaration(identifier, expression) => todo!(),
            Declaration::FunctionDeclaration(function_decl) => todo!(),
            Declaration::StructDeclaration(struct_decl) => todo!(),
            Declaration::ModDeclaration(module_declaration) => todo!(),
            Declaration::UseDeclaration(_) => todo!(),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use std::cell::RefCell;
//
//     use crate::{
//         checker::{
//             symbols::VariableSymbol, type_checker::VariableType, Symbol, SymbolId, SymbolInner,
//         },
//         lexer::{tokens::Tokens, Lexer},
//         parser::{ast::Identifier, Parser},
//     };
//
//     use super::SemanticAnalyser;
//
// #[test]
// fn simple_assignment_type_inference() {
//     let input = br#"let test = "Foo";"#;
//     let (_, r) = Lexer::lex_tokens(input).unwrap();
//     let tokens = Tokens::new(&r);
//     let (_, result) = Parser::parse(tokens).unwrap();
//     let errors = RefCell::new(vec![]);
//     let mut analyser = SemanticAnalyser::new(&errors);
//     analyser.init();
//     let analyser = analyser.analyse_program(result);
//     // let inferred = result.check_type(&analyser);
//     assert_eq!(
//         analyser
//             .symbol_table
//             .symbols
//             .iter()
//             .find(|s| match &s.1.inner {
//                 SymbolInner::Module(_) => unreachable!(),
//                 SymbolInner::Type(_) => unreachable!(),
//                 SymbolInner::Function(_) => unreachable!(),
//                 SymbolInner::Variable(variable) => variable.name.name == "test",
//             })
//             .unwrap()
//             .1
//             .clone(),
//         Symbol {
//             inner: SymbolInner::Variable(VariableSymbol {
//                 r#type: Some(VariableType::String),
//                 name: Identifier {
//                     name: "test".to_string(),
//                     id: Some(SymbolId(1))
//                 }
//             }),
//             scope: Scope::Module(SymbolId(0))
//         }
//     );
// }
// }
