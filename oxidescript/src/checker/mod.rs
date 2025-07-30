use std::collections::HashMap;

use module_resolver::SymbolsResolver;
use symbols::{ModuleSymbol, Symbol, SymbolInner};
use type_checker::VariableType;

use crate::{parser::ast::Program, resolver::SymbolId};

pub mod declaration;
pub mod expression;
pub mod module_resolver;
pub mod modules;
pub mod program;
pub mod symbols;
pub mod type_checker;

#[derive(Debug)]
pub enum CheckError {
    WrongType,
}

pub trait AstNode {
    fn visit(&mut self, visitor: &mut dyn AstVisitor);
    fn check_type(&self, ctx: &TypeChecker) -> VariableType;
}

pub trait AstVisitor {}

struct TypeChecker<'s> {
    symbols: &'s SymbolTable,
}

#[derive(Debug, Default)]
struct SymbolTable {
    symbols: HashMap<SymbolId, Symbol>,
}

// 1. resolve modules and lex and parse them and include them in the ast, or if they have
//    inline content, Option::take that content and put it into the module tree
// 2. check types
// 3. check for unused things, and remove them from the ast
pub struct SemanticAnalyser {}

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
