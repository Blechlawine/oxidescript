use std::{cell::RefCell, collections::HashMap};

use module_resolver::SymbolsResolver;
use symbols::{ModuleSymbol, Symbol, SymbolId, SymbolInner};
use type_checker::VariableType;

use crate::parser::ast::{Path, Program};

pub mod declaration;
pub mod expression;
pub mod module_resolver;
pub mod modules;
pub mod program;
pub mod symbols;
pub mod type_checker;

#[derive(Debug)]
pub struct SemanticAnalyser<'c> {
    parent_scopes: RefCell<HashMap<Scope, Scope>>,
    symbol_table: SymbolTable,
    errors: &'c RefCell<Vec<CheckError>>,
    // resolved: &'c RefCell<BTreeMap<Path, Resolved>>,
    /// this is the location in the ast, where we are
    current_scope: Option<Scope>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Scope {
    /// inside a module, the entrypoint scope is crate::main
    Module(SymbolId),
    /// inside a function
    Function(SymbolId),
    /// Block scope, like in if blocks
    Block(Path),
}

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

impl<'c> SemanticAnalyser<'c> {
    pub fn new(errors: &'c RefCell<Vec<CheckError>>) -> Self {
        Self {
            parent_scopes: RefCell::new(HashMap::new()),
            symbol_table: SymbolTable::default(),
            errors,
            current_scope: None,
        }
    }

    pub fn analyse_program(mut self, node: Program) -> Self {
        // 1. resolve modules and lex and parse them and include them in the ast, or if they have
        //    inline content, Option::take that content and put it into the module tree
        // 2. check types
        // 3. check for unused things, and remove them from the ast
        let main_module_id = SymbolId::new();
        let main_scope = Scope::Module(main_module_id);
        self.symbol_table.symbols.insert(
            main_module_id,
            Symbol {
                scope: main_scope,
                inner: SymbolInner::Module(ModuleSymbol::Intern { ast: Some(node) }),
            },
        );
        let mut module_resolver = SymbolsResolver::new(&mut self.symbol_table);
        module_resolver.start_resolving(main_module_id);
        // node.visit(&mut module_resolver);
        self
    }

    pub fn init(&mut self) {
        // self.resolved
        //     .borrow_mut()
        //     .insert(Path::from("String"), Resolved::Type(VariableType::String));
        // self.resolved
        //     .borrow_mut()
        //     .insert(Path::from("number"), Resolved::Type(VariableType::Number));
        // self.resolved
        //     .borrow_mut()
        //     .insert(Path::from("bool"), Resolved::Type(VariableType::Bool));
    }

    /// resolve a variable path in the current scope
    pub fn resolve_symbol(&self, id: SymbolId) -> Symbol {
        self.symbol_table
            .symbols
            .get(&id)
            .unwrap_or_else(|| panic!("Couldn't find symbol {} in scope", id))
            .clone()
    }

    // inserts a type into resolved at the current path
    // pub fn insert_declaration(&self, name: Path, insert: Resolved) {
    //     let path = self.current_scope.join(&name);
    //     for i in 0..path.len() {
    //         let (parts, rest) = path.elements.split_at(i + 1);
    //         let existing = {
    //             let resolved = self.resolved.borrow();
    //             resolved
    //                 .get(&Path {
    //                     elements: parts.to_vec(),
    //                     full_path: None,
    //                 })
    //                 .cloned()
    //         };
    //         let is_last_element = rest.is_empty();
    //         if let Some(existing) = existing {
    //             if is_last_element && existing != insert {
    //                 panic!("Can't overwrite existing type at path: {}", path)
    //             }
    //         } else if is_last_element {
    //             // println!("inserting type, {:?}", &path);
    //             self.symbol_table
    //                 .borrow_mut()
    //                 .insert(path.clone(), insert.clone());
    //             break;
    //         } else {
    //             println!("Does this need to be done?");
    //             // println!("inserting module, {:?}", &parts);
    //             // self.resolved.borrow_mut().insert(
    //             //     Path {
    //             //         elements: parts.to_vec(),
    //             //     },
    //             //     Resolved::Module { is_extern: false },
    //             // );
    //         }
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::{
        checker::{
            symbols::VariableSymbol, type_checker::VariableType, Scope, Symbol, SymbolId,
            SymbolInner,
        },
        lexer::{tokens::Tokens, Lexer},
        parser::{ast::Identifier, Parser},
    };

    use super::SemanticAnalyser;

    #[test]
    fn simple_assignment_type_inference() {
        let input = br#"let test = "Foo";"#;
        let (_, r) = Lexer::lex_tokens(input).unwrap();
        let tokens = Tokens::new(&r);
        let (_, result) = Parser::parse(tokens).unwrap();
        let errors = RefCell::new(vec![]);
        let mut analyser = SemanticAnalyser::new(&errors);
        analyser.init();
        let analyser = analyser.analyse_program(result);
        // let inferred = result.check_type(&analyser);
        assert_eq!(
            analyser
                .symbol_table
                .symbols
                .iter()
                .find(|s| match &s.1.inner {
                    SymbolInner::Module(_) => unreachable!(),
                    SymbolInner::Type(_) => unreachable!(),
                    SymbolInner::Function(_) => unreachable!(),
                    SymbolInner::Variable(variable) => variable.name.name == "test",
                })
                .unwrap()
                .1
                .clone(),
            Symbol {
                inner: SymbolInner::Variable(VariableSymbol {
                    r#type: Some(VariableType::String),
                    name: Identifier {
                        name: "test".to_string(),
                        id: Some(SymbolId(1))
                    }
                }),
                scope: Scope::Module(SymbolId(0))
            }
        );
    }
}
