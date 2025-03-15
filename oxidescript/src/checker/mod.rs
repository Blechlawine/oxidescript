use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

use serde::Deserialize;

use crate::parser::ast::{Identifier, Path};

pub mod declaration;
pub mod expression;
pub mod modules;
pub mod program;

#[derive(Debug)]
pub struct CheckContext {
    pub scope: RefCell<Scope>,
    errors: RefCell<Vec<CheckError>>,
    resolved: RefCell<BTreeMap<Path, Resolved>>,
    current_resolved_path: Path,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Resolved {
    Module,
    Type(VariableType),
}

#[derive(Debug, Default)]
pub struct Scope {
    pub variables: HashMap<String, Variable>,
    local_types: HashMap<String, VariableType>,
}

#[derive(Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Variable {
    r#type: VariableType,
}

#[derive(Deserialize, Clone, Debug, PartialEq, Eq, Default)]
pub enum VariableType {
    String,
    Number,
    Bool,
    Struct {
        fields: HashMap<String, VariableType>,
    },
    Vec(Box<VariableType>),
    Function {
        parameters: Vec<VariableType>,
        return_type: Box<VariableType>,
    },
    /// the type which non-returning if and for expressions infer to
    #[default]
    Void,
}

impl Display for VariableType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VariableType::String => f.write_str("String"),
            VariableType::Number => f.write_str("number"),
            VariableType::Bool => f.write_str("bool"),
            VariableType::Struct { .. } => f.write_str("Struct"),
            VariableType::Vec(variable_type) => write!(f, "Vec<{variable_type}>"),
            VariableType::Void => f.write_str("void"),
            VariableType::Function {
                parameters,
                return_type,
            } => {
                write!(f, "({:?}) -> {}", parameters, return_type)
            }
        }
    }
}

#[derive(Debug)]
pub enum CheckError {
    WrongType,
}

pub trait Check {
    fn check(&self, ctx: &CheckContext) -> VariableType;
}

impl Default for CheckContext {
    fn default() -> Self {
        let mut resolved = BTreeMap::new();
        resolved.insert(Path::from("String"), Resolved::Type(VariableType::String));
        resolved.insert(Path::from("number"), Resolved::Type(VariableType::Number));
        resolved.insert(Path::from("bool"), Resolved::Type(VariableType::Bool));
        Self {
            scope: RefCell::new(Scope::default()),
            errors: RefCell::new(vec![]),
            resolved: RefCell::new(resolved),
            current_resolved_path: Path::from(Identifier("crate".to_string())),
        }
    }
}

impl CheckContext {
    pub fn resolve(&self, path: &Path) -> Option<Resolved> {
        self.resolved.borrow().get(path).cloned()
    }

    /// resolve a variable path in the current scope
    pub fn resolve_variable(&self, variable_path: &Path) -> Variable {
        if variable_path.len() == 1 {
            // the path is only the identifier, so we should resolve from scope
            let ident = variable_path.elements.first().unwrap();
            self.scope
                .borrow()
                .variables
                .get(&ident.0)
                .unwrap_or_else(|| panic!("Couldn't find variable {} in scope", ident.0))
                .clone()
        } else {
            // the path is not just an identifier, so we should resolve from resolved
            todo!()
        }
    }

    /// inserts a type into resolved at the current path
    pub fn insert_declaration(&self, name: Path, insert: Resolved) {
        let path = self.current_resolved_path.join(&name);
        for i in 0..path.len() - 1 {
            let (parts, rest) = path.elements.split_at(i);
            let resolved = self.resolved.borrow();
            let existing = resolved.get(&Path {
                elements: parts.to_vec(),
            });
            let is_last_element = rest.is_empty();
            if let Some(existing) = existing {
                if is_last_element && *existing != insert {
                    panic!("Can't overwrite existing type at path: {}", path)
                }
            } else if is_last_element {
                self.resolved
                    .borrow_mut()
                    .insert(path.clone(), insert.clone());
                break;
            } else {
                self.resolved.borrow_mut().insert(
                    Path {
                        elements: parts.to_vec(),
                    },
                    Resolved::Module,
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        checker::{Variable, VariableType},
        lexer::{tokens::Tokens, Lexer},
        parser::Parser,
    };

    use super::{Check, CheckContext};

    #[test]
    fn simple_assignment_type_inference() {
        let input = br#"let test = "Foo";"#;
        let (_, r) = Lexer::lex_tokens(input).unwrap();
        let tokens = Tokens::new(&r);
        let (_, result) = Parser::parse(tokens).unwrap();
        let check_ctx = CheckContext::default();
        let inferred = result.check(&check_ctx);
        assert_eq!(inferred, VariableType::Void);
        assert_eq!(
            check_ctx.scope.borrow().variables.get("test"),
            Some(Variable {
                r#type: VariableType::String
            })
            .as_ref()
        );
    }
}
