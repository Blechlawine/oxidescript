use std::{cell::RefCell, collections::HashMap, fmt::Display};

pub mod declaration;
pub mod expression;
pub mod program;

#[derive(Debug)]
pub struct CheckContext {
    scope: RefCell<Scope>,
    errors: RefCell<Vec<CheckError>>,
}

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<String, Variable>,
    types: HashMap<String, VariableType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable {
    r#type: VariableType,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
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
        Self {
            scope: RefCell::new(Scope::default()),
            errors: RefCell::new(vec![]),
        }
    }
}

impl CheckContext {
    pub fn resolve_type(&self, type_name: &str) -> VariableType {
        self.scope
            .borrow()
            .types
            .get(type_name)
            .unwrap_or_else(|| panic!("Couldn't resolve type {type_name}"))
            .clone()
    }

    pub fn resolve_variable(&self, variable_name: &str) -> Variable {
        self.scope
            .borrow()
            .variables
            .get(variable_name)
            .unwrap_or_else(|| panic!("Couldn't find variable {variable_name} in scope"))
            .clone()
    }
}

impl Default for Scope {
    fn default() -> Self {
        let mut types = HashMap::new();
        types.insert("String".to_string(), VariableType::String);
        types.insert("number".to_string(), VariableType::Number);
        types.insert("bool".to_string(), VariableType::Bool);
        Self {
            variables: HashMap::new(),
            types,
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
