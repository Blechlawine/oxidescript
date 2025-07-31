use std::{collections::HashMap, fmt::Display};

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
