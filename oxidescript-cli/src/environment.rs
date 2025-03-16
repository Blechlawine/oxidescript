use oxidescript::{
    checker::{AstNode, SemanticAnalyser},
    lexer::{tokens::Tokens, Lexer},
    parser::Parser,
};

use crate::JavascriptRuntime;

#[derive(Clone, Debug, Default)]
pub enum JavascriptEnvironment {
    #[default]
    Browser,
    Bun,
    Node,
}

impl From<&str> for JavascriptEnvironment {
    fn from(value: &str) -> Self {
        match value {
            "browser" => JavascriptEnvironment::Browser,
            "bun" => JavascriptEnvironment::Bun,
            "node" => JavascriptEnvironment::Node,
            _ => Self::default(),
        }
    }
}

impl From<&JavascriptRuntime> for JavascriptEnvironment {
    fn from(value: &JavascriptRuntime) -> Self {
        match value {
            JavascriptRuntime::Bun => JavascriptEnvironment::Bun,
            JavascriptRuntime::Node => JavascriptEnvironment::Node,
        }
    }
}

impl JavascriptEnvironment {
    pub fn load(&self, ctx: &mut SemanticAnalyser) {
        let env_file = match self {
            JavascriptEnvironment::Browser => {
                include_str!("../environments/browser.d.os")
            }
            JavascriptEnvironment::Bun => {
                include_str!("../environments/bun.d.os")
            }
            JavascriptEnvironment::Node => {
                include_str!("../environments/node.d.os")
            }
        };

        let (unlexed, tokens) = Lexer::lex_tokens(env_file.as_bytes()).unwrap();
        assert!(unlexed.is_empty());
        let (unparsed, ast) = Parser::parse(Tokens::new(&tokens)).unwrap();
        assert!(unparsed.tokens.is_empty());
        ast.check_type(ctx);
    }
}
