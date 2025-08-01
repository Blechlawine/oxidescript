use oxidescript::{
    checker::SemanticAnalyser,
    lexer::{Lexer, Span, tokens::Tokens},
    parser::Parser,
};

#[derive(Clone, Debug, Default)]
pub enum JavascriptEnvironment {
    #[default]
    Browser,
    // Maybe we can add more environments here, but for now we just support the browser
}

impl From<&str> for JavascriptEnvironment {
    fn from(value: &str) -> Self {
        match value {
            "browser" => JavascriptEnvironment::Browser,
            _ => Self::default(),
        }
    }
}

impl JavascriptEnvironment {
    pub fn load(&self, ctx: &mut SemanticAnalyser) {
        let env_file = match self {
            JavascriptEnvironment::Browser => {
                include_str!("../environments/browser.d.os")
            }
        };

        let (unlexed, tokens) = Lexer::lex_tokens(Span::new(env_file)).unwrap();
        assert!(unlexed.is_empty());
        let (unparsed, ast) = Parser::parse(Tokens::new(&tokens)).unwrap();
        assert!(unparsed.tokens.is_empty());
        // ast.check_type(ctx);
        todo!()
    }
}
