use oxc::{
    ast::{
        ast::{Expression, FormalParameter, FormalParameters, TSTypeParameterInstantiation},
        AstBuilder,
    },
    span::Span,
};

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, FormalParameters<'c>> for Vec<oxidescript::parser::ast::Parameter> {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> FormalParameters<'c> {
        FormalParameters {
            span: Span::new(0, 0),
            kind: oxc::ast::ast::FormalParameterKind::FormalParameter,
            items: oxc::allocator::Vec::from_iter_in(
                self.into_iter().map(|param| param.into_oxc(ctx)),
                ctx.allocator,
            ),
            rest: None,
        }
    }
}

impl<'c> IntoOxc<'c, FormalParameter<'c>> for oxidescript::parser::ast::Parameter {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> FormalParameter<'c> {
        FormalParameter {
            span: Span::new(0, 0),
            pattern: self.name.into_oxc(ctx),
            accessibility: None,
            readonly: false,
            r#override: false,
            decorators: oxc::allocator::Vec::new_in(ctx.allocator),
        }
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::CallExpr {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        AstBuilder::new(ctx.allocator).expression_call(
            Span::new(0, 0),
            self.lhs.into_oxc(ctx),
            None::<TSTypeParameterInstantiation>,
            self.arguments.into_oxc(ctx),
            false,
        )
    }
}
