use oxc::{
    ast::ast::{Expression, FormalParameter, FormalParameters},
    span::Span,
};

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c, 'src> IntoOxc<'c, FormalParameters<'c>>
    for Vec<oxidescript::parser::ast::Parameter<'src>>
{
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

impl<'c, 'src> IntoOxc<'c, FormalParameter<'c>> for oxidescript::parser::ast::Parameter<'src> {
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

impl<'c, 'src> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::CallExpr<'src> {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        ctx.expr_call(self.lhs.into_oxc(ctx), self.arguments.into_oxc(ctx))
    }
}
