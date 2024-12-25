use oxc::{
    ast::{ast::Expression, AstBuilder},
    span::Span,
};

use crate::IntoOxc;

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::IndexExpr {
    fn into_oxc(self, ctx: &'c crate::JavascriptCompilerContext<'c>) -> Expression<'c> {
        AstBuilder::new(ctx.allocator)
            .member_expression_computed(
                Span::new(0, 0),
                self.lhs.into_oxc(ctx),
                self.index.into_oxc(ctx),
                false,
            )
            .into()
    }
}
