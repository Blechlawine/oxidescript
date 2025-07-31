use oxc::{
    ast::{
        AstBuilder,
        ast::{Expression, UnaryOperator},
    },
    span::Span,
};

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::UnaryExpr<'_> {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        AstBuilder::new(ctx.allocator).expression_unary(
            Span::new(0, 0),
            self.op.into_oxc(ctx),
            self.rhs.into_oxc(ctx),
        )
    }
}

impl<'c> IntoOxc<'c, UnaryOperator> for oxidescript::parser::ast::UnaryOperator {
    fn into_oxc(self, _: &'c JavascriptCompilerContext<'c>) -> UnaryOperator {
        match self {
            oxidescript::parser::ast::UnaryOperator::LogicalNot => UnaryOperator::LogicalNot,
            oxidescript::parser::ast::UnaryOperator::BitwiseNot => UnaryOperator::BitwiseNot,
            oxidescript::parser::ast::UnaryOperator::Minus => UnaryOperator::UnaryNegation,
            oxidescript::parser::ast::UnaryOperator::Plus => UnaryOperator::UnaryPlus,
        }
    }
}
