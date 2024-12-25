use oxc::{
    ast::{
        ast::{BooleanLiteral, Expression, NumberBase, NumericLiteral, StringLiteral},
        AstBuilder,
    },
    span::Span,
};
use oxidescript::parser::ast::Number;

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::Literal {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        match self {
            oxidescript::parser::ast::Literal::StringLiteral(s) => {
                AstBuilder::new(ctx.allocator).expression_string_literal(Span::new(0, 0), &s, None)
            }
            oxidescript::parser::ast::Literal::NumberLiteral(n) => Expression::NumericLiteral(
                oxc::allocator::Box::new_in(n.into_oxc(ctx), ctx.allocator),
            ),
            oxidescript::parser::ast::Literal::BooleanLiteral(b) => {
                AstBuilder::new(ctx.allocator).expression_boolean_literal(Span::new(0, 0), b)
            }
        }
    }
}

impl<'c> IntoOxc<'c, BooleanLiteral> for bool {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> BooleanLiteral {
        AstBuilder::new(ctx.allocator).boolean_literal(Span::new(0, 0), self)
    }
}

impl<'c> IntoOxc<'c, StringLiteral<'c>> for String {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> StringLiteral<'c> {
        AstBuilder::new(ctx.allocator).string_literal(Span::new(0, 0), self, None)
    }
}

impl<'c> IntoOxc<'c, NumericLiteral<'c>> for Number {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> NumericLiteral<'c> {
        match self {
            Number::F(f) => AstBuilder::new(ctx.allocator).numeric_literal(
                Span::new(0, 0),
                f.parse().unwrap(),
                None,
                NumberBase::Decimal,
            ),
            Number::I { base, value } => AstBuilder::new(ctx.allocator).numeric_literal(
                Span::new(0, 0),
                value.into(),
                None,
                match base {
                    oxidescript::parser::ast::NumberBase::Bin => NumberBase::Binary,
                    oxidescript::parser::ast::NumberBase::Hex => NumberBase::Hex,
                    oxidescript::parser::ast::NumberBase::Oct => NumberBase::Octal,
                    oxidescript::parser::ast::NumberBase::Dec => NumberBase::Decimal,
                },
            ),
        }
    }
}
