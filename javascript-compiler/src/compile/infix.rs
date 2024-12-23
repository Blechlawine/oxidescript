use oxc::{
    ast::{ast::Expression, AstBuilder},
    span::Span,
};

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::InfixExpr {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        match self.op {
            oxidescript::parser::ast::InfixOperator::Plus => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::Addition,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::Minus => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::Subtraction,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::Multiply => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::Multiplication,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::Divide => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::Division,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::Modulo => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::Remainder,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::Equal => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::StrictEquality,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::NotEqual => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::Inequality,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::GreaterThan => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::GreaterThan,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::LessThan => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::LessThan,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::GreaterThanEqual => {
                AstBuilder::new(ctx.allocator).expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::GreaterEqualThan,
                    self.rhs.into_oxc(ctx),
                )
            }
            oxidescript::parser::ast::InfixOperator::LessThanEqual => {
                AstBuilder::new(ctx.allocator).expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::LessEqualThan,
                    self.rhs.into_oxc(ctx),
                )
            }
            oxidescript::parser::ast::InfixOperator::LogicalOr => AstBuilder::new(ctx.allocator)
                .expression_logical(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::LogicalOperator::Or,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::LogicalAnd => AstBuilder::new(ctx.allocator)
                .expression_logical(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::LogicalOperator::And,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::BitwiseOr => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::BitwiseOR,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::BitwiseXor => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::BitwiseXOR,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::BitwiseAnd => AstBuilder::new(ctx.allocator)
                .expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::BitwiseAnd,
                    self.rhs.into_oxc(ctx),
                ),
            oxidescript::parser::ast::InfixOperator::BitwiseLeftShift => {
                AstBuilder::new(ctx.allocator).expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::ShiftLeft,
                    self.rhs.into_oxc(ctx),
                )
            }
            oxidescript::parser::ast::InfixOperator::BitwiseRightShift => {
                AstBuilder::new(ctx.allocator).expression_binary(
                    Span::new(0, 0),
                    self.lhs.into_oxc(ctx),
                    oxc::syntax::operator::BinaryOperator::ShiftRight,
                    self.rhs.into_oxc(ctx),
                )
            }
        }
    }
}
