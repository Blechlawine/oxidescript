use oxc::{
    ast::{
        ast::{
            Argument, BinaryExpression, BinaryOperator, Expression, LogicalExpression,
            LogicalOperator,
        },
        AstBuilder,
    },
    span::Span,
};

use crate::{IntoOxc, JavascriptCompilerContext};

fn binary_expr<'c>(
    ctx: &'c JavascriptCompilerContext<'c>,
    lhs: impl IntoOxc<'c, Expression<'c>>,
    rhs: impl IntoOxc<'c, Expression<'c>>,
    op: BinaryOperator,
) -> oxc::allocator::Box<BinaryExpression<'c>> {
    oxc::allocator::Box::new_in(
        AstBuilder::new(ctx.allocator).binary_expression(
            Span::new(0, 0),
            lhs.into_oxc(ctx),
            op,
            rhs.into_oxc(ctx),
        ),
        ctx.allocator,
    )
}

fn logical_expr<'c>(
    ctx: &'c JavascriptCompilerContext<'c>,
    lhs: impl IntoOxc<'c, Expression<'c>>,
    rhs: impl IntoOxc<'c, Expression<'c>>,
    op: LogicalOperator,
) -> oxc::allocator::Box<LogicalExpression<'c>> {
    oxc::allocator::Box::new_in(
        AstBuilder::new(ctx.allocator).logical_expression(
            Span::new(0, 0),
            lhs.into_oxc(ctx),
            op,
            rhs.into_oxc(ctx),
        ),
        ctx.allocator,
    )
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::InfixExpr {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        match self.op {
            oxidescript::parser::ast::InfixOperator::Plus => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Addition),
            ),
            oxidescript::parser::ast::InfixOperator::Minus => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Subtraction),
            ),
            oxidescript::parser::ast::InfixOperator::Multiply => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Multiplication),
            ),
            oxidescript::parser::ast::InfixOperator::Divide => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Division),
            ),
            oxidescript::parser::ast::InfixOperator::Modulo => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Remainder),
            ),
            oxidescript::parser::ast::InfixOperator::Equal => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::StrictEquality),
            ),
            oxidescript::parser::ast::InfixOperator::NotEqual => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Inequality),
            ),
            oxidescript::parser::ast::InfixOperator::GreaterThan => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::GreaterThan),
            ),
            oxidescript::parser::ast::InfixOperator::LessThan => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::LessThan),
            ),
            oxidescript::parser::ast::InfixOperator::GreaterThanEqual => {
                Expression::BinaryExpression(binary_expr(
                    ctx,
                    *self.lhs,
                    *self.rhs,
                    BinaryOperator::GreaterEqualThan,
                ))
            }
            oxidescript::parser::ast::InfixOperator::LessThanEqual => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::LessEqualThan),
            ),
            oxidescript::parser::ast::InfixOperator::LogicalOr => Expression::LogicalExpression(
                logical_expr(ctx, *self.lhs, *self.rhs, LogicalOperator::Or),
            ),
            oxidescript::parser::ast::InfixOperator::LogicalAnd => Expression::LogicalExpression(
                logical_expr(ctx, *self.lhs, *self.rhs, LogicalOperator::And),
            ),
            oxidescript::parser::ast::InfixOperator::BitwiseOr => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::BitwiseOR),
            ),
            oxidescript::parser::ast::InfixOperator::BitwiseXor => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::BitwiseXOR),
            ),
            oxidescript::parser::ast::InfixOperator::BitwiseAnd => Expression::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::BitwiseAnd),
            ),
            oxidescript::parser::ast::InfixOperator::BitwiseLeftShift => {
                Expression::BinaryExpression(binary_expr(
                    ctx,
                    *self.lhs,
                    *self.rhs,
                    BinaryOperator::ShiftLeft,
                ))
            }
            oxidescript::parser::ast::InfixOperator::BitwiseRightShift => {
                Expression::BinaryExpression(binary_expr(
                    ctx,
                    *self.lhs,
                    *self.rhs,
                    BinaryOperator::ShiftRight,
                ))
            }
        }
    }
}

impl<'c> IntoOxc<'c, Argument<'c>> for oxidescript::parser::ast::InfixExpr {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Argument<'c> {
        match self.op {
            oxidescript::parser::ast::InfixOperator::Equal => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Equality),
            ),
            oxidescript::parser::ast::InfixOperator::NotEqual => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Inequality),
            ),
            oxidescript::parser::ast::InfixOperator::GreaterThan => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::GreaterThan),
            ),
            oxidescript::parser::ast::InfixOperator::LessThan => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::LessThan),
            ),
            oxidescript::parser::ast::InfixOperator::GreaterThanEqual => {
                Argument::BinaryExpression(binary_expr(
                    ctx,
                    *self.lhs,
                    *self.rhs,
                    BinaryOperator::GreaterEqualThan,
                ))
            }
            oxidescript::parser::ast::InfixOperator::LessThanEqual => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::LessEqualThan),
            ),
            oxidescript::parser::ast::InfixOperator::Plus => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Addition),
            ),
            oxidescript::parser::ast::InfixOperator::Minus => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Subtraction),
            ),
            oxidescript::parser::ast::InfixOperator::Multiply => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Multiplication),
            ),
            oxidescript::parser::ast::InfixOperator::Divide => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Division),
            ),
            oxidescript::parser::ast::InfixOperator::Modulo => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::Remainder),
            ),
            oxidescript::parser::ast::InfixOperator::LogicalOr => Argument::LogicalExpression(
                logical_expr(ctx, *self.lhs, *self.rhs, LogicalOperator::Or),
            ),
            oxidescript::parser::ast::InfixOperator::LogicalAnd => Argument::LogicalExpression(
                logical_expr(ctx, *self.lhs, *self.rhs, LogicalOperator::And),
            ),
            oxidescript::parser::ast::InfixOperator::BitwiseOr => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::BitwiseOR),
            ),
            oxidescript::parser::ast::InfixOperator::BitwiseXor => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::BitwiseXOR),
            ),
            oxidescript::parser::ast::InfixOperator::BitwiseAnd => Argument::BinaryExpression(
                binary_expr(ctx, *self.lhs, *self.rhs, BinaryOperator::BitwiseAnd),
            ),
            oxidescript::parser::ast::InfixOperator::BitwiseLeftShift => {
                Argument::BinaryExpression(binary_expr(
                    ctx,
                    *self.lhs,
                    *self.rhs,
                    BinaryOperator::ShiftLeft,
                ))
            }
            oxidescript::parser::ast::InfixOperator::BitwiseRightShift => {
                Argument::BinaryExpression(binary_expr(
                    ctx,
                    *self.lhs,
                    *self.rhs,
                    BinaryOperator::ShiftRight,
                ))
            }
        }
    }
}
