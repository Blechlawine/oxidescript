use oxc::{
    ast::{
        ast::{Expression, Statement},
        AstBuilder,
    },
    span::Span,
};

use crate::{IntoOxc, JavascriptCompilerContext};

use super::iife;

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::IfExpr {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        iife(
            oxc::allocator::Vec::from_iter_in([self.into_oxc(ctx)], ctx.allocator),
            ctx,
        )
    }
}

impl<'c> IntoOxc<'c, Statement<'c>> for oxidescript::parser::ast::IfExpr {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Statement<'c> {
        AstBuilder::new(ctx.allocator).statement_if(
            Span::new(0, 0),
            self.condition.into_oxc(ctx),
            AstBuilder::new(ctx.allocator)
                .statement_block(Span::new(0, 0), self.then_block.into_oxc(ctx)),
            if let Some(else_block) = self.else_block {
                if !self.else_if_blocks.is_empty() {
                    let mut iter = self.else_if_blocks.into_iter().rev();
                    let mut inner = iter.next().map(|block| {
                        AstBuilder::new(ctx.allocator).statement_if(
                            Span::new(0, 0),
                            block.condition.into_oxc(ctx),
                            AstBuilder::new(ctx.allocator)
                                .statement_block(Span::new(0, 0), block.then_block.into_oxc(ctx)),
                            Some(
                                AstBuilder::new(ctx.allocator)
                                    .statement_block(Span::new(0, 0), else_block.into_oxc(ctx)),
                            ),
                        )
                    });
                    for else_if_block in iter {
                        inner = Some(AstBuilder::new(ctx.allocator).statement_if(
                            Span::new(0, 0),
                            else_if_block.condition.into_oxc(ctx),
                            AstBuilder::new(ctx.allocator).statement_block(
                                Span::new(0, 0),
                                else_if_block.then_block.into_oxc(ctx),
                            ),
                            inner,
                        ));
                    }
                    inner
                } else {
                    Some(
                        AstBuilder::new(ctx.allocator)
                            .statement_block(Span::new(0, 0), else_block.into_oxc(ctx)),
                    )
                }
            } else if !self.else_if_blocks.is_empty() {
                let mut iter = self.else_if_blocks.into_iter().rev();
                let mut inner = iter.next().map(|block| {
                    AstBuilder::new(ctx.allocator).statement_if(
                        Span::new(0, 0),
                        block.condition.into_oxc(ctx),
                        AstBuilder::new(ctx.allocator)
                            .statement_block(Span::new(0, 0), block.then_block.into_oxc(ctx)),
                        None,
                    )
                });
                for else_if_block in iter {
                    inner = Some(AstBuilder::new(ctx.allocator).statement_if(
                        Span::new(0, 0),
                        else_if_block.condition.into_oxc(ctx),
                        AstBuilder::new(ctx.allocator).statement_block(
                            Span::new(0, 0),
                            else_if_block.then_block.into_oxc(ctx),
                        ),
                        inner,
                    ));
                }
                inner
            } else {
                None
            },
        )
    }
}
