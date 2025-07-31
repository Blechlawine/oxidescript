use oxc::ast::ast::{Expression, Statement};
use oxidescript::parser::ast::ForExpr;
use rand::{Rng, distributions::Alphanumeric};

use crate::IntoOxc;

impl<'c, 'src> IntoOxc<'c, Expression<'c>> for ForExpr<'src> {
    fn into_oxc(self, ctx: &'c crate::JavascriptCompilerContext<'c>) -> Expression<'c> {
        let has_output = self.body.return_value.is_some();
        if has_output {
            let output_id = rand::thread_rng()
                .sample_iter(&Alphanumeric)
                .take(8)
                .map(char::from)
                .collect::<String>();
            let inner_statements = oxc::allocator::Vec::from_iter_in(
                [ctx.statement_expr(
                    ctx.expr_call(
                        ctx.member_access(
                            oxidescript::parser::ast::Identifier {
                                name: &output_id,
                                id: None,
                            }
                            .into_oxc(ctx),
                            ctx.ident_name("push"),
                        ),
                        oxc::allocator::Vec::from_iter_in(
                            [ctx.iife(self.body.into_oxc(ctx)).into()],
                            ctx.allocator,
                        ),
                    ),
                )],
                ctx.allocator,
            );
            let r#loop = ctx.for_of(
                self.lhs.into_oxc(ctx),
                self.rhs.into_oxc(ctx),
                inner_statements,
            );
            ctx.iife(oxc::allocator::Vec::from_iter_in(
                [
                    oxc::ast::ast::Statement::VariableDeclaration(oxc::allocator::Box::new_in(
                        ctx.r#let(
                            oxidescript::parser::ast::Identifier {
                                name: &output_id,
                                id: None,
                            }
                            .into_oxc(ctx),
                            Some(ctx.array(None)),
                        ),
                        ctx.allocator,
                    )),
                    r#loop,
                    ctx.r#return(Some(
                        oxidescript::parser::ast::Identifier {
                            name: &output_id,
                            id: None,
                        }
                        .into_oxc(ctx),
                    )),
                ],
                ctx.allocator,
            ))
        } else {
            todo!(
                "for loop without return_value in body can't be compiled to expression, only statement"
            );
        }
    }
}

impl<'c, 'src> IntoOxc<'c, Statement<'c>> for ForExpr<'src> {
    fn into_oxc(self, ctx: &'c crate::JavascriptCompilerContext<'c>) -> Statement<'c> {
        ctx.for_of(
            self.lhs.into_oxc(ctx),
            self.rhs.into_oxc(ctx),
            self.body.into_oxc(ctx),
        )
    }
}
