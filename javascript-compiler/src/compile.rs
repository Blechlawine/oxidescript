use oxc::{
    ast::{
        ast::{
            Argument, ArrayExpressionElement, BindingRestElement, Expression, FunctionBody,
            Program, Statement, TSTypeAnnotation, TSTypeParameterDeclaration,
            TSTypeParameterInstantiation, VariableDeclarator,
        },
        AstBuilder,
    },
    span::{SourceType, Span},
};

pub mod conditional;
pub mod function;
pub mod ident;
pub mod infix;
pub mod literal;
pub mod unary;

use crate::{IntoOxc, JavascriptCompilerContext};

impl<'c> IntoOxc<'c, Program<'c>> for oxidescript::parser::ast::Program {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Program {
        AstBuilder::new(ctx.allocator).program(
            Span::new(0, 0),
            SourceType::default(),
            "",
            oxc::allocator::Vec::new_in(ctx.allocator),
            None,
            oxc::allocator::Vec::new_in(ctx.allocator),
            oxc::allocator::Vec::from_iter_in(
                self.into_iter().map(|statement| statement.into_oxc(ctx)),
                ctx.allocator,
            ),
        )
    }
}

impl<'c> IntoOxc<'c, Statement<'c>> for oxidescript::parser::ast::Statement {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Statement {
        println!("compiling statement to statement");
        match self {
            oxidescript::parser::ast::Statement::ExpressionStatement { expression, .. } => {
                AstBuilder::new(ctx.allocator)
                    .statement_expression(Span::new(0, 0), expression.into_oxc(ctx))
            }
            oxidescript::parser::ast::Statement::DeclarationStatement(declaration) => {
                match declaration {
                    oxidescript::parser::ast::Declaration::ConstDeclaration(ident, expr) => {
                        oxc::ast::ast::Statement::VariableDeclaration(oxc::allocator::Box::new_in(
                            AstBuilder::new(ctx.allocator).variable_declaration(
                                Span::new(0, 0),
                                oxc::ast::ast::VariableDeclarationKind::Const,
                                oxc::allocator::Vec::from_iter_in(
                                    vec![VariableDeclarator {
                                        span: Span::new(0, 0),
                                        kind: oxc::ast::ast::VariableDeclarationKind::Const,
                                        id: ident.into_oxc(ctx),
                                        init: Some(expr.into_oxc(ctx)),
                                        definite: false,
                                    }],
                                    ctx.allocator,
                                ),
                                false,
                            ),
                            ctx.allocator,
                        ))
                    }
                    oxidescript::parser::ast::Declaration::LetDeclaration(ident, expr) => {
                        oxc::ast::ast::Statement::VariableDeclaration(oxc::allocator::Box::new_in(
                            AstBuilder::new(ctx.allocator).variable_declaration(
                                Span::new(0, 0),
                                oxc::ast::ast::VariableDeclarationKind::Let,
                                oxc::allocator::Vec::from_iter_in(
                                    vec![VariableDeclarator {
                                        span: Span::new(0, 0),
                                        kind: oxc::ast::ast::VariableDeclarationKind::Let,
                                        id: ident.into_oxc(ctx),
                                        init: Some(expr.into_oxc(ctx)),
                                        definite: false,
                                    }],
                                    ctx.allocator,
                                ),
                                false,
                            ),
                            ctx.allocator,
                        ))
                    }
                    oxidescript::parser::ast::Declaration::FunctionDeclaration {
                        name,
                        parameters,
                        body,
                    } => {
                        oxc::ast::ast::Statement::FunctionDeclaration(oxc::allocator::Box::new_in(
                            oxc::ast::ast::Function {
                                r#type: oxc::ast::ast::FunctionType::FunctionDeclaration,
                                span: Span::new(0, 0),
                                id: Some(name.into_oxc(ctx)),
                                generator: false,
                                r#async: false,
                                declare: false,
                                type_parameters: None,
                                this_param: None,
                                params: oxc::allocator::Box::new_in(
                                    parameters.into_oxc(ctx),
                                    ctx.allocator,
                                ),
                                body: Some(body.into_oxc(ctx)),
                                return_type: None,
                                scope_id: None.into(),
                            },
                            ctx.allocator,
                        ))
                    }
                }
            }
        }
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Box<'c, FunctionBody<'c>>>
    for oxidescript::parser::ast::Block
{
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Box<'c, FunctionBody<'c>> {
        println!("compiling block to function body");
        oxc::allocator::Box::new_in(
            AstBuilder::new(ctx.allocator).function_body(
                Span::new(0, 0),
                oxc::allocator::Vec::new_in(ctx.allocator),
                self.into_oxc(ctx),
            ),
            ctx.allocator,
        )
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Vec<'c, Statement<'c>>> for oxidescript::parser::ast::Block {
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Vec<'c, Statement<'c>> {
        println!("compiling block to vec of statements");
        let statements = self
            .statements
            .into_iter()
            .map(|statement| statement.into_oxc(ctx));
        let return_value = self.return_value.map(|return_value| {
            AstBuilder::new(ctx.allocator)
                .statement_return(Span::new(0, 0), Some(return_value.into_oxc(ctx)))
        });
        let mut vec = oxc::allocator::Vec::from_iter_in(statements, ctx.allocator);
        if let Some(return_value) = return_value {
            vec.push(return_value);
        }
        vec
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::Block {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        println!("compiling block to expression");
        let callee = AstBuilder::new(ctx.allocator).expression_arrow_function(
            Span::new(0, 0),
            false,
            false,
            None::<TSTypeParameterDeclaration>,
            AstBuilder::new(ctx.allocator).formal_parameters(
                Span::new(0, 0),
                oxc::ast::ast::FormalParameterKind::FormalParameter,
                oxc::allocator::Vec::new_in(ctx.allocator),
                None::<BindingRestElement>,
            ),
            None::<TSTypeAnnotation>,
            <Self as IntoOxc<oxc::allocator::Box<FunctionBody>>>::into_oxc(self, ctx),
        );
        AstBuilder::new(ctx.allocator).expression_call(
            Span::new(0, 0),
            callee,
            None::<TSTypeParameterInstantiation>,
            oxc::allocator::Vec::new_in(ctx.allocator),
            false,
        )
    }
}

impl<'c> IntoOxc<'c, Expression<'c>> for oxidescript::parser::ast::Expression {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Expression<'c> {
        println!("compiling expression to expression");
        match self {
            oxidescript::parser::ast::Expression::IdentifierExpression(ident) => {
                ident.into_oxc(ctx)
            }
            oxidescript::parser::ast::Expression::LiteralExpression(literal) => {
                literal.into_oxc(ctx)
            }
            oxidescript::parser::ast::Expression::UnaryExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::InfixExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::ArrayExpression(exprs) => AstBuilder::new(
                ctx.allocator,
            )
            .expression_array(Span::new(0, 0), exprs.into_oxc(ctx), None),
            oxidescript::parser::ast::Expression::IfExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::BlockExpression(block) => todo!(),
            oxidescript::parser::ast::Expression::CallExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::IndexExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::MemberAccessExpression(expr) => todo!(),
        }
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Vec<'c, ArrayExpressionElement<'c>>>
    for Vec<oxidescript::parser::ast::Expression>
{
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Vec<'c, ArrayExpressionElement<'c>> {
        AstBuilder::new(ctx.allocator)
            .vec_from_iter(self.into_iter().map(|expr| expr.into_oxc(ctx)))
    }
}

impl<'c> IntoOxc<'c, oxc::allocator::Vec<'c, Argument<'c>>>
    for Vec<oxidescript::parser::ast::Expression>
{
    fn into_oxc(
        self,
        ctx: &'c JavascriptCompilerContext<'c>,
    ) -> oxc::allocator::Vec<'c, Argument<'c>> {
        AstBuilder::new(ctx.allocator)
            .vec_from_iter(self.into_iter().map(|expr| expr.into_oxc(ctx)))
    }
}

impl<'c> IntoOxc<'c, ArrayExpressionElement<'c>> for oxidescript::parser::ast::Expression {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> ArrayExpressionElement<'c> {
        match self {
            oxidescript::parser::ast::Expression::IdentifierExpression(ident) => {
                ArrayExpressionElement::Identifier(oxc::allocator::Box::new_in(
                    ident.into_oxc(ctx),
                    ctx.allocator,
                ))
            }
            oxidescript::parser::ast::Expression::LiteralExpression(literal) => todo!(),
            oxidescript::parser::ast::Expression::UnaryExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::InfixExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::ArrayExpression(exprs) => todo!(),
            oxidescript::parser::ast::Expression::IfExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::BlockExpression(block) => todo!(),
            oxidescript::parser::ast::Expression::CallExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::IndexExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::MemberAccessExpression(expr) => todo!(),
        }
    }
}

impl<'c> IntoOxc<'c, Argument<'c>> for oxidescript::parser::ast::Expression {
    fn into_oxc(self, ctx: &'c JavascriptCompilerContext<'c>) -> Argument<'c> {
        match self {
            oxidescript::parser::ast::Expression::IdentifierExpression(ident) => {
                Argument::Identifier(oxc::allocator::Box::new_in(
                    ident.into_oxc(ctx),
                    ctx.allocator,
                ))
            }
            oxidescript::parser::ast::Expression::LiteralExpression(literal) => {
                literal.into_oxc(ctx)
            }
            oxidescript::parser::ast::Expression::UnaryExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::InfixExpression(expr) => expr.into_oxc(ctx),
            oxidescript::parser::ast::Expression::ArrayExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::IfExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::BlockExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::CallExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::IndexExpression(expr) => todo!(),
            oxidescript::parser::ast::Expression::MemberAccessExpression(expr) => todo!(),
        }
    }
}
