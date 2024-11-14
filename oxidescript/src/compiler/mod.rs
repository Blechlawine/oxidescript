use crate::parser::ast::{
    Block, Declaration, Expression, InfixOperator, Literal, Parameter, Program, Statement,
    UnaryOperator,
};

pub trait Compiler {
    fn new() -> Self;
    fn compile(&self, program: Program) -> String;
}

#[derive(Default, Debug)]
struct JavascriptCompilationOutput {
    code: String,
    semicolon_allowed: bool,
    is_block: bool,
    evaluates_to: Option<String>,
    prepend_to_statement: Vec<String>,
}

impl From<&str> for JavascriptCompilationOutput {
    fn from(value: &str) -> Self {
        JavascriptCompilationOutput {
            code: value.to_string(),
            ..Default::default()
        }
    }
}

impl FromIterator<JavascriptCompilationOutput> for JavascriptCompilationOutput {
    fn from_iter<T: IntoIterator<Item = JavascriptCompilationOutput>>(iter: T) -> Self {
        let mut code = String::new();
        let mut prepend_to_statement = Vec::new();
        for output in iter {
            code.push_str(&output.code);
            prepend_to_statement.extend(output.prepend_to_statement);
        }
        JavascriptCompilationOutput {
            code,
            prepend_to_statement,
            ..Default::default()
        }
    }
}

#[derive(Default, Debug, Eq, PartialEq)]
#[allow(dead_code)]
enum ExpressionTarget {
    FunctionArgument,
    FunctionBody,
    Index,
    #[default]
    Statement,
    Expression,
    IfCondition,
    IfThenBlock,
    ElseIfBlock,
    ElseBlock,
}

#[derive(Debug)]
struct JavascriptCompilerContext {
    indent: usize,
    expression_target: Vec<ExpressionTarget>,
    block_return_value_counter: u32,
}

impl JavascriptCompilerContext {
    fn new() -> JavascriptCompilerContext {
        JavascriptCompilerContext {
            indent: 0,
            expression_target: vec![ExpressionTarget::Statement],
            block_return_value_counter: 0,
        }
    }

    pub fn run_with(
        &mut self,
        indent: Option<usize>,
        expression_target: Option<ExpressionTarget>,
        to_compile: impl Fn(&mut JavascriptCompilerContext) -> JavascriptCompilationOutput,
    ) -> JavascriptCompilationOutput {
        let with_target = expression_target.is_some();
        if let Some(indent) = indent {
            self.indent += indent;
        }
        if let Some(expression_target) = expression_target {
            self.expression_target.push(expression_target);
        }
        let result = to_compile(self);
        if let Some(indent) = indent {
            self.indent -= indent;
        }
        if with_target {
            self.expression_target.pop();
        }
        result
    }
}

trait JavascriptCompile {
    fn compile(&self, ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput;
}

impl JavascriptCompile for Program {
    fn compile(&self, ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        self.iter()
            .map(|statement| statement.compile(ctx))
            .collect()
    }
}

impl JavascriptCompile for Statement {
    fn compile(&self, ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        let statement = match self {
            Statement::ExpressionStatement {
                expression: expr, ..
            } => expr.compile(ctx),
            Statement::DeclarationStatement(decl) => decl.compile(ctx),
        };
        let code = build_block(&statement, false, ctx);
        let prepend = statement.prepend_to_statement.join("\n");
        JavascriptCompilationOutput {
            code: format!(
                "{}{}{}{}\n",
                prepend,
                indent(ctx.indent),
                code,
                if statement.semicolon_allowed { ";" } else { "" }
            ),
            ..Default::default()
        }
    }
}

fn build_block(
    block_output: &JavascriptCompilationOutput,
    eval: bool,
    ctx: &JavascriptCompilerContext,
) -> String {
    if block_output.is_block {
        if let Some(evaluates_to) = block_output.evaluates_to.as_ref() {
            let statements = vec![
                "let return_value = undefined;".to_string(),
                "{".to_string(),
                format!(
                    "{}{}return_value = {};",
                    block_output.code.clone(),
                    indent(ctx.indent + 1),
                    evaluates_to
                ),
                "}".to_string(),
                eval.then_some("return_value")
                    .unwrap_or_default()
                    .to_string(),
            ]
            .into_iter()
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();
            statements.join("\n")
        } else {
            block_output.code.clone()
        }
    } else {
        block_output.code.clone()
    }
}

fn indent(width: usize) -> String {
    " ".repeat(width * 4)
}

impl JavascriptCompile for Expression {
    fn compile(&self, ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        match self {
            Expression::IdentifierExpression(ident) => JavascriptCompilationOutput {
                code: ident.0.clone(),
                semicolon_allowed: true,
                ..Default::default()
            },
            Expression::LiteralExpression(literal) => literal.compile(ctx),
            Expression::UnaryExpression(op, arg) => {
                let op = op.compile(ctx);
                let arg = arg.compile(ctx);
                JavascriptCompilationOutput {
                    code: format!("{}{}", op.code, arg.code),
                    semicolon_allowed: arg.semicolon_allowed,
                    ..Default::default()
                }
            }
            Expression::InfixExpression(op, arg0, arg1) => {
                let op = op.compile(ctx);
                let arg0 = arg0.compile(ctx);
                let arg1 = arg1.compile(ctx);
                JavascriptCompilationOutput {
                    code: format!("{} {} {}", arg0.code, op.code, arg1.code),
                    semicolon_allowed: arg1.semicolon_allowed,
                    ..Default::default()
                }
            }
            Expression::ArrayExpression(exprs) => {
                let exprs = exprs.compile(ctx);
                JavascriptCompilationOutput {
                    code: format!("[{}]", exprs.code),
                    semicolon_allowed: true,
                    ..Default::default()
                }
            }
            Expression::CallExpression(ident, args) => {
                let ident = ident.compile(ctx);
                ctx.expression_target
                    .push(ExpressionTarget::FunctionArgument);
                let args = args.compile(ctx);
                ctx.expression_target.pop();
                JavascriptCompilationOutput {
                    code: format!("{}({})", ident.code, args.code),
                    semicolon_allowed: true,
                    prepend_to_statement: args.prepend_to_statement,
                    ..Default::default()
                }
            }
            Expression::MemberAccessExpression(expr, ident) => {
                let expr = expr.compile(ctx);
                let ident = ident.0.clone();
                JavascriptCompilationOutput {
                    code: format!("{}.{}", expr.code, ident),
                    semicolon_allowed: true,
                    prepend_to_statement: expr.prepend_to_statement,
                    ..Default::default()
                }
            }
            Expression::IndexExpression(expr, index_expr) => {
                let expr_compiled = expr.compile(ctx);
                let expr = build_block(&expr_compiled, true, ctx);
                ctx.expression_target.push(ExpressionTarget::Index);
                let index_expr = index_expr.compile(ctx);
                ctx.expression_target.pop();
                JavascriptCompilationOutput {
                    code: format!("{}[{}]", expr, index_expr.code),
                    semicolon_allowed: true,
                    prepend_to_statement: expr_compiled.prepend_to_statement,
                    ..Default::default()
                }
            }
            Expression::BlockExpression(block) => {
                let return_value = block.return_value.as_ref().map(|rv| rv.compile(ctx));
                let return_value_name = format!("return_value{}", ctx.block_return_value_counter);
                ctx.block_return_value_counter += 1;
                if block.statements.is_empty() {
                    // directly output the return value, because the block things are unnecessary
                    if matches!(
                        ctx.expression_target.last(),
                        Some(
                            ExpressionTarget::Index
                                | ExpressionTarget::FunctionArgument
                                | ExpressionTarget::IfCondition
                        )
                    ) {
                        return JavascriptCompilationOutput {
                            code: return_value.map(|rv| rv.code).unwrap_or_default(),
                            ..Default::default()
                        };
                    }
                    return JavascriptCompilationOutput {
                        code: format!(
                            "let {return_value_name} = {};",
                            return_value
                                .map(|rv| rv.code)
                                .unwrap_or("undefined".to_string())
                        ),
                        ..Default::default()
                    };
                }
                ctx.indent += 1;
                let block_statements = block.statements.compile(ctx);
                ctx.indent -= 1;
                if matches!(
                    ctx.expression_target.last(),
                    Some(
                        ExpressionTarget::Index
                            | ExpressionTarget::FunctionArgument
                            | ExpressionTarget::IfCondition
                    )
                ) {
                    let prepend_code = vec![
                        format!("let {return_value_name} = undefined;"),
                        "{".to_string(),
                        format!(
                            "{}{}{};",
                            block_statements.code,
                            indent(ctx.indent + 1),
                            return_value
                                .map(|rv| format!("{return_value_name} = {}", rv.code))
                                .unwrap_or_default()
                        ),
                        "}\n".to_string(),
                    ]
                    .into_iter()
                    .filter(|s| !s.is_empty())
                    .collect::<Vec<_>>()
                    .join("\n");
                    JavascriptCompilationOutput {
                        code: return_value_name.to_string(),
                        prepend_to_statement: vec![prepend_code],
                        semicolon_allowed: true,
                        ..Default::default()
                    }
                } else {
                    JavascriptCompilationOutput {
                        code: block_statements.code,
                        semicolon_allowed: false,
                        is_block: true,
                        evaluates_to: return_value.map(|rv| rv.code),
                        ..Default::default()
                    }
                }
            }
            Expression::IfExpression {
                condition,
                then_block,
                else_if_blocks,
                else_block,
            } => {
                // TODO: handle complex usages of if expressions, like when used as a value
                // somewhere
                let condition = ctx.run_with(None, Some(ExpressionTarget::IfCondition), |c| {
                    condition.compile(c)
                });
                let then_block = ctx.run_with(None, Some(ExpressionTarget::IfThenBlock), |c| {
                    then_block.compile(c)
                });
                let else_if_blocks = ctx.run_with(None, Some(ExpressionTarget::ElseIfBlock), |c| {
                    else_if_blocks
                        .iter()
                        .map(|(condition, block)| {
                            let condition = condition.compile(c);
                            let block = block.compile(c);
                            JavascriptCompilationOutput {
                                code: format!(" else if ({}) {}", condition.code, block.code),
                                ..Default::default()
                            }
                        })
                        .collect()
                });
                let else_block = else_block.as_ref().map(|e| {
                    ctx.run_with(None, Some(ExpressionTarget::ElseBlock), |c| e.compile(c))
                });
                let mut code = format!(
                    "if ({}) {}{}",
                    condition.code, then_block.code, else_if_blocks.code
                );
                if let Some(else_block) = else_block {
                    code.push_str(format!(" else {}", else_block.code).as_str())
                }
                JavascriptCompilationOutput {
                    code,
                    prepend_to_statement: condition.prepend_to_statement,
                    semicolon_allowed: false,
                    ..Default::default()
                }
            }
        }
    }
}

impl JavascriptCompile for Literal {
    fn compile(&self, _ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        match self {
            Literal::NumberLiteral(n) => JavascriptCompilationOutput {
                code: n.to_string(),
                semicolon_allowed: true,
                ..Default::default()
            },
            Literal::StringLiteral(s) => JavascriptCompilationOutput {
                code: format!("\"{}\"", s),
                semicolon_allowed: true,
                ..Default::default()
            },
            Literal::BooleanLiteral(b) => JavascriptCompilationOutput {
                code: b.to_string(),
                semicolon_allowed: true,
                ..Default::default()
            },
        }
    }
}

impl JavascriptCompile for UnaryOperator {
    fn compile(&self, _ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        match self {
            UnaryOperator::LogicalNot => "!".into(),
            UnaryOperator::BitwiseNot => "~".into(),
            UnaryOperator::Minus => "-".into(),
            UnaryOperator::Plus => "+".into(),
        }
    }
}

impl JavascriptCompile for InfixOperator {
    fn compile(&self, _ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        match self {
            InfixOperator::Equal => "==".into(),
            InfixOperator::NotEqual => "!=".into(),
            InfixOperator::GreaterThan => ">".into(),
            InfixOperator::LessThan => "<".into(),
            InfixOperator::GreaterThanEqual => ">=".into(),
            InfixOperator::LessThanEqual => "<=".into(),
            InfixOperator::Plus => "+".into(),
            InfixOperator::Minus => "-".into(),
            InfixOperator::Multiply => "*".into(),
            InfixOperator::Divide => "/".into(),
            InfixOperator::Modulo => "%".into(),
            InfixOperator::LogicalOr => "||".into(),
            InfixOperator::LogicalAnd => "&&".into(),
            InfixOperator::BitwiseOr => "|".into(),
            InfixOperator::BitwiseXor => "^".into(),
            InfixOperator::BitwiseAnd => "&".into(),
            InfixOperator::BitwiseLeftShift => "<<".into(),
            InfixOperator::BitwiseRightShift => ">>".into(),
        }
    }
}

impl JavascriptCompile for Declaration {
    fn compile(&self, ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        match self {
            Declaration::ConstDeclaration(ident, expr) => {
                let expr = expr.compile(ctx);
                let ident = ident.0.clone();
                JavascriptCompilationOutput {
                    code: format!("const {} = {};", ident, expr.code),
                    prepend_to_statement: expr.prepend_to_statement,
                    ..Default::default()
                }
            }
            Declaration::LetDeclaration(ident, expr) => {
                let expr = expr.compile(ctx);
                let ident = ident.0.clone();
                JavascriptCompilationOutput {
                    code: format!("let {} = {};", ident, expr.code),
                    prepend_to_statement: expr.prepend_to_statement,
                    ..Default::default()
                }
            }
            Declaration::FunctionDeclaration {
                name,
                parameters,
                body,
            } => {
                let parameters = parameters.compile(ctx);
                let name = name.0.clone();
                let body = ctx.run_with(None, Some(ExpressionTarget::FunctionBody), |c| {
                    body.compile(c)
                });
                JavascriptCompilationOutput {
                    code: format!("function {}({}) {}", name, parameters.code, body.code),
                    ..Default::default()
                }
            }
        }
    }
}

impl JavascriptCompile for Vec<Parameter> {
    fn compile(&self, ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        JavascriptCompilationOutput {
            code: self
                .iter()
                .map(|p| p.compile(ctx).code)
                .collect::<Vec<_>>()
                .join(", ")
                .to_string(),
            ..Default::default()
        }
    }
}

impl JavascriptCompile for Parameter {
    fn compile(&self, _ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        JavascriptCompilationOutput {
            code: self.name.0.clone(),
            ..Default::default()
        }
    }
}

impl JavascriptCompile for Vec<Expression> {
    fn compile(&self, ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        let expressions = self.iter().map(|e| e.compile(ctx)).collect::<Vec<_>>();
        JavascriptCompilationOutput {
            code: expressions
                .iter()
                .map(|e| e.code.clone())
                .collect::<Vec<_>>()
                .join(", ")
                .to_string(),
            prepend_to_statement: expressions
                .iter()
                .flat_map(|e| e.prepend_to_statement.clone())
                .collect(),
            ..Default::default()
        }
    }
}

impl JavascriptCompile for Block {
    fn compile(&self, ctx: &mut JavascriptCompilerContext) -> JavascriptCompilationOutput {
        let statements = self
            .statements
            .iter()
            .map(|statement| {
                ctx.indent += 1;
                let statement = statement.compile(ctx);
                ctx.indent -= 1;
                statement
            })
            .collect::<JavascriptCompilationOutput>();
        let return_value = self
            .return_value
            .as_ref()
            .map(|return_value| return_value.compile(ctx))
            .map(|return_value| match ctx.expression_target.last() {
                Some(
                    ExpressionTarget::IfThenBlock
                    | ExpressionTarget::ElseIfBlock
                    | ExpressionTarget::ElseBlock,
                ) => {
                    format!(
                        "{}return_value = {};\n",
                        indent(ctx.indent + 1),
                        return_value.code
                    )
                }
                Some(ExpressionTarget::FunctionBody) => {
                    format!("{}return {};\n", indent(ctx.indent + 1), return_value.code)
                }
                _ => todo!(),
            })
            .unwrap_or("".into());
        JavascriptCompilationOutput {
            prepend_to_statement: vec![format!("let return_value;")],
            code: format!("{{\n{}{}}}", statements.code, return_value),
            ..Default::default()
        }
    }
}

pub struct JavascriptCompiler;

impl JavascriptCompiler {
    pub fn compile(program: Program) -> String {
        let mut ctx = JavascriptCompilerContext::new();
        let compiled = program.compile(&mut ctx);
        compiled.code
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::{tokens::Tokens, Lexer},
        parser::Parser,
    };

    use super::*;

    fn assert_input_with_javascript(input: &[u8], expected_results: &'static str) {
        let (_, r) = Lexer::lex_tokens(input).unwrap();
        let tokens = Tokens::new(&r);
        let (_, result) = Parser::parse(tokens).unwrap();
        let mut ctx = JavascriptCompilerContext::new();
        let compiled = result.compile(&mut ctx);
        assert_eq!(compiled.code, expected_results);
    }

    #[test]
    fn literals() {
        let input = r#"
        5;
        "foo";
        true;"#;
        assert_input_with_javascript(input.as_bytes(), "5;\n\"foo\";\ntrue;\n");
    }

    #[test]
    fn expressions() {
        let input = r#"
        test;
        5;
        -5;
        5 + 5;
        [5, 10];
        "#;
        assert_input_with_javascript(input.as_bytes(), "test;\n5;\n-5;\n5 + 5;\n[5, 10];\n");
    }

    #[test]
    fn declarations() {
        let input = r#"
        const test = 5;
        let test = 5;
        fn test() {
            let test = 5;
        }
        fn test(foo: string, bar: number) {
            let baz = 5;
            baz
        }
        "#;
        assert_input_with_javascript(
            input.as_bytes(),
            "const test = 5;\nlet test = 5;\nfunction test() {\n    let test = 5;\n}\nfunction test(foo, bar) {\n    let baz = 5;\n    return baz;\n}\n",
        );
    }

    #[test]
    fn code_snippet() {
        let input = r#"
        fn foo(bar: number, baz: number) {
            bar + baz
        }
        foo(20, 30 - 2);
        "#;
        // TODO: add function call expression and more

        assert_input_with_javascript(
            input.as_bytes(),
            "function foo(bar, baz) {\n    return bar + baz;\n}\nfoo(20, 30 - 2);\n",
        );
    }

    #[test]
    fn block_expression_without_statements() {
        // TODO: this should be just removed from the output, since it doesn't do anything
        let input = r#"
        {
            5
        };
        "#;
        assert_input_with_javascript(input.as_bytes(), "let return_value0 = 5;\n");
    }

    #[test]
    fn block_expression_with_statements() {
        let input = r#"
        {
            const foo = 5;
            foo
        };
        "#;
        assert_input_with_javascript(
            input.as_bytes(),
            "let return_value = undefined;\n{\n    const foo = 5;\n    return_value = foo;\n}\n",
        );
    }

    #[test]
    fn if_statement() {
        let input = r#"
        if true {
            false;
        } else if false {
            "Helo";
        } else {
            true;
        }
        "#;
        assert_input_with_javascript(
            input.as_bytes(),
            "if (true) {\n    false;\n} else if (false) {\n    \"Helo\";\n} else {\n    true;\n}\n",
        )
    }
}
