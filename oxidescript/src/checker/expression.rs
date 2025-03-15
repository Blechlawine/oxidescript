use crate::parser::ast::{
    CallExpr, ElseIfExpr, Expression, ForExpr, IfExpr, InfixExpr, InfixOperator, Literal,
    MemberAccessExpr, UnaryExpr, UnaryOperator,
};

use super::{Check, CheckContext, VariableType};

impl Check for Expression {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        match self {
            Expression::PathExpression(path) => ctx.resolve_variable(path).r#type,
            Expression::LiteralExpression(literal) => literal.check(ctx),
            Expression::UnaryExpression(unary_expr) => unary_expr.check(ctx),
            Expression::InfixExpression(infix_expr) => infix_expr.check(ctx),
            Expression::ArrayExpression(vec) => todo!("array infer_type"),
            Expression::IfExpression(if_expr) => if_expr.check(ctx),
            Expression::ForExpression(for_expr) => for_expr.check(ctx),
            Expression::BlockExpression(block) => todo!("block infer_type"),
            Expression::CallExpression(call_expr) => call_expr.check(ctx),
            Expression::IndexExpression(index_expr) => todo!("index infer_type"),
            Expression::MemberAccessExpression(member_access_expr) => member_access_expr.check(ctx),
        }
    }
}

impl Check for Literal {
    fn check(&self, _: &CheckContext) -> VariableType {
        match self {
            Literal::StringLiteral(_) => VariableType::String,
            Literal::NumberLiteral(_) => VariableType::Number,
            Literal::BooleanLiteral(_) => VariableType::Bool,
        }
    }
}

impl Check for UnaryExpr {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        let rhs_type = self.rhs.check(ctx);
        match (&self.op, &rhs_type) {
            (UnaryOperator::LogicalNot, VariableType::Bool) => VariableType::Bool,
            (UnaryOperator::BitwiseNot, VariableType::Number) => VariableType::Number,
            (UnaryOperator::Minus, VariableType::Number) => VariableType::Number,
            (UnaryOperator::Plus, VariableType::Number) => VariableType::Number,
            _ => panic!("Invalid operator for type {rhs_type}"),
        }
    }
}

impl Check for InfixExpr {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        let lhs_type = self.lhs.check(ctx);
        let rhs_type = self.rhs.check(ctx);
        match (&lhs_type, &rhs_type) {
            (VariableType::String, VariableType::String) => match self.op {
                InfixOperator::Equal
                | InfixOperator::NotEqual
                | InfixOperator::GreaterThan
                | InfixOperator::LessThan
                | InfixOperator::GreaterThanEqual
                | InfixOperator::LessThanEqual => VariableType::Bool,
                InfixOperator::Plus => VariableType::String,
                _ => {
                    panic!("Invalid operator for type String")
                }
            },
            (VariableType::Number, VariableType::Number) => match self.op {
                InfixOperator::Equal
                | InfixOperator::NotEqual
                | InfixOperator::GreaterThan
                | InfixOperator::LessThan
                | InfixOperator::GreaterThanEqual
                | InfixOperator::LessThanEqual => VariableType::Bool,
                InfixOperator::Plus
                | InfixOperator::Minus
                | InfixOperator::Multiply
                | InfixOperator::Divide
                | InfixOperator::Modulo => VariableType::Number,
                InfixOperator::BitwiseOr
                | InfixOperator::BitwiseXor
                | InfixOperator::BitwiseAnd
                | InfixOperator::BitwiseLeftShift
                | InfixOperator::BitwiseRightShift => VariableType::Number,
                _ => {
                    panic!("Invalid operator for type number")
                }
            },
            (VariableType::Bool, VariableType::Bool) => match self.op {
                InfixOperator::Equal
                | InfixOperator::NotEqual
                | InfixOperator::GreaterThan
                | InfixOperator::LessThan
                | InfixOperator::GreaterThanEqual
                | InfixOperator::LessThanEqual
                | InfixOperator::LogicalOr
                | InfixOperator::LogicalAnd => VariableType::Bool,
                _ => panic!("Invalid operator for type bool"),
            },
            (VariableType::Struct { .. }, VariableType::Struct { .. }) => match self.op {
                InfixOperator::Equal | InfixOperator::NotEqual => VariableType::Bool,
                _ => panic!("Invalid operator for type Struct"),
            },
            (VariableType::Vec(_), VariableType::Vec(_)) => match self.op {
                InfixOperator::Equal | InfixOperator::NotEqual => VariableType::Bool,
                _ => panic!("Invalid operator for type Vec"),
            },
            (VariableType::Void, VariableType::Void) => VariableType::Void,
            _ => panic!("Invalid type comparison: {lhs_type} {} {rhs_type}", self.op),
        }
    }
}

impl Check for MemberAccessExpr {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        let lhs_type = self.lhs.check(ctx);
        if let VariableType::Struct { fields } = lhs_type {
            if let Some(field_type) = fields.get(&self.ident.0) {
                field_type.clone()
            } else {
                panic!("Missing field {}", self.ident.0)
            }
        } else {
            panic!("Cannot index type other than struct: {}", self.ident.0)
        }
    }
}

impl Check for ForExpr {
    fn check(&self, _: &CheckContext) -> VariableType {
        if self.body.return_value.is_none() {
            VariableType::Void
        } else {
            todo!("for expr infer type")
        }
    }
}

impl Check for IfExpr {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        let condition_type = self.condition.check(ctx);
        if condition_type != VariableType::Bool {
            panic!("Expected bool, found: {condition_type}");
        }
        let then_type = self.then_block.check(ctx);
        if !self
            .else_if_blocks
            .iter()
            .map(|b| b.check(ctx))
            .all(|t| t == then_type)
        {
            panic!("Expected {then_type}");
        }
        let else_type = self.else_block.as_ref().map(|b| b.check(ctx));
        if let Some(else_type) = else_type {
            if else_type != then_type {
                panic!("Expected {then_type}, found: {else_type}");
            }
        }
        then_type
    }
}

impl Check for ElseIfExpr {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        let condition_type = self.condition.check(ctx);
        if condition_type != VariableType::Bool {
            panic!("Expected bool, found: {condition_type}");
        }
        self.then_block.check(ctx)
    }
}

impl Check for CallExpr {
    fn check(&self, ctx: &CheckContext) -> VariableType {
        let lhs_type = self.lhs.check(ctx);
        if let VariableType::Function {
            parameters,
            return_type,
        } = lhs_type
        {
            if !self
                .arguments
                .iter()
                .map(|a| a.check(ctx))
                .zip(parameters)
                .all(|(given, expected)| given == expected)
            {
                panic!("Invalid argument type");
            }
            *return_type
        } else {
            panic!("Cannot call non-function type: {lhs_type}");
        }
    }
}
