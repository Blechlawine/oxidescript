use std::cell::RefCell;

use crate::{
    loader::ParsedSourceTree,
    parser::ast::{
        Block, CallExpr, Declaration, Expression, ForExpr, FunctionDecl, Identifier, IfExpr,
        IndexExpr, InfixExpr, MemberAccessExpr, ModuleDeclaration, Path, Program, Statement,
        StructDecl, TypeExpression, UnaryExpr, Use,
    },
    scope::{ParentScopes, Scope},
    symbols::SymbolTable,
};

/// the resolver is responsible for resolving all the symbols and scopes in a program
#[derive(Default)]
pub struct Resolver<'src> {
    pub scopes: RefCell<ParentScopes>,
    pub symbols: RefCell<SymbolTable<'src>>,
    pub tree: ParsedSourceTree<'src>,
}

impl Resolver<'_> {
    pub fn resolve_program<'src>(&self) -> Program<'src> {
        let scope = self.scopes.borrow().global_scope();
        for (module, source) in self.tree.iter() {
            self.resolve_module(module, source, scope);
        }
        todo!("return main module from resolver::resolve_program")
        // self.resolve_vec_of_statements(source, scope);
    }

    fn resolve_module(&self, module: &Path, source: &[Statement], scope: Scope) {
        let scope = self.scopes.borrow_mut().new_scope();
        // self.symbols.insert(Symbol {
        //     id: self.symbols.next_symbol_id(),
        //     symbol_type: SymbolType::Module,
        //     identifier: module,
        //     scope,
        //     r#type: None,
        // });
        self.resolve_vec_of_statements(source, scope);
    }

    fn resolve_vec_of_statements(&self, statements: &[Statement], scope: Scope) {
        for statement in statements {
            self.resolve_statement(statement, scope);
        }
    }

    fn resolve_statement(&self, statement: &Statement, scope: Scope) {
        match statement {
            Statement::ExpressionStatement { expression, .. } => {
                self.resolve_expression(expression, scope)
            }
            Statement::DeclarationStatement(declaration) => {
                self.resolve_declaration(declaration, scope)
            }
        }
    }

    fn resolve_expression(&self, expression: &Expression, scope: Scope) {
        match expression {
            Expression::PathExpression(path) => self.resolve_path(path, scope),
            Expression::LiteralExpression(_) => {}
            Expression::UnaryExpression(unary_expr) => self.resolve_unary_expr(unary_expr, scope),
            Expression::InfixExpression(infix_expr) => self.resolve_infix_expr(infix_expr, scope),
            Expression::ArrayExpression(expressions) => {
                self.resolve_vec_of_expressions(expressions, scope)
            }
            Expression::IfExpression(if_expr) => self.resolve_if_expr(if_expr, scope),
            Expression::ForExpression(for_expr) => self.resolve_for_expr(for_expr, scope),
            Expression::BlockExpression(block) => self.resolve_block(&block, scope),
            Expression::CallExpression(call_expr) => self.resolve_call_expr(call_expr, scope),
            Expression::IndexExpression(index_expr) => self.resolve_index_expr(index_expr, scope),
            Expression::MemberAccessExpression(member_access_expr) => {
                self.resolve_member_access_expr(member_access_expr, scope)
            }
        }
    }

    fn resolve_path(&self, path: &Path, scope: Scope) {
        todo!()
    }

    fn resolve_unary_expr(&self, unary_expr: &UnaryExpr, scope: Scope) {
        self.resolve_expression(&unary_expr.rhs, scope);
    }

    fn resolve_infix_expr(&self, infix_expr: &InfixExpr, scope: Scope) {
        self.resolve_expression(&infix_expr.lhs, scope);
        self.resolve_expression(&infix_expr.rhs, scope);
    }

    fn resolve_vec_of_expressions(&self, expressions: &[Expression], scope: Scope) {
        for expression in expressions {
            self.resolve_expression(expression, scope);
        }
    }

    fn resolve_if_expr(&self, if_expr: &IfExpr, scope: Scope) {
        self.resolve_expression(&if_expr.condition, scope);
        // we don't need to make a new scope here, because the block already does that
        self.resolve_block(&if_expr.then_block, scope);
        for else_if in if_expr.else_if_blocks.iter() {
            self.resolve_expression(&else_if.condition, scope);
            self.resolve_block(&else_if.then_block, scope);
        }
        if let Some(else_block) = if_expr.else_block.as_ref() {
            self.resolve_block(&else_block, scope);
        }
    }

    fn resolve_for_expr(&self, for_expr: &ForExpr, scope: Scope) {
        self.resolve_expression(&for_expr.rhs, scope);
        let for_scope = self.scopes.borrow_mut().new_scope_with_parent(scope);
        self.resolve_identifier(
            &for_expr.lhs,
            ResolutionType::Definition(DefinitionType::Const),
            for_scope,
        );
    }

    fn resolve_block(&self, block: &Block, scope: Scope) {
        let block_scope = self.scopes.borrow_mut().new_scope_with_parent(scope);
        self.resolve_vec_of_statements(&block.statements, block_scope);
    }

    fn resolve_call_expr(&self, call_expr: &CallExpr, scope: Scope) {
        self.resolve_expression(&call_expr.lhs, scope);
        self.resolve_vec_of_expressions(&call_expr.arguments, scope);
    }

    fn resolve_index_expr(&self, index_expr: &IndexExpr, scope: Scope) {
        self.resolve_expression(&index_expr.lhs, scope);
        self.resolve_expression(&index_expr.index, scope);
    }

    fn resolve_member_access_expr(&self, member_access_expr: &MemberAccessExpr, scope: Scope) {
        self.resolve_expression(&member_access_expr.lhs, scope);
    }

    fn resolve_declaration(&self, declaration: &Declaration, scope: Scope) {
        match declaration {
            Declaration::ConstDeclaration(identifier, expression) => {
                self.resolve_expression(expression, scope);
                self.resolve_identifier(
                    identifier,
                    ResolutionType::Definition(DefinitionType::Const),
                    scope,
                );
            }
            Declaration::LetDeclaration(identifier, expression) => {
                self.resolve_expression(expression, scope);
                self.resolve_identifier(
                    identifier,
                    ResolutionType::Definition(DefinitionType::Let),
                    scope,
                );
            }
            Declaration::FunctionDeclaration(function_decl) => {
                self.resolve_function_decl(function_decl, scope)
            }
            Declaration::StructDeclaration(struct_decl) => {
                self.resolve_struct_decl(struct_decl, scope)
            }
            Declaration::ModDeclaration(module_declaration) => {
                self.resolve_module_declaration(module_declaration, scope)
            }
            Declaration::UseDeclaration(r#use) => self.resolve_use(r#use, scope),
        }
    }

    fn resolve_struct_decl(&self, struct_decl: &StructDecl, scope: Scope) {
        self.resolve_identifier(
            &struct_decl.ident,
            ResolutionType::Definition(DefinitionType::Struct),
            scope,
        );
        for field in struct_decl.fields.iter() {
            self.resolve_type_expression(&field.r#type, scope);
        }
    }

    fn resolve_type_expression(&self, r#type: &TypeExpression, scope: Scope) {
        match r#type {
            TypeExpression::Path(path) => self.resolve_path(path, scope),
        }
    }

    fn resolve_module_declaration(&self, module_declaration: &ModuleDeclaration, scope: Scope) {
        match module_declaration {
            ModuleDeclaration::Extern { path, content } => todo!(),
            ModuleDeclaration::Intern { path, content } => todo!(),
        }
    }

    fn resolve_use(&self, r#use: &Use, scope: Scope) {
        todo!()
    }

    fn resolve_function_decl(&self, function_decl: &FunctionDecl, scope: Scope) {
        self.resolve_identifier(
            &function_decl.name,
            ResolutionType::Definition(DefinitionType::Function),
            scope,
        );
        let function_scope = self.scopes.borrow_mut().new_scope_with_parent(scope);
        for parameter in function_decl.parameters.iter() {
            self.resolve_identifier(
                &parameter.name,
                ResolutionType::Definition(DefinitionType::Const),
                function_scope,
            );
            self.resolve_type_expression(&parameter.r#type, scope);
        }
        if let Some(return_type) = function_decl.return_type.as_ref() {
            self.resolve_type_expression(&return_type, scope);
        }
        if let Some(body) = function_decl.body.as_ref() {
            self.resolve_block(&body, function_scope);
        }
    }

    fn resolve_identifier(
        &self,
        identifier: &Identifier,
        resolution_type: ResolutionType,
        scope: Scope,
    ) {
        todo!()
    }
}

enum ResolutionType {
    Definition(DefinitionType),
    Usage,
}

enum DefinitionType {
    Const,
    Let,
    Struct,
    Function,
}
