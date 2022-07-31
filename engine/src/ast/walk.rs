use super::ast::{BlockNode, StatementNode, StatemenIndentWrapper, StatementNodeKind, ExpressionNode, FunctionDeclarationNode, TypeDeclarationNode, StructStatementNode, VariableDeclarationNode, SkippedTokens, SkippedTokenNode, ExpressionKind, AtomicExpressionNode, UnaryExpressionNode, BinaryExpressionNode, LogicalExpressionNode};

pub trait Visitor {
    fn visit_block(&self, block: &BlockNode) {
        let block_node = &block.0.as_ref().borrow();
        for stmt in &*block_node.stmts.as_ref().borrow() {
            match stmt {
                StatemenIndentWrapper::CORRECTLY_INDENTED(stmt)                 => self.visit_statement(stmt),
                StatemenIndentWrapper::INCORRECTLY_INDENTED((stmt, _))          => self.visit_statement(stmt),
                StatemenIndentWrapper::LEADING_SKIPPED_TOKENS(skipped_tokens)   => {
                    self.visit_skipped_tokens(skipped_tokens);
                },
                StatemenIndentWrapper::TRAILING_SKIPPED_TOKENS(skipped_tokens)  => {
                    self.visit_skipped_tokens(skipped_tokens);
                },
                StatemenIndentWrapper::EXTRA_NEWLINES(skipped_tokens)           => {
                    self.visit_skipped_tokens(skipped_tokens);
                }
            }
        }
    }

    fn visit_statement(&self, stmt: &StatementNode) {
        let stmt_node = &stmt.0.as_ref().borrow();
        match &stmt_node.kind {
            StatementNodeKind::EXPRESSION(expr)                              => {
                self.visit_expression(expr);
            }
            StatementNodeKind::VARIABLE_DECLARATION(variable_decl)  => {
                self.visit_variable_decl(variable_decl);
            }
            StatementNodeKind::FUNCTION_DECLARATION(func_decl)      => {
                self.visit_func_decl(func_decl);
            }
            StatementNodeKind::TYPE_DECLARATION(type_decl)              => {
                self.visit_type_decl(type_decl);
            }
            StatementNodeKind::STRUCT_STATEMENT(struct_stmt)            => {
                self.visit_struct_stmt(struct_stmt);
            }
            _ => {}
        }
    }

    fn visit_skipped_tokens(&self, skipped_tokens: &SkippedTokens) {
        let skipped_tokens_node = skipped_tokens.0.as_ref().borrow();
        for skipped_token in skipped_tokens_node.skipped_tokens.as_ref() {
            self.visit_skipped_token(skipped_token)
        }
    }

    fn visit_skipped_token(&self, skipped_token: &SkippedTokenNode) {
        todo!()
    }

    fn visit_expression(&self, expr: &ExpressionNode) {
        let expr_node = expr.0.as_ref().borrow();
        match &expr_node.kind {
            ExpressionKind::ATOMIC(atomic_expr)     => self.visit_atomic_expr(atomic_expr),
            ExpressionKind::UNARY(unary_expr)        => self.visit_unary_expr(unary_expr),
            ExpressionKind::BINARY(binary_expr)     => self.visit_binary_expr(binary_expr),
            ExpressionKind::LOGICAL(logical_expr)  => self.visit_logical_expr(logical_expr),
            _ => {}
        }
    }

    fn visit_atomic_expr(&self, atomic_expr: &AtomicExpressionNode) {
        todo!()
    }

    fn visit_unary_expr(&self, unary_expr: &UnaryExpressionNode) {
        todo!()
    }

    fn visit_binary_expr(&self, binary_expr: &BinaryExpressionNode) {
        todo!()
    }

    fn visit_logical_expr(&self, logical_expr: &LogicalExpressionNode) {
        todo!()
    }

    fn visit_variable_decl(&self, variable_decl: &VariableDeclarationNode) {
        todo!()
    }

    fn visit_func_decl(&self, func_decl: &FunctionDeclarationNode) {
        todo!()
    }

    fn visit_type_decl(&self, type_decl: &TypeDeclarationNode) {
        todo!()
    }

    fn visit_struct_stmt(&self, struct_stmt: &StructStatementNode) {
        todo!()
    }
    // TODO - define for all other nodes
}

pub trait Node {
    fn accept<T: Visitor>(visitor: &T);
}