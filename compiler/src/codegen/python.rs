use crate::scope::lookup::LookupResult;
use crate::scope::mangled::MangledIdentifierName;
use crate::scope::scope::ScopeIndex;
use crate::scope::semantic_db::SemanticStateDatabase;
use crate::scope::symbol::core::{ConcreteSymbolDataEntry, SymbolDataEntry, SymbolIndex};
use crate::scope::symbol::variables::VariableData;
use crate::scope::traits::AbstractSymbol;
use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, BoundedMethodKind, BoundedMethodWrapperNode, CallablePrototypeNode,
            ConditionalBlockNode, CoreAssignmentNode, CoreIdentifierInDeclNode,
            CoreIdentifierInUseNode, CoreRVariableDeclarationNode, CoreStatementIndentWrapperNode,
            CoreStatementNode, CoreTokenNode, CoreTypeDeclarationNode,
            EnumVariantExprOrClassMethodCallNode, IdentifierInDeclNode, IdentifierInUseNode,
            MatchCaseStatementNode, OkIdentifierInDeclNode, OkIdentifierInUseNode, StatementNode,
            TokenNode, TypeDeclarationNode, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::JarvilCodeHandler,
    constants::common::{FUNC_SUFFIX, TY_SUFFIX, VAR_SUFFIX},
    context,
    lexer::token::{CoreToken, Token},
    scope::symbol::types::core::UserDefinedTypeData,
};
use rustc_hash::FxHashSet;
use std::convert::TryInto;

// Utility functions
pub fn whitespaces_from_indent_level(indent_level: usize) -> String {
    let expected_indent_spaces = context::indent_spaces() * indent_level;
    " ".to_string()
        .repeat(expected_indent_spaces.try_into().unwrap())
}

pub fn trivia_from_token_node(token: &TokenNode) -> Option<&Vec<Token>> {
    match token.core_ref() {
        CoreTokenNode::Ok(ok_token_node) => match ok_token_node.core_ref().token.trivia() {
            Some(trivia) => Some(trivia),
            None => None,
        },
        _ => unreachable!(),
    }
}

pub struct PythonCodeGenerator<'ctx> {
    indent_level: usize,
    generated_code: String,
    code_handler: &'ctx JarvilCodeHandler<'ctx>,
    semantic_db: SemanticStateDatabase,
}

impl<'ctx> PythonCodeGenerator<'ctx> {
    pub fn new(
        code_handler: &'ctx JarvilCodeHandler<'ctx>,
        semantic_db: SemanticStateDatabase,
    ) -> PythonCodeGenerator {
        PythonCodeGenerator {
            indent_level: 0,
            generated_code: "".to_string(),
            code_handler,
            semantic_db,
        }
    }

    pub fn open_block(&mut self) {
        self.indent_level += 1;
    }

    pub fn close_block(&mut self) {
        self.indent_level -= 1;
    }

    pub fn generate_python_code(mut self, ast: &BlockNode) -> String {
        let code_block = ast.0.as_ref();
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        let index = match self.semantic_db.namespace_ref().lookup_in_funcs_namespace(
            ScopeIndex::global(),
            self.semantic_db.interner().intern("main"),
        ) {
            LookupResult::Ok(lookup_data) => match lookup_data
                .symbol_obj
                .symbol_index()
                .index(self.semantic_db.namespace_ref().funcs_ref())
            {
                Some(index) => index,
                None => unreachable!(),
            },
            LookupResult::Unresolved | LookupResult::NotInitialized(_) => unreachable!(),
        };
        let main_call_str = format!("\n\nmain_{}_func()", index.index());
        self.add_str_to_python_code(&main_call_str);
        self.generated_code
    }

    pub fn add_str_to_python_code(&mut self, str: &str) {
        self.generated_code.push_str(str);
    }

    pub fn add_indention_to_python_code(&mut self) {
        self.add_str_to_python_code(&whitespaces_from_indent_level(self.indent_level));
    }

    pub fn non_locals(&self, block: &BlockNode) -> &FxHashSet<MangledIdentifierName<VariableData>> {
        self.semantic_db.non_locals_ref(block)
    }

    pub fn mangled_identifier_name_in_decl(&self, identifier: &OkIdentifierInDeclNode) -> String {
        match self.semantic_db.symbol_for_identifier_in_decl(identifier) {
            Some(symbol_entry) => match symbol_entry {
                SymbolDataEntry::Variable(symbol_index) => symbol_index
                    .mangled_name(self.semantic_db.namespace_ref().variables_ref())
                    .to_string(VAR_SUFFIX, self.semantic_db.interner()),
                SymbolDataEntry::Function(symbol_index) => symbol_index
                    .mangled_name(self.semantic_db.namespace_ref().funcs_ref())
                    .to_string(FUNC_SUFFIX, self.semantic_db.interner()),
                SymbolDataEntry::Type(symbol_index) => symbol_index
                    .mangled_name(self.semantic_db.namespace_ref().types_ref())
                    .to_string(TY_SUFFIX, self.semantic_db.interner()),
                SymbolDataEntry::Interface(_) => unreachable!(),
            },
            None => identifier.token_value_str(&self.code_handler),
        }
    }

    pub fn mangled_identifier_name_in_use(&self, identifier: &OkIdentifierInUseNode) -> String {
        let Some(concrete_symbol_entry) = self.semantic_db.symbol_for_identifier_in_use(identifier)
        else {
            return identifier.token_value_str(&self.code_handler);
        };
        match concrete_symbol_entry {
            ConcreteSymbolDataEntry::Variable(concrete_symbol_index) => concrete_symbol_index
                .symbol_index()
                .mangled_name(self.semantic_db.namespace_ref().variables_ref())
                .to_string(VAR_SUFFIX, self.semantic_db.interner()),
            ConcreteSymbolDataEntry::Function(concrete_symbol_index) => concrete_symbol_index
                .symbol_index()
                .mangled_name(self.semantic_db.namespace_ref().funcs_ref())
                .to_string(FUNC_SUFFIX, self.semantic_db.interner()),
            ConcreteSymbolDataEntry::Type(concrete_symbol_index) => concrete_symbol_index
                .symbol_index()
                .mangled_name(self.semantic_db.namespace_ref().types_ref())
                .to_string(TY_SUFFIX, self.semantic_db.interner()),
            ConcreteSymbolDataEntry::Interface(_) => unreachable!(),
        }
    }

    pub fn print_token(&mut self, token: &Token) {
        let trivia = match token.trivia() {
            Some(trivia) => Some(trivia),
            None => None,
        };
        self.print_trivia(trivia);
        let token_value = token.token_value_str(&self.code_handler);
        match token.core_token() {
            CoreToken::SINGLE_LINE_COMMENT => {
                self.add_str_to_python_code("\n");
            }
            CoreToken::BLOCK_COMMENT => {
                self.add_str_to_python_code("\n");
            }
            CoreToken::ENDMARKER => (),
            CoreToken::LITERAL => {
                // Jarvil `str` can span multiple lines so this
                // translates to Python three-quotes string.
                let len = token_value.len();
                let mut critical_section = token_value[1..(len - 1)].to_string();
                critical_section.push_str("'''");
                let mut final_str = "'''".to_string();
                final_str.push_str(&critical_section);
                self.add_str_to_python_code(&final_str);
            }
            _ => {
                self.add_str_to_python_code(&token_value);
            }
        }
    }

    pub fn print_trivia(&mut self, trivia: Option<&Vec<Token>>) {
        if let Some(trivia) = trivia {
            for trivia_entry in trivia {
                self.print_token(trivia_entry);
            }
        }
    }

    pub fn print_token_node(&mut self, token: &TokenNode) {
        match token.core_ref() {
            CoreTokenNode::Ok(ok_token_node) => {
                self.walk_ok_token(ok_token_node);
            }
            CoreTokenNode::MissingTokens(_) => unreachable!(),
        }
    }

    pub fn print_token_node_without_trivia(&mut self, token: &TokenNode) {
        match token.core_ref() {
            CoreTokenNode::Ok(ok_token_node) => {
                self.add_str_to_python_code(&ok_token_node.token_value_str(&self.code_handler));
            }
            CoreTokenNode::MissingTokens(_) => unreachable!(),
        }
    }

    pub fn print_identifier_in_decl(&mut self, identifier: &IdentifierInDeclNode, is_trivia: bool) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierInDeclNode::Ok(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let token_value = self.mangled_identifier_name_in_decl(identifier);
        if is_trivia {
            let token = &identifier.0.as_ref().name.core_ref().token;
            let trivia = match token.trivia() {
                Some(trivia) => Some(trivia),
                None => None,
            };
            self.print_trivia(trivia);
        }
        self.add_str_to_python_code(&token_value);
    }

    pub fn print_identifier_in_use(&mut self, identifier: &IdentifierInUseNode, is_trivia: bool) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierInUseNode::Ok(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let token_value = self.mangled_identifier_name_in_use(identifier);
        if is_trivia {
            let token = &identifier.0.as_ref().name.core_ref().token;
            let trivia = match token.trivia() {
                Some(trivia) => Some(trivia),
                None => None,
            };
            self.print_trivia(trivia);
        }
        self.add_str_to_python_code(&token_value);
    }

    pub fn print_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        let name = &core_variable_decl.name;
        let equal = &core_variable_decl.equal;
        let r_node = &core_variable_decl.r_node;
        match r_node.core_ref() {
            CoreRVariableDeclarationNode::Expression(expr_stmt) => {
                self.print_identifier_in_decl(name, false);
                self.print_token_node(equal);
                self.walk_expr_stmt(expr_stmt);
            }
            CoreRVariableDeclarationNode::Lambda(lambda_decl) => {
                let callable_body = &lambda_decl.core_ref().body;
                self.add_str_to_python_code("def");
                self.print_identifier_in_decl(name, true);
                self.walk_callable_body(callable_body);
            }
        }
    }

    pub fn print_callable_prototype(&mut self, callable_prototype: &CallablePrototypeNode) {
        let core_callable_prototype = callable_prototype.core_ref();
        let lparen = &core_callable_prototype.lparen;
        let params = &core_callable_prototype.params;
        let rparen = &core_callable_prototype.rparen;
        self.print_token_node(lparen);
        if let Some(params) = params {
            self.walk_comma_separated_name_ty_specs(params);
        }
        self.print_token_node(rparen);
    }

    pub fn print_ty_decl(&mut self, ty_decl: &TypeDeclarationNode) {
        let core_ty_decl = ty_decl.core_ref();
        match core_ty_decl {
            CoreTypeDeclarationNode::Struct(struct_decl) => {
                let core_struct_decl = struct_decl.core_ref();
                let struct_name = &core_struct_decl.name;
                let colon = &core_struct_decl.colon;
                let block = &core_struct_decl.block;
                self.add_str_to_python_code("class");
                self.print_identifier_in_decl(struct_name, true);
                self.print_token_node(colon);
                self.walk_block(block);
            }
            CoreTypeDeclarationNode::Lambda(_) => {
                self.add_str_to_python_code("\n");
            }
            CoreTypeDeclarationNode::Enum(enum_decl) => {
                let core_enum_decl = enum_decl.core_ref();
                let enum_name = &core_enum_decl.name;
                let colon = &core_enum_decl.colon;
                self.add_str_to_python_code("class");
                self.print_identifier_in_decl(enum_name, true);
                self.print_token_node(colon);
                let constructor_str = format!(
                    "\n{}def __init__(self, index, data=None):\n{}self.index = index\n{}self.data = data\n",
                    whitespaces_from_indent_level(self.indent_level + 1),
                    whitespaces_from_indent_level(self.indent_level + 2),
                    whitespaces_from_indent_level(self.indent_level + 2)
                );
                self.add_str_to_python_code(&constructor_str);
            }
            CoreTypeDeclarationNode::MissingTokens(_) => unreachable!(),
        }
    }

    pub fn print_enum_variant_expr_or_class_method_call(
        &mut self,
        enum_variant_expr_or_class_method_call: &EnumVariantExprOrClassMethodCallNode,
        is_trivia: bool,
    ) {
        let core_enum_variant_expr_or_class_method_call =
            enum_variant_expr_or_class_method_call.core_ref();
        let ty_name = &core_enum_variant_expr_or_class_method_call.ty_name;
        let property_name = &core_enum_variant_expr_or_class_method_call.property_name;
        let params = &core_enum_variant_expr_or_class_method_call.params;
        let CoreIdentifierInUseNode::Ok(ok_ty_name) = ty_name.core_ref() else {
            return;
        };
        let Some(concrete_symbol_index) =
            self.semantic_db.ty_symbol_for_identifier_in_use(ok_ty_name)
        else {
            return;
        };
        match &self
            .semantic_db
            .ty_symbol_ref(concrete_symbol_index.symbol_index())
        {
            UserDefinedTypeData::Struct(_) => {
                self.print_identifier_in_use(ty_name, is_trivia);
                self.add_str_to_python_code(".");
                self.print_identifier_in_use(property_name, true);
                if let Some((lparen, params, rparen)) = params {
                    self.print_token_node(lparen);
                    if let Some(params) = params {
                        self.walk_comma_separated_expr(params);
                    }
                    self.print_token_node(rparen);
                }
            }
            UserDefinedTypeData::Enum(enum_data) => {
                if let CoreIdentifierInUseNode::Ok(ok_variant_name) = property_name.core_ref() {
                    let variant_name_str = ok_variant_name
                        .token_value(&self.code_handler, self.semantic_db.interner());
                    if let Some(index) = enum_data.try_index_for_variant(variant_name_str) {
                        self.print_identifier_in_use(ty_name, is_trivia);
                        self.add_str_to_python_code(&format!("(index={}", index));
                        if let Some((_, params, _)) = params {
                            if let Some(params) = params {
                                self.add_str_to_python_code(", data=");
                                let expr = &params.core_ref().entity;
                                self.walk_expr(expr);
                            }
                        }
                        self.add_str_to_python_code(")");
                    }
                }
            }
            UserDefinedTypeData::Lambda(_) | UserDefinedTypeData::Generic(_) => {
                unreachable!()
            }
        };
    }

    pub fn print_match_case(&mut self, match_case: &MatchCaseStatementNode) {
        let core_match_case = match_case.core_ref();
        let expr = &core_match_case.expr;
        let match_block = &core_match_case.block;
        let mut symbol_index: Option<SymbolIndex<UserDefinedTypeData>> = None;
        let mut conditional_keyword_str: &'static str = "if";

        // this is added so that expr is evaluated only once in the generated python code
        self.add_indention_to_python_code();
        self.add_str_to_python_code("__tmp_enum_expr = ");
        self.walk_expr(expr);
        self.add_str_to_python_code("\n");

        for stmt in &match_block.core_ref().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatementIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatementIndentWrapperNode::IncorrectlyIndented(stmt) => &stmt.core_ref().stmt,
                _ => continue,
            };
            let CoreStatementNode::CaseBranch(case_branch) = stmt.core_ref() else {
                unreachable!()
            };
            let core_case_branch = case_branch.core_ref();
            let enum_name = &core_case_branch.enum_name;
            if symbol_index.is_none() {
                // cache the symbol_data to be used for all case branches
                if let CoreIdentifierInDeclNode::Ok(ok_enum_name) = enum_name.core_ref() {
                    if let Some(sym_index) = self
                        .semantic_db
                        .ty_symbol_for_identifier_in_decl(ok_enum_name)
                    {
                        symbol_index = Some(sym_index);
                    }
                }
            }
            let variant_name = &core_case_branch.variant_name;
            if let CoreIdentifierInDeclNode::Ok(ok_variant_name) = variant_name.core_ref() {
                let variant_name_str =
                    ok_variant_name.token_value(&self.code_handler, self.semantic_db.interner());
                let index = match symbol_index {
                    Some(symbol_index) => self
                        .semantic_db
                        .ty_symbol_mut_ref(symbol_index)
                        .enum_data_mut_ref()
                        .try_index_for_variant(variant_name_str)
                        .unwrap(),
                    None => unreachable!(),
                };
                let case_block = &core_case_branch.block;
                self.add_indention_to_python_code();
                self.add_str_to_python_code(conditional_keyword_str);
                // self.walk_expr(expr);
                self.add_str_to_python_code(&format!(" __tmp_enum_expr.index == {}:", index));
                self.open_block();
                self.print_token_node(&case_block.core_ref().newline);
                if let Some((_, variable_name, _)) = &core_case_branch.variable_name {
                    self.add_indention_to_python_code();
                    self.print_identifier_in_decl(variable_name, true);
                    self.add_str_to_python_code(" = ");
                    // self.walk_expr(expr);
                    self.add_str_to_python_code("__tmp_enum_expr.data\n")
                }
                for stmt in &case_block.core_ref().stmts {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block();
            }
            conditional_keyword_str = "elif";
        }
    }

    pub fn print_bounded_method_wrapper(
        &mut self,
        bounded_method_wrapper: &BoundedMethodWrapperNode,
    ) {
        let bounded_kind = match self.semantic_db.bounded_kind_ref(bounded_method_wrapper) {
            Some(bounded_kind) => bounded_kind,
            None => unreachable!(),
        };
        match bounded_kind {
            BoundedMethodKind::ClassMethod => {
                let core_func_decl = &bounded_method_wrapper.0.as_ref().func_decl.core_ref();
                self.print_token_node_without_trivia(&core_func_decl.def_keyword);
                self.walk_identifier_in_decl(&core_func_decl.name);
                self.walk_callable_body(&core_func_decl.body);
            }
            BoundedMethodKind::Method | BoundedMethodKind::Constructor => {
                let core_func_decl = bounded_method_wrapper.0.as_ref().func_decl.core_ref();
                let def_keyword = &core_func_decl.def_keyword;
                let name = &core_func_decl.name;
                let body = core_func_decl.body.core_ref();
                let colon = &body.colon;
                let block = &body.block;
                let prototype = body.prototype.core_ref();
                let lparen = &prototype.lparen;
                let rparen = &prototype.rparen;
                let params = &prototype.params;

                self.print_token_node_without_trivia(def_keyword);
                self.print_identifier_in_decl(name, true);
                self.print_token_node(lparen);
                self.add_str_to_python_code("self");
                if let Some(params) = params {
                    self.add_str_to_python_code(", ");
                    self.walk_comma_separated_name_ty_specs(params);
                }
                self.print_token_node(rparen);
                self.print_token_node(colon);
                self.walk_block(block);
            }
        }
    }

    pub fn print_conditional_block(&mut self, conditional_block: &ConditionalBlockNode) {
        self.add_indention_to_python_code();
        let core_conditional_block = conditional_block.core_ref();
        self.print_token_node_without_trivia(&core_conditional_block.condition_keyword);
        self.walk_expr(&core_conditional_block.condition_expr);
        self.walk_token(&core_conditional_block.colon);
        self.walk_block(&core_conditional_block.block);
    }

    fn print_stmt(&mut self, stmt: &StatementNode) {
        match stmt.core_ref() {
            CoreStatementNode::Expression(expr_stmt) => {
                self.add_indention_to_python_code();
                let core_expr_stmt = expr_stmt.core_ref();
                self.print_expr_without_trivia(&core_expr_stmt.expr);
                self.walk_token(&core_expr_stmt.newline);
            }
            CoreStatementNode::Assignment(assignment) => {
                self.add_indention_to_python_code();
                match assignment.core_ref() {
                    CoreAssignmentNode::Ok(ok_assignment) => {
                        let core_ok_assignment = ok_assignment.core_ref();
                        self.print_atom_node_without_trivia(&core_ok_assignment.l_atom);
                        self.walk_token(&core_ok_assignment.equal);
                        self.walk_r_assignment(&core_ok_assignment.r_assign);
                    }
                    CoreAssignmentNode::InvalidLValue(_) => unreachable!(),
                }
            }
            CoreStatementNode::VariableDeclaration(variable_decl) => {
                self.add_indention_to_python_code();
                self.print_variable_decl(variable_decl)
            }
            CoreStatementNode::Return(return_stmt) => {
                self.add_indention_to_python_code();
                let core_return_stmt = return_stmt.core_ref();
                self.print_token_node_without_trivia(&core_return_stmt.return_keyword);
                if let Some(expr) = &core_return_stmt.expr {
                    self.walk_expr(expr);
                }
                self.print_token_node(&core_return_stmt.newline);
            }
            CoreStatementNode::Conditional(conditional_stmt) => {
                let core_conditional_stmt = conditional_stmt.core_ref();
                self.walk_conditional_block(&core_conditional_stmt.if_block);
                for elif_block in &core_conditional_stmt.elifs {
                    self.walk_conditional_block(elif_block);
                }
                if let Some((else_keyword, colon, else_block)) = &core_conditional_stmt.else_block {
                    self.add_indention_to_python_code();
                    self.print_token_node_without_trivia(else_keyword);
                    self.walk_token(colon);
                    self.walk_block(else_block);
                }
            }
            CoreStatementNode::WhileLoop(while_loop_stmt) => {
                self.add_indention_to_python_code();
                let core_while_loop = while_loop_stmt.core_ref();
                self.print_token_node_without_trivia(&core_while_loop.while_keyword);
                self.walk_expr(&core_while_loop.condition_expr);
                self.walk_token(&core_while_loop.colon);
                self.walk_block(&core_while_loop.block);
            }
            CoreStatementNode::ForLoop(for_loop_stmt) => {
                self.add_indention_to_python_code();
                let core_for_loop = for_loop_stmt.core_ref();
                self.print_token_node_without_trivia(&core_for_loop.for_keyword);
                self.walk_identifier_in_decl(&core_for_loop.loop_variable);
                self.walk_token(&core_for_loop.in_keyword);
                self.walk_expr(&core_for_loop.iterable_expr);
                self.walk_token(&core_for_loop.colon);
                self.walk_block(&core_for_loop.block);
            }
            CoreStatementNode::Break(break_stmt) => {
                self.add_indention_to_python_code();
                let core_break_stmt = break_stmt.core_ref();
                self.print_token_node_without_trivia(&core_break_stmt.break_keyword);
                self.print_token_node(&core_break_stmt.newline);
            }
            CoreStatementNode::Continue(continue_stmt) => {
                self.add_indention_to_python_code();
                let core_continue_stmt = continue_stmt.core_ref();
                self.print_token_node_without_trivia(&core_continue_stmt.continue_keyword);
                self.print_token_node(&core_continue_stmt.newline);
            }
            CoreStatementNode::FunctionWrapper(func_wrapper) => {
                self.add_indention_to_python_code();
                let core_func_wrapper = func_wrapper.core_ref();
                let core_func_decl = core_func_wrapper.func_decl.core_ref();
                self.print_token_node_without_trivia(&core_func_decl.def_keyword);
                self.walk_identifier_in_decl(&core_func_decl.name);
                self.walk_callable_body(&core_func_decl.body);
            }
            CoreStatementNode::BoundedMethodWrapper(bounded_method_wrapper) => {
                self.add_indention_to_python_code();
                self.print_bounded_method_wrapper(bounded_method_wrapper)
            }
            CoreStatementNode::TypeDeclaration(ty_decl) => {
                self.add_indention_to_python_code();
                self.print_ty_decl(ty_decl);
            }
            CoreStatementNode::MatchCase(match_case_stmt) => self.print_match_case(match_case_stmt),
            CoreStatementNode::StructPropertyDeclaration(_)
            | CoreStatementNode::InterfaceDeclaration(_)
            | CoreStatementNode::DeclareFunctionPrototype(_) => {
                self.add_str_to_python_code("\n");
            }
            CoreStatementNode::EnumVariantDeclaration(_)
            | CoreStatementNode::InterfaceMethodPrototypeWrapper(_)
            | CoreStatementNode::CaseBranch(_) => unreachable!(),
        }
    }
}

impl<'ctx> Visitor for PythonCodeGenerator<'ctx> {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::Block(block) => {
                self.open_block();
                let core_block = block.0.as_ref();
                self.print_token_node(&core_block.newline);

                if block.core_ref().kind.has_callable_body() {
                    let mut nonlocal_strs = vec![];
                    let variable_non_locals = self.non_locals(block);
                    for variable_name in variable_non_locals.iter() {
                        let mangled_variable_name =
                            variable_name.to_string(VAR_SUFFIX, self.semantic_db.interner());
                        nonlocal_strs.push(format!(
                            "{}nonlocal {}\n",
                            whitespaces_from_indent_level(self.indent_level),
                            mangled_variable_name
                        ));
                    }
                    for nonlocal_str in nonlocal_strs {
                        self.add_str_to_python_code(&nonlocal_str);
                    }
                }
                for stmt in &core_block.stmts {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block();
                None
            }
            ASTNode::StatementIndentWrapper(stmt_wrapper) => {
                let core_stmt_wrapper = stmt_wrapper.core_ref();
                match core_stmt_wrapper {
                    CoreStatementIndentWrapperNode::CorrectlyIndented(ok_stmt) => {
                        self.walk_stmt(ok_stmt);
                    }
                    CoreStatementIndentWrapperNode::ExtraNewlines(_) => {
                        self.add_str_to_python_code("\n")
                    }
                    CoreStatementIndentWrapperNode::IncorrectlyIndented(_)
                    | CoreStatementIndentWrapperNode::LeadingSkippedTokens(_)
                    | CoreStatementIndentWrapperNode::TrailingSkippedTokens(_) => unreachable!(),
                }
                None
            }
            ASTNode::Statement(stmt) => {
                self.print_stmt(stmt);
                None
            }
            ASTNode::CallablePrototype(callable_prototype) => {
                self.print_callable_prototype(callable_prototype);
                None
            }
            ASTNode::ConditionalBlock(conditional_block) => {
                self.print_conditional_block(conditional_block);
                None
            }
            ASTNode::NameTypeSpec(name_ty_spec) => {
                // This is where type-annotations are evapored in the generated Python code
                let core_name_ty_spec = name_ty_spec.core_ref();
                let name = &core_name_ty_spec.name;
                self.print_identifier_in_decl(name, true);
                None
            }
            ASTNode::EnumVariantExprOrClassMethodCall(enum_variant_expr_or_class_method_call) => {
                self.print_enum_variant_expr_or_class_method_call(
                    enum_variant_expr_or_class_method_call,
                    true,
                );
                None
            }
            ASTNode::IdentifierInDecl(identifier_in_decl) => {
                self.print_identifier_in_decl(identifier_in_decl, true);
                None
            }
            ASTNode::IdentifierInUse(identifier_in_use) => {
                self.print_identifier_in_use(identifier_in_use, true);
                None
            }
            ASTNode::OkToken(token) => {
                self.print_token(&token.core_ref().token);
                None
            }
            _ => Some(()),
        }
    }
}
