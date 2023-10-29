use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, BoundedMethodKind, BoundedMethodWrapperNode, CallablePrototypeNode,
            CoreIdentifierInDeclNode, CoreIdentifierInUseNode, CoreRVariableDeclarationNode,
            CoreStatementIndentWrapperNode, CoreTokenNode, CoreTypeDeclarationNode,
            EnumVariantExprOrClassMethodCallNode, IdentifierInDeclNode, IdentifierInUseNode,
            OkIdentifierInDeclNode, OkIdentifierInUseNode, TokenNode, TypeDeclarationNode,
            VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::JarvilCode,
    constants::common::{FUNC_SUFFIX, TY_SUFFIX, VAR_SUFFIX},
    context,
    lexer::token::{CoreToken, Token},
    scope::{
        core::{LookupResult, MangledIdentifierName},
        handler::{ConcreteSymbolDataEntry, SemanticStateDatabase, SymbolDataEntry},
    },
};
use rustc_hash::FxHashSet;
use std::convert::TryInto;

// Utility functions
pub fn get_whitespaces_from_indent_level(indent_level: usize) -> String {
    let expected_indent_spaces = context::indent_spaces() * indent_level;
    " ".to_string()
        .repeat(expected_indent_spaces.try_into().unwrap())
}

pub fn get_trivia_from_token_node(token: &TokenNode) -> Option<&Vec<Token>> {
    match token.core_ref() {
        CoreTokenNode::Ok(ok_token_node) => match &ok_token_node.core_ref().token.trivia {
            Some(trivia) => Some(trivia),
            None => None,
        },
        _ => unreachable!(),
    }
}

pub struct PythonCodeGenerator {
    indent_level: usize,
    generated_code: String,
    code: JarvilCode,
    semantic_state_db: SemanticStateDatabase,
}

impl PythonCodeGenerator {
    pub fn new(code: JarvilCode, semantic_state_db: SemanticStateDatabase) -> PythonCodeGenerator {
        PythonCodeGenerator {
            indent_level: 0,
            generated_code: "".to_string(),
            code,
            semantic_state_db,
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
        for stmt in code_block.stmts.as_ref() {
            self.walk_stmt_indent_wrapper(stmt);
        }
        //let main_call_str = format!(
        //    "\n\nif __name__ == \"__main__\":\n{}main_func()",
        //    get_whitespaces_from_indent_level(1)
        //);
        let index = match self
            .semantic_state_db
            .namespace
            .lookup_in_functions_namespace(0, &self.semantic_state_db.interner.intern("main"))
        {
            LookupResult::Ok(lookup_data) => match lookup_data.symbol_data.0.get_index() {
                Some(index) => index,
                None => unreachable!(),
            },
            LookupResult::Unresolved | LookupResult::NotInitialized(_) => unreachable!(),
        };
        let main_call_str = format!("\n\nmain_{}_func()", index);
        self.add_str_to_python_code(&main_call_str);
        self.generated_code
    }

    pub fn add_str_to_python_code(&mut self, str: &str) {
        self.generated_code.push_str(str);
    }

    pub fn get_non_locals(&self, block: &BlockNode) -> &FxHashSet<MangledIdentifierName> {
        self.semantic_state_db.get_non_locals_ref(block)
    }

    pub fn get_mangled_identifier_name_in_decl(
        &self,
        identifier: &OkIdentifierInDeclNode,
    ) -> String {
        match self
            .semantic_state_db
            .get_symbol_data_for_identifier_in_decl(identifier)
        {
            Some(symbol_data) => match symbol_data {
                SymbolDataEntry::Variable(variable_symbol_data) => {
                    return variable_symbol_data
                        .get_mangled_name()
                        .to_string(VAR_SUFFIX, &self.semantic_state_db.interner);
                }
                SymbolDataEntry::Function(func_symbol_data) => {
                    return func_symbol_data
                        .get_mangled_name()
                        .to_string(FUNC_SUFFIX, &self.semantic_state_db.interner);
                }
                SymbolDataEntry::Type(type_symbol_data) => {
                    return type_symbol_data
                        .get_mangled_name()
                        .to_string(TY_SUFFIX, &self.semantic_state_db.interner);
                }
                SymbolDataEntry::Interface(_) => unreachable!(),
            },
            None => identifier.token_value_str(&self.code),
        }
    }

    pub fn get_mangled_identifier_name_in_use(&self, identifier: &OkIdentifierInUseNode) -> String {
        match self
            .semantic_state_db
            .get_symbol_data_for_identifier_in_use(identifier)
        {
            Some(symbol_data) => match symbol_data {
                ConcreteSymbolDataEntry::Variable(variable_symbol_data) => {
                    return variable_symbol_data
                        .symbol_data
                        .get_mangled_name()
                        .to_string(VAR_SUFFIX, &self.semantic_state_db.interner);
                }
                ConcreteSymbolDataEntry::Function(func_symbol_data) => {
                    return func_symbol_data
                        .symbol_data
                        .get_mangled_name()
                        .to_string(FUNC_SUFFIX, &self.semantic_state_db.interner);
                }
                ConcreteSymbolDataEntry::Type(type_symbol_data) => {
                    return type_symbol_data
                        .symbol_data
                        .get_mangled_name()
                        .to_string(TY_SUFFIX, &self.semantic_state_db.interner)
                }
                ConcreteSymbolDataEntry::Interface(_) => unreachable!(),
            },
            None => identifier.token_value_str(&self.code),
        }
    }

    pub fn print_token(&mut self, token: &Token) {
        let trivia = match &token.trivia {
            Some(trivia) => Some(trivia),
            None => None,
        };
        self.print_trivia(trivia);
        let token_value = token.token_value_str(&self.code);
        match token.core_token {
            CoreToken::SINGLE_LINE_COMMENT => {
                if token_value.starts_with('/') {
                    let mut modified_str = "#".to_string();
                    modified_str.push_str(&token_value[2..]);
                    self.add_str_to_python_code(&modified_str);
                } else {
                    self.add_str_to_python_code(&token_value);
                }
            }
            CoreToken::BLOCK_COMMENT => {
                let len = token_value.len();
                let mut critical_section = token_value[2..(len - 2)].to_string();
                critical_section.push_str("\"\"\"");
                let mut final_str = "\"\"\"".to_string();
                final_str.push_str(&critical_section);
                self.add_str_to_python_code(&final_str);
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

    pub fn print_identifier_in_decl(&mut self, identifier: &IdentifierInDeclNode) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierInDeclNode::Ok(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let token_value = self.get_mangled_identifier_name_in_decl(identifier);
        let token = &identifier.0.as_ref().name.core_ref().token;
        let trivia = match &token.trivia {
            Some(trivia) => Some(trivia),
            None => None,
        };
        self.print_trivia(trivia);
        self.add_str_to_python_code(&token_value);
    }

    pub fn print_identifier_in_use(&mut self, identifier: &IdentifierInUseNode) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierInUseNode::Ok(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let token_value = self.get_mangled_identifier_name_in_use(identifier);
        let token = &identifier.0.as_ref().name.core_ref().token;
        let trivia = match &token.trivia {
            Some(trivia) => Some(trivia),
            None => None,
        };
        self.print_trivia(trivia);
        self.add_str_to_python_code(&token_value);
    }

    pub fn print_identifier_in_decl_without_trivia(&mut self, identifier: &IdentifierInDeclNode) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierInDeclNode::Ok(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let token_value = self.get_mangled_identifier_name_in_decl(identifier);
        self.add_str_to_python_code(&token_value);
    }

    pub fn print_identifier_in_use_without_trivia(&mut self, identifier: &IdentifierInUseNode) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierInUseNode::Ok(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let token_value = self.get_mangled_identifier_name_in_use(identifier);
        self.add_str_to_python_code(&token_value);
    }

    pub fn print_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        let let_keyword = &core_variable_decl.let_keyword;
        let name = &core_variable_decl.name;
        let equal = &core_variable_decl.equal;
        let r_node = &core_variable_decl.r_node;
        let trivia = get_trivia_from_token_node(let_keyword);
        self.print_trivia(trivia);
        match r_node.core_ref() {
            CoreRVariableDeclarationNode::Expression(expr_stmt) => {
                self.print_identifier_in_decl_without_trivia(name);
                self.print_token_node(equal);
                self.walk_expr_stmt(expr_stmt);
            }
            CoreRVariableDeclarationNode::Lambda(lambda_decl) => {
                let callable_body = &lambda_decl.core_ref().body;
                self.add_str_to_python_code("def");
                self.print_identifier_in_decl(name);
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
            self.walk_comma_separated_name_type_specs(params);
        }
        self.print_token_node(rparen);
    }

    pub fn print_type_decl(&mut self, type_decl: &TypeDeclarationNode) {
        let core_type_decl = type_decl.core_ref();
        match core_type_decl {
            CoreTypeDeclarationNode::Struct(struct_decl) => {
                let core_struct_decl = struct_decl.core_ref();
                let struct_name = &core_struct_decl.name;
                let type_keyword = &core_struct_decl.type_keyword;
                let colon = &core_struct_decl.colon;
                let block = &core_struct_decl.block;
                let trivia = get_trivia_from_token_node(type_keyword);
                self.print_trivia(trivia);
                self.add_str_to_python_code("class");
                self.print_identifier_in_decl(struct_name);
                self.print_token_node(colon);
                self.walk_block(block);
            }
            CoreTypeDeclarationNode::Lambda(_) => {
                self.add_str_to_python_code("\n");
            }
            CoreTypeDeclarationNode::Enum(enum_decl) => {
                let core_enum_decl = enum_decl.core_ref();
                let enum_name = &core_enum_decl.name;
                let type_keyword = &core_enum_decl.type_keyword;
                let colon = &core_enum_decl.colon;
                let trivia = get_trivia_from_token_node(type_keyword);
                self.print_trivia(trivia);
                self.add_str_to_python_code("class");
                self.print_identifier_in_decl(enum_name);
                self.print_token_node(colon);
                let constructor_str = format!(
                    "\n{}def __init__(index, data=None):\n{}self.index = index\n{}self.data = data\n",
                    get_whitespaces_from_indent_level(self.indent_level + 1),
                    get_whitespaces_from_indent_level(self.indent_level + 2),
                    get_whitespaces_from_indent_level(self.indent_level + 2)
                );
                self.add_str_to_python_code(&constructor_str);
            }
            CoreTypeDeclarationNode::MissingTokens(_) => unreachable!(),
        }
    }

    pub fn print_enum_variant_expr_or_class_method_call(
        &mut self,
        enum_variant_expr_or_class_method_call: &EnumVariantExprOrClassMethodCallNode,
    ) {
        let core_enum_variant_expr_or_class_method_call =
            enum_variant_expr_or_class_method_call.core_ref();
        // let lparen = &core_enum_variant_expr_or_class_method_call.lparen;
        // let rparen = &core_enum_variant_expr_or_class_method_call.rparen;
        let ty_name = &core_enum_variant_expr_or_class_method_call.ty_name;
        let property_name = &core_enum_variant_expr_or_class_method_call.property_name;
        let params = &core_enum_variant_expr_or_class_method_call.params;
        // TODO - get the symbol_data for `ty_name` and check if it's classmethod or enum
        /*
        self.print_identifier_in_use(ty_name);
        self.add_str_to_python_code(".");
        self.print_identifier_in_use(property_name);
        self.print_token_node(lparen);
        if let Some(params) = params {
            self.walk_comma_separated_expressions(params);
        }
        self.print_token_node(rparen);
        */
    }

    pub fn print_bounded_method_wrapper(
        &mut self,
        bounded_method_wrapper: &BoundedMethodWrapperNode,
    ) {
        let bounded_kind = match self
            .semantic_state_db
            .get_bounded_kind_ref(bounded_method_wrapper)
        {
            Some(bounded_kind) => bounded_kind,
            None => unreachable!(),
        };
        match bounded_kind {
            BoundedMethodKind::ClassMethod => {
                self.walk_func_decl(&bounded_method_wrapper.0.as_ref().func_decl);
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

                self.print_token_node(def_keyword);
                self.print_identifier_in_decl(name);
                self.print_token_node(lparen);
                self.add_str_to_python_code("self");
                if let Some(params) = params {
                    self.add_str_to_python_code(", ");
                    self.walk_comma_separated_name_type_specs(params);
                }
                self.print_token_node(rparen);
                self.print_token_node(colon);
                self.walk_block(block);
            }
        }
    }
}

impl Visitor for PythonCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::Block(block) => {
                self.open_block();
                let core_block = block.0.as_ref();
                self.print_token_node(&core_block.newline);

                if block.core_ref().kind.has_callable_body() {
                    let mut nonlocal_strs = vec![];
                    let variable_non_locals = self.get_non_locals(block);
                    for variable_name in variable_non_locals.iter() {
                        let mangled_variable_name =
                            variable_name.to_string(VAR_SUFFIX, &self.semantic_state_db.interner);
                        nonlocal_strs.push(format!(
                            "{}nonlocal {}\n",
                            get_whitespaces_from_indent_level(self.indent_level),
                            mangled_variable_name
                        ));
                    }
                    for nonlocal_str in nonlocal_strs {
                        self.add_str_to_python_code(&nonlocal_str);
                    }
                }

                for stmt in core_block.stmts.as_ref() {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block();
                None
            }
            ASTNode::StatementIndentWrapper(stmt_wrapper) => {
                let core_stmt_wrapper = stmt_wrapper.core_ref();
                match core_stmt_wrapper {
                    CoreStatementIndentWrapperNode::CorrectlyIndented(ok_stmt) => {
                        // self.add_str_to_python_code(&get_whitespaces_from_indent_level(1));
                        self.walk_stmt(ok_stmt);
                    }
                    CoreStatementIndentWrapperNode::ExtraNewlines(extra_newlines) => {
                        let core_extra_newlines = extra_newlines.core_ref();
                        for extra_newline in &core_extra_newlines.skipped_tokens {
                            let core_token = &extra_newline.core_ref().skipped_token;
                            // self.add_str_to_python_code(&get_whitespaces_from_indent_level(1));
                            self.print_token(core_token);
                        }
                    }
                    CoreStatementIndentWrapperNode::IncorrectlyIndented(_) => unreachable!(),
                    CoreStatementIndentWrapperNode::LeadingSkippedTokens(_) => unreachable!(),
                    CoreStatementIndentWrapperNode::TrailingSkippedTokens(_) => unreachable!(),
                }
                None
            }
            ASTNode::VariableDeclaration(variable_decl) => {
                self.print_variable_decl(variable_decl);
                None
            }
            ASTNode::StructPropertyDeclaration(_) => {
                self.add_str_to_python_code("\n");
                None
            }
            ASTNode::BoundedMethodWrapper(bounded_method_wrapper) => {
                self.print_bounded_method_wrapper(bounded_method_wrapper);
                None
            }
            ASTNode::CallablePrototype(callable_prototype) => {
                self.print_callable_prototype(callable_prototype);
                None
            }
            ASTNode::NameTypeSpec(name_type_spec) => {
                // This is where type-annotations are evapored in the generated Python code
                let core_name_type_spec = name_type_spec.core_ref();
                let name = &core_name_type_spec.name;
                self.print_identifier_in_decl(name);
                None
            }
            ASTNode::TypeDeclaration(type_decl) => {
                self.print_type_decl(type_decl);
                None
            }
            ASTNode::InterfaceDeclaration(_) => {
                self.add_str_to_python_code("\n");
                None
            }
            ASTNode::EnumVariantExprOrClassMethodCall(enum_variant_expr_or_class_method_call) => {
                self.print_enum_variant_expr_or_class_method_call(
                    enum_variant_expr_or_class_method_call,
                );
                None
            }
            ASTNode::IdentifierInDecl(identifier_in_decl) => {
                self.print_identifier_in_decl(identifier_in_decl);
                None
            }
            ASTNode::IdentifierInUse(identifier_in_use) => {
                self.print_identifier_in_use(identifier_in_use);
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
