use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, BoundedMethodKind, BoundedMethodWrapperNode, CallablePrototypeNode,
            ClassMethodCallNode, CoreCallableBodyNode, CoreIdentifierNode,
            CoreRVariableDeclarationNode, CoreStatemenIndentWrapperNode, CoreTokenNode,
            CoreTypeDeclarationNode, IdentifierNode, OkIdentifierNode, TokenNode,
            TypeDeclarationNode, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::JarvilCode,
    context,
    lexer::token::{CoreToken, Token},
    scope::{core::NamespaceKind, handler::NamespaceHandler},
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::convert::TryInto;

// Utility functions
pub fn get_whitespaces_from_indent_level(indent_level: usize) -> String {
    let expected_indent_spaces = context::indent_spaces() * indent_level;
    return " "
        .to_string()
        .repeat(expected_indent_spaces.try_into().unwrap());
}

pub fn get_trivia_from_token_node(token: &TokenNode) -> Option<&Vec<Token>> {
    match token.core_ref() {
        CoreTokenNode::Ok(ok_token_node) => match &ok_token_node.core_ref().token.trivia {
            Some(trivia) => return Some(trivia),
            None => return None,
        },
        _ => unreachable!(),
    }
}

pub struct PythonCodeGenerator {
    indent_level: usize,
    generate_code: String,
    code: JarvilCode,
    namespace_handler: NamespaceHandler,
}

impl PythonCodeGenerator {
    pub fn new(code: JarvilCode, namespace_handler: NamespaceHandler) -> PythonCodeGenerator {
        PythonCodeGenerator {
            indent_level: 0,
            generate_code: "".to_string(),
            code: code,
            namespace_handler,
        }
    }

    pub fn open_block(&mut self) {
        self.indent_level = self.indent_level + 1;
    }

    pub fn close_block(&mut self) {
        self.indent_level = self.indent_level - 1;
    }

    pub fn generate_python_code(mut self, ast: &BlockNode) -> String {
        let code_block = ast.0.as_ref();
        for stmt in &*code_block.stmts.as_ref() {
            self.walk_stmt_indent_wrapper(stmt);
        }
        //let main_call_str = format!(
        //    "\n\nif __name__ == \"__main__\":\n{}main_func()",
        //    get_whitespaces_from_indent_level(1)
        //);
        let main_call_str = "\n\nmain_func()";
        self.add_str_to_python_code(main_call_str);
        self.generate_code
    }

    pub fn add_str_to_python_code(&mut self, str: &str) {
        self.generate_code.push_str(str);
    }

    pub fn get_non_locals(
        &self,
        block: &BlockNode,
    ) -> (&FxHashSet<String>, &FxHashMap<String, bool>) {
        self.namespace_handler.get_non_locals_ref(block)
    }

    pub fn get_suffix_str_for_identifier(&self, identifier: &OkIdentifierNode) -> &'static str {
        match self
            .namespace_handler
            .identifier_binding_table
            .get(identifier)
        {
            Some((scope_index, namespace_kind)) => {
                let name = identifier.token_value(&self.code);
                match namespace_kind {
                    NamespaceKind::Variable => {
                        match self
                            .namespace_handler
                            .namespace
                            .get_from_variables_namespace(*scope_index, &name)
                        {
                            Some(symbol_data) => {
                                if symbol_data.2 {
                                    return "_var";
                                }
                                return "";
                            }
                            None => unreachable!(),
                        }
                    }
                    NamespaceKind::Function => {
                        match self
                            .namespace_handler
                            .namespace
                            .get_from_functions_namespace(*scope_index, &name)
                        {
                            Some(symbol_data) => {
                                if symbol_data.2 {
                                    return "_func";
                                }
                                return "";
                            }
                            None => unreachable!(),
                        }
                    }
                    NamespaceKind::Type => {
                        match self
                            .namespace_handler
                            .namespace
                            .get_from_types_namespace(*scope_index, &name)
                        {
                            Some(symbol_data) => {
                                if symbol_data.2 {
                                    return "_ty";
                                }
                                return "";
                            }
                            None => unreachable!(),
                        }
                    }
                };
            }
            None => return "",
        };
    }

    pub fn print_token(&mut self, token: &Token) {
        let trivia = match &token.trivia {
            Some(trivia) => Some(trivia),
            None => None,
        };
        self.print_trivia(trivia);
        let token_value = token.token_value(&self.code);
        match token.core_token {
            CoreToken::SINGLE_LINE_COMMENT => {
                if token_value.chars().next().unwrap() == '/' {
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
            CoreToken::ENDMARKER => return,
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
                self.walk_ok_token(&ok_token_node);
            }
            CoreTokenNode::MissingTokens(_) => unreachable!(),
        }
    }

    pub fn print_identifier(&mut self, identifier: &IdentifierNode) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierNode::Ok(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let suffix_str = self.get_suffix_str_for_identifier(identifier);
        let mut token_value = identifier.token_value(&self.code);
        token_value.push_str(suffix_str);
        let token = &identifier.0.as_ref().token.core_ref().token;
        let trivia = match &token.trivia {
            Some(trivia) => Some(trivia),
            None => None,
        };
        self.print_trivia(trivia);
        self.add_str_to_python_code(&token_value);
    }

    pub fn print_identifier_without_trivia(&mut self, identifier: &IdentifierNode) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierNode::Ok(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let suffix_str = self.get_suffix_str_for_identifier(identifier);
        let mut token_value = identifier.token_value(&self.code);
        token_value.push_str(suffix_str);
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
                self.print_identifier_without_trivia(name);
                self.print_token_node(equal);
                self.walk_expr_stmt(expr_stmt);
            }
            CoreRVariableDeclarationNode::Lambda(lambda_decl) => {
                let callable_body = &lambda_decl.core_ref().body;
                self.add_str_to_python_code("def");
                self.print_identifier(name);
                self.walk_callable_body(callable_body);
            }
            CoreRVariableDeclarationNode::MissingTokens(_) => unreachable!(),
        }
    }

    pub fn print_callable_prototype(&mut self, callable_prototype: &CallablePrototypeNode) {
        let core_callable_prototype = callable_prototype.core_ref();
        let lparen = &core_callable_prototype.lparen;
        let params = &core_callable_prototype.params;
        let rparen = &core_callable_prototype.rparen;
        self.print_token_node(lparen);
        if let Some(params) = params {
            self.walk_name_type_specs(params);
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
                self.print_identifier(struct_name);
                self.print_token_node(colon);
                self.walk_block(block);
            }
            CoreTypeDeclarationNode::Lambda(_) => {
                self.add_str_to_python_code("\n");
            }
            CoreTypeDeclarationNode::MissingTokens(_) => unreachable!(),
        }
    }

    pub fn print_class_method_call(&mut self, class_method_call: &ClassMethodCallNode) {
        let core_class_method_call = class_method_call.core_ref();
        let lparen = &core_class_method_call.lparen;
        let rparen = &core_class_method_call.rparen;
        let class_name = &core_class_method_call.class_name;
        let class_method_name = &core_class_method_call.class_method_name;
        let params = &core_class_method_call.params;
        self.print_identifier(class_name);
        self.add_str_to_python_code(".");
        self.print_identifier(class_method_name);
        self.print_token_node(lparen);
        if let Some(params) = params {
            self.walk_params(params);
        }
        self.print_token_node(rparen);
    }

    pub fn print_bounded_method_wrapper(
        &mut self,
        bounded_method_wrapper: &BoundedMethodWrapperNode,
    ) {
        let bounded_kind = match self
            .namespace_handler
            .get_bounded_kind_ref(bounded_method_wrapper)
        {
            Some(bounded_kind) => bounded_kind,
            None => unreachable!(),
        };
        match bounded_kind {
            BoundedMethodKind::ClassMethod => {
                self.walk_func_decl(&bounded_method_wrapper.0.as_ref().func_decl);
                return;
            }
            BoundedMethodKind::Method | BoundedMethodKind::Constructor => {
                let core_func_decl = bounded_method_wrapper.0.as_ref().func_decl.core_ref();
                let def_keyword = &core_func_decl.def_keyword;
                let name = &core_func_decl.name;
                let body = match core_func_decl.body.core_ref() {
                    CoreCallableBodyNode::Ok(ok_callable_body) => ok_callable_body.core_ref(),
                    _ => unreachable!(),
                };
                let colon = &body.colon;
                let block = &body.block;
                let prototype = body.prototype.core_ref();
                let lparen = &prototype.lparen;
                let rparen = &prototype.rparen;
                let params = &prototype.params;

                self.print_token_node(def_keyword);
                self.print_identifier(name);
                self.print_token_node(lparen);
                self.add_str_to_python_code("self");
                if let Some(params) = params {
                    self.add_str_to_python_code(", ");
                    self.walk_name_type_specs(params);
                }
                self.print_token_node(rparen);
                self.print_token_node(colon);
                self.walk_block(block);
            }
        };
    }
}

impl Visitor for PythonCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::Block(block) => {
                self.open_block();
                let core_block = block.0.as_ref();
                self.print_token_node(&core_block.newline);
                let mut nonlocal_strs = vec![];
                let (variable_non_locals, func_non_locals) = self.get_non_locals(block);
                for variable_name in variable_non_locals.iter() {
                    nonlocal_strs.push(format!(
                        "{}nonlocal {}_var\n",
                        get_whitespaces_from_indent_level(self.indent_level),
                        variable_name
                    ));
                }
                for (func_name, &is_global) in func_non_locals.iter() {
                    if is_global {
                        // TODO - if declarations are in global we can completely remove this
                        // as there are no variable declarations which needs to have proper
                        // behaviour of non-local assignments but global scope does not have
                        // any variable declarartions and functions cannot be assigned so
                        // we can safely remove explicit global statement here
                        // self.add_str_to_python_code(&format!("global {}\n", func_name));
                        nonlocal_strs.push(format!(
                            "{}global {}_func\n",
                            get_whitespaces_from_indent_level(self.indent_level),
                            func_name
                        ))
                    } else {
                        nonlocal_strs.push(format!(
                            "{}nonlocal {}_func\n",
                            get_whitespaces_from_indent_level(self.indent_level),
                            func_name
                        ))
                    }
                }
                for nonlocal_str in nonlocal_strs {
                    self.add_str_to_python_code(&nonlocal_str);
                }
                for stmt in &*core_block.stmts.as_ref() {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block();
                return None;
            }
            ASTNode::StatementIndentWrapper(stmt_wrapper) => {
                let core_stmt_wrapper = stmt_wrapper.core_ref();
                match core_stmt_wrapper {
                    CoreStatemenIndentWrapperNode::CorrectlyIndented(ok_stmt) => {
                        // self.add_str_to_python_code(&get_whitespaces_from_indent_level(1));
                        self.walk_stmt(ok_stmt);
                    }
                    CoreStatemenIndentWrapperNode::ExtraNewlines(extra_newlines) => {
                        let core_extra_newlines = extra_newlines.core_ref();
                        for extra_newline in &core_extra_newlines.skipped_tokens {
                            let core_token = &extra_newline.core_ref().skipped_token;
                            // self.add_str_to_python_code(&get_whitespaces_from_indent_level(1));
                            self.print_token(core_token);
                        }
                    }
                    CoreStatemenIndentWrapperNode::IncorrectlyIndented(_) => unreachable!(),
                    CoreStatemenIndentWrapperNode::LeadingSkippedTokens(_) => unreachable!(),
                    CoreStatemenIndentWrapperNode::TrailingSkippedTokens(_) => unreachable!(),
                }
                return None;
            }
            ASTNode::VariableDeclaration(variable_decl) => {
                self.print_variable_decl(variable_decl);
                return None;
            }
            ASTNode::StructPropertyDeclaration(_) => {
                self.add_str_to_python_code("\n");
                return None;
            }
            ASTNode::BoundedMethodWrapper(bounded_method_wrapper) => {
                self.print_bounded_method_wrapper(bounded_method_wrapper);
                return None;
            }
            ASTNode::CallablePrototype(callable_prototype) => {
                self.print_callable_prototype(callable_prototype);
                return None;
            }
            ASTNode::NameTypeSpec(name_type_spec) => {
                // This is where type-annotations are evapored in the generated Python code
                let core_name_type_spec = name_type_spec.core_ref();
                let name = &core_name_type_spec.name;
                self.print_identifier(name);
                return None;
            }
            ASTNode::TypeDeclaration(type_decl) => {
                self.print_type_decl(type_decl);
                return None;
            }
            ASTNode::ClassMethodCall(class_method_call) => {
                self.print_class_method_call(class_method_call);
                return None;
            }
            ASTNode::Identifier(identifier) => {
                self.print_identifier(identifier);
                return None;
            }
            ASTNode::OkToken(token) => {
                self.print_token(&token.core_ref().token);
                return None;
            }
            _ => Some(()),
        }
    }
}
