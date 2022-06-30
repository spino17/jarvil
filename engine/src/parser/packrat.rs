// Default parser for jarvil uses Packrat approach, first given by Bryan Ford in his master thesis at MIT. It is essentially a 
// top down recursive descent parsing with lazy memonization in order to avoid exponential parse time and provide reliable 
// linear time parsing!
// See https://pdos.csail.mit.edu/~baford/packrat/thesis/ for more information.

use crate::lexer::token::{Token, CoreToken};
use crate::parser::ast::AST;
use std::rc::Rc;
use crate::errors::{ParseError, SyntaxError, SemanticError};
use crate::scope::{Env, SymbolData, FunctionData, StructFunction};
use crate::parser::components;
use crate::context;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use crate::parser::helper::{clone_atom_result, clone_expr_result};
use crate::types::{Type, Struct, CoreType, Lambda};

pub trait Parser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError>;  // return an AST
}

#[derive(Debug)]
pub enum RoutineCache {
    // currently only two routine (atom, expr) results are cached by the parser
    ATOM(Rc<RefCell<FxHashMap<usize, Result<(ParseSuccess, Option<Type>, bool, bool), ParseError>>>>),
    EXPR(Rc<RefCell<FxHashMap<usize, Result<(ParseSuccess, bool), ParseError>>>>),
}

#[derive(Debug)]
pub struct ParseSuccess {
    pub lookahead: usize,
    pub possible_err: Option<ParseError>,
}

pub struct PackratParser {
    token_vec: Vec<Token>,
    lookahead: usize,
    indent_level: i64,
    env: Env,
    code_lines: Vec<(Rc<String>, usize)>,
    cache: Vec<Rc<RoutineCache>>,
    // TODO - add AST data structure
}

impl PackratParser {
    pub fn new(code_lines: Vec<(Rc<String>, usize)>) -> Self {
        let env = Env::new();
        let atom_cache_map: FxHashMap<usize, Result<(ParseSuccess, Option<Type>, bool, bool), ParseError>> = FxHashMap::default();
        let expr_cache_map: FxHashMap<usize, Result<(ParseSuccess, bool), ParseError>> = FxHashMap::default();
        PackratParser {
            token_vec: Vec::new(),
            lookahead: 0,
            indent_level: -1,
            env,
            code_lines,
            cache: vec![
                Rc::new(RoutineCache::ATOM(Rc::new(RefCell::new(atom_cache_map)))),
                Rc::new(RoutineCache::EXPR(Rc::new(RefCell::new(expr_cache_map)))),
            ],
        }
    }
}

impl Parser for PackratParser {
    fn parse(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        self.code(token_vec)?;
        Ok(())
    }
}

impl PackratParser {
    // parsing utilities
    pub fn get_lookahead(&self) -> usize {
        self.lookahead
    }

    pub fn reset_lookahead(&mut self, reset_index: usize) {
        self.lookahead = reset_index;
    }

    pub fn get_index(&self) -> usize {
        let mut temp_lookahead = self.lookahead;
        loop {
            let token = &self.token_vec[temp_lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    temp_lookahead = temp_lookahead + 1;
                },
                _ => {
                    return (self.token_vec[temp_lookahead].start_index + self.token_vec[temp_lookahead].end_index) / 2 as usize
                }
            }
        }
    }

    pub fn get_indent_level(&self) -> i64 {
        self.indent_level
    }

    pub fn reset_indent_level(&mut self, reset_indent: i64) {
        self.indent_level = reset_indent;
    }

    pub fn get_code_line(&self, mut curr_line_number: usize, index: usize) -> (Rc<String>, usize, usize, usize) {
        loop {
            let (s, line_start_index) = &self.code_lines[curr_line_number - 1];
            if index >= *line_start_index {
                return (s.clone(), *line_start_index, curr_line_number, index)
            }
            curr_line_number = curr_line_number - 1;
        }
    }

    pub fn get_env(&self) -> Env {
        Env(self.env.0.clone())
    }

    pub fn reset_env(&mut self, reset_env: &Env) {
        self.env = Env(reset_env.0.clone())
    }

    pub fn set_new_env_for_block(&mut self) {
        let curr_env = Env(self.env.0.clone());
        let env = Env::new_with_parent_env(&curr_env);
        self.env = env;
    }

    pub fn set_identifier_to_scope(&mut self, identifier_name: &Rc<String>, data_type: &Type, is_init: bool) -> Option<String> {
        self.env.set_identifier(identifier_name, data_type, is_init)
    }

    pub fn set_user_defined_struct_type_to_scope(&mut self, 
        identifier_name: &Rc<String>, fields: &Rc<Vec<(Rc<String>, Type)>>) -> Option<String> {
        self.env.set_user_defined_struct_type(identifier_name, fields)
    }

    pub fn set_user_defined_lambda_type(&mut self, identifier_name: &Rc<String>, 
        params: &Rc<Vec<(Rc<String>, Type)>>, return_type: &Rc<Option<Type>>) -> Option<String> {
        self.env.set_user_defined_lambda_type(identifier_name, params, return_type)
    }

    pub fn set_function_to_scope(&mut self, identifier_name: &Rc<String>, 
        params: &Rc<Vec<(Rc<String>, Type)>>, return_type: &Rc<Option<Type>>) -> Option<String> {
        self.env.set_function(identifier_name, params, return_type)
    }

    pub fn set_params_to_scope(&mut self, params: Option<&Vec<(Rc<String>, Type)>>) {
        if let Some(params) = params {
            for (identifier_name, data_type) in params {
                self.set_identifier_to_scope(identifier_name, data_type, true);
            }
        }
    }

    pub fn set_method_to_struct(&mut self, struct_name: &Rc<String>, method_name: &Rc<String>, method_data: StructFunction) {
        self.env.set_method_to_struct(struct_name, method_name, method_data);
    }

    pub fn has_field_with_name(&self, data_type: &Type, field_name: &Rc<String>) -> Option<Type> {
        match data_type.get_user_defined_type_name() {
            Some(data_type_key) => {
                match self.env.get(&data_type_key) {
                    Some(symbol_data) => {
                        match &symbol_data.has_field_with_name(field_name) {
                            Some(val) => Some(Type(val.0.clone())),
                            None => None,
                        }
                    },
                    None => None
                }
            },
            None => None,
        }
    }

    pub fn has_method_with_name(&self, data_type: &Type, method_name: &Rc<String>) -> Option<FunctionData> {
        match data_type.get_user_defined_type_name() {
            Some(data_type_key) => {
                match self.env.get(&data_type_key) {
                    Some(symbol_data) => {
                        match &symbol_data.has_method_with_name(method_name) {
                            Some(val) => Some(FunctionData{
                                params: val.params.clone(),
                                return_type: val.return_type.clone(),
                            }),
                            None => None,
                        }
                    },
                    None => None
                }
            },
            None => None,
        }
    }

    pub fn has_class_method_with_name(&self, struct_name: &Rc<String>, 
        class_method_name: &Rc<String>) -> Option<FunctionData> {
        match self.env.get(struct_name) {
            Some(symbol_data) => {
                match &symbol_data.has_class_method_with_name(class_method_name) {
                    Some(val) => Some(FunctionData{
                        params: val.params.clone(),
                        return_type: val.return_type.clone(),
                    }),
                    None => None,
                }
            },
            None => None
        }
    }

    pub fn set_token_vec(&mut self, token_vec: Vec<Token>) {
        self.token_vec = token_vec;
    }

    pub fn get_curr_line_number(&self) -> usize {
        self.token_vec[self.lookahead].line_number
    }

    pub fn get_curr_core_token(&mut self) -> &CoreToken {
        self.ignore_blanks();
        &self.token_vec[self.lookahead].core_token
    }

    pub fn get_curr_token_name(&mut self) -> Rc<String> {
        self.ignore_blanks();
        self.token_vec[self.lookahead].name.clone()
    }

    pub fn get_next_token_name(&self) ->Rc<String> {
        let mut temp_lookahead = self.lookahead;
        loop {
            let token = &self.token_vec[temp_lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    temp_lookahead = temp_lookahead + 1;
                },
                _ => {
                    return token.name.clone();
                }
            }
        }
    }

    pub fn check_next_token(&self, symbol: &str) -> bool {
        let mut temp_lookahead = self.lookahead;
        loop {
            let token = &self.token_vec[temp_lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    temp_lookahead = temp_lookahead + 1;
                },
                _ => {
                    if token.is_eq(symbol) {
                        return true
                    } else {
                        return false
                    }
                }
            }
        }
    }

    pub fn ignore_blanks(&mut self) {
        loop {
            let token = &self.token_vec[self.lookahead];
            match token.core_token {
                CoreToken::BLANK => {
                    self.lookahead = self.lookahead + 1;
                },
                _ => return
            }
        }
    }

    pub fn check_declaration(&self, token: &Token) -> Result<SymbolData, SemanticError> {
        let line_number = token.line_number;
        match &token.core_token {
            CoreToken::IDENTIFIER(token_value) => {
                match self.env.get(&token_value.0) {
                    Some(symbol_data) => Ok(symbol_data),
                    None => {
                        let index = self.get_index();
                        let err_message = format!("identifier '{}' is not declared in the current scope", token_value.0);
                        Err(SemanticError::new(
                            self.get_code_line(line_number, index),
                            err_message)
                        )
                    }
                }
            },
            _ => unreachable!("check_declaration cannot be used for tokens other than type identifier")
        }
    }

    pub fn has_lambda_type(&self, symbol_data: &SymbolData) -> Option<FunctionData> {
        let data_type = symbol_data.get_type();
        match data_type.get_user_defined_type_name() {
            Some(data_type_key) => {
                match self.env.get(&data_type_key) {
                    Some(type_data) => {
                        type_data.get_lambda_data()
                    },
                    None => {
                        None
                    }
                }
            },
            None => None,
        }
    }

    // parsing routines for terminals
    pub fn expect(&mut self, symbol: &str) -> Result<(ParseSuccess, usize), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        if String::from("empty").eq(symbol) {
            return Ok((ParseSuccess{
                lookahead: self.lookahead,
                possible_err: None,
            }, token.line_number))
        }
        if token.is_eq(symbol) {
            self.lookahead = self.lookahead + 1;
            Ok((ParseSuccess{
                lookahead: self.lookahead,
                possible_err: None,
            }, token.line_number))
        } else {
            let index = self.get_index();
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                self.get_code_line(token.line_number, index),
                format!(
                "expected '{}', got '{}'",
                PackratParser::parse_for_err_message(String::from(symbol)), 
                PackratParser::parse_for_err_message(token.name.to_string()))))
            )
        }
    }

    pub fn expect_any_id(&mut self) -> Result<(ParseSuccess, usize, Rc<String>), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::IDENTIFIER(token_value) => {
                self.lookahead = self.lookahead + 1;
                return Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None
                }, token.line_number, token_value.0.clone()))
            },
            _ => {
                let index = self.get_index();
                return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    self.get_code_line(token.line_number, index),
                    format!(
                    "expected identifier, got '{}'", 
                    PackratParser::parse_for_err_message(token.name.to_string()))))
                )
            }
        }
    }

    pub fn expect_any_id_in_scope(&mut self) -> Result<(ParseSuccess, usize, Rc<String>, SymbolData), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::IDENTIFIER(token_value) => {
                let symbol_data = self.check_declaration(&token)?;
                self.lookahead = self.lookahead + 1;
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, token.line_number, token_value.0.clone(), symbol_data))
            },
            _ => {
                let index = self.get_index();
                Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    self.get_code_line(token.line_number, index),
                    format!("expected identifier, got '{}'",
                    PackratParser::parse_for_err_message( token.name.to_string()))))
                )
            }
        }
    }

    pub fn expect_id(&mut self) -> Result<(ParseSuccess, usize, Rc<String>, Type, bool), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::IDENTIFIER(token_value) => {
                let symbol_data = self.check_declaration(&token)?;
                if let Some(response) = symbol_data.get_id_data() {
                    self.lookahead = self.lookahead + 1;
                    let (data_type, is_init) = (response.0, response.1);
                    Ok((ParseSuccess{
                        lookahead: self.lookahead,
                        possible_err: None,
                    }, token.line_number, token_value.0.clone(), data_type, is_init))
                } else {
                    let index = self.get_index();
                    Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                        self.get_code_line(token.line_number, index),
                        format!("expected identifier, got {} '{}'", 
                        symbol_data.get_category_of_identifier(), token_value.0.clone())))
                    )
                }
            },
            _ => {
                let index = self.get_index();
                Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    self.get_code_line(token.line_number, index),
                    format!("expected identifier, got '{}'",
                    PackratParser::parse_for_err_message( token.name.to_string()))))
                )
            }
        }
    }

    pub fn expect_type(&mut self)
    -> Result<(ParseSuccess, usize, Type), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::TYPE(token_value) => {
                self.lookahead = self.lookahead + 1;
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, token.line_number, Type::get_atomic_type(&token_value.0.clone().to_string())))
            },
            CoreToken::IDENTIFIER(token_value) => {
                let symbol_data = self.check_declaration(&token)?;
                if symbol_data.is_user_defined_struct_type() {
                    self.lookahead = self.lookahead + 1;
                    Ok((ParseSuccess{
                        lookahead: self.lookahead,
                        possible_err: None,
                    }, token.line_number, Type(Rc::new(CoreType::STRUCT(Struct{
                        name: token_value.0.clone(),
                    })))))
                } else if symbol_data.is_user_defined_lambda_type() {
                    self.lookahead = self.lookahead + 1;
                    Ok((ParseSuccess{
                        lookahead: self.lookahead,
                        possible_err: None,
                    }, token.line_number, Type(Rc::new(CoreType::LAMBDA(Lambda{
                        name: Some(token_value.0.clone()),
                    })))))
                } else {
                    let index = self.get_index();
                    Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                        self.get_code_line(token.line_number, index),
                        format!("expected type, got {} '{}'", 
                        symbol_data.get_category_of_identifier(), token_value.0.clone()))))
                }
            },
            _ => {
                let index = self.get_index();
                Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    self.get_code_line(token.line_number, index),
                    format!("expected type, got '{}'", 
                    PackratParser::parse_for_err_message( token.name.to_string())))))
            }
        }
    }

    pub fn expect_callable(&mut self)
    -> Result<(ParseSuccess, usize, Rc<String>, Rc<Vec<(Rc<String>, Type)>>, Rc<Option<Type>>), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::IDENTIFIER(token_value) => {
                let symbol_data = self.check_declaration(&token)?;
                if let Some(response) = symbol_data.get_function_data() {
                    self.lookahead = self.lookahead + 1;
                    let (params, return_type) = (response.params, response.return_type);
                    return Ok((ParseSuccess{
                        lookahead: self.lookahead,
                        possible_err: None,
                    }, token.line_number, token_value.0.clone(), params, return_type))
                } else if let Some(lambda_data) = self.has_lambda_type(&symbol_data) {
                    self.lookahead = self.lookahead + 1;
                    return Ok((ParseSuccess{
                        lookahead: self.lookahead,
                        possible_err: None,
                    }, token.line_number, token_value.0.clone(), lambda_data.params, lambda_data.return_type))
                } else if let Some(struct_constructor_data) = symbol_data.get_struct_constructor_data() {
                    self.lookahead = self.lookahead + 1;
                    return Ok((ParseSuccess{
                        lookahead: self.lookahead,
                        possible_err: None,
                    }, token.line_number, token_value.0.clone(), struct_constructor_data.params, struct_constructor_data.return_type))
                } else {
                    let index = self.get_index();
                    Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                        self.get_code_line(token.line_number, index),
                        format!("expected function or identifier with lambda type, got {} '{}'", 
                        symbol_data.get_category_of_identifier(), token_value.0.clone())))
                    )
                }
            },
            _ => {
                let index = self.get_index();
                Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    self.get_code_line(token.line_number, index),
                    format!("'{}' is not callable",
                    PackratParser::parse_for_err_message( token.name.to_string())))))
            }
        }
    }

    pub fn expect_indent_spaces(&mut self) -> Result<(ParseSuccess, i64), ParseError> {
        let expected_indent_spaces = context::get_indent() * self.indent_level;
        let mut indent_spaces = 0;
        loop {
            let token = &self.token_vec[self.lookahead];
            match &token.core_token {
                CoreToken::BLANK => indent_spaces = indent_spaces + 1,
                CoreToken::NEWLINE => indent_spaces = 0,
                CoreToken::TAB => {
                    let index = self.get_index();
                    let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                        self.get_code_line(token.line_number, index),
                        String::from(
                        "incorrectly indented statement\n    tabs are not allowed for indentation"))
                    );
                    return Err(err)
                },
                _ => {
                    if indent_spaces == expected_indent_spaces {
                        return Ok((ParseSuccess{
                            lookahead: self.lookahead,
                            possible_err: None,
                        }, indent_spaces))
                    } else {
                        let index = self.get_index();
                        let err =ParseError::SYNTAX_ERROR(SyntaxError::new(
                            self.get_code_line(token.line_number, index),
                            format!(
                                "incorrectly indented statement\n    expected indent of '{}' spaces, got '{}' spaces", 
                                expected_indent_spaces, indent_spaces)));
                        return Ok((ParseSuccess{
                            lookahead: self.lookahead,
                            possible_err: Some(err),
                        }, indent_spaces))
                    }
                }
            }
            self.lookahead = self.lookahead + 1;
        }
    }

    pub fn expect_zero_or_more<F: FnMut() -> Result<ParseSuccess, ParseError>>(mut f: F, 
        initial_lookahead: usize) -> ParseSuccess {
        let mut curr_lookahead = initial_lookahead;
        loop {
            match f() {
                Ok(response) => {
                    curr_lookahead = response.lookahead;
                    continue;
                },
                Err(err) => {
                    return ParseSuccess{
                        lookahead: curr_lookahead,
                        possible_err: Some(err)
                    };
                }
            }
        }
    }

    pub fn expect_optionally<T, F: FnMut() -> Result<T, ParseError>>(mut f: F, curr_value: T) -> (bool, T, Option<ParseError>) {
        match f() {
            Ok(response) => (true, response, None),
            Err(err) => {
                (false, curr_value, Some(err))
            }
        }
    }

    pub fn parse_for_err_message(message: String) -> String {
        let mut parsed_message = message;
        if parsed_message.eq("\n") {
            parsed_message = String::from("newline")
        }
        parsed_message
    }

    pub fn get_or_set_cache<T: std::fmt::Debug,
    F: FnOnce(&mut PackratParser) -> Result<T, ParseError>, 
    G: FnOnce(&Result<T, ParseError>) -> Result<T, ParseError>,
    H: FnOnce(&T) -> usize>(
        &mut self,
        cache_map: &Rc<RefCell<FxHashMap<usize, Result<T, ParseError>>>>,
        routine_fn: F,
        clone_result_fn: G,
        get_lookahead_fn: H,
        curr_lookahead: usize,
        message: &str,
    ) -> Result<T, ParseError> {
        match cache_map.borrow().get(&curr_lookahead) {
            Some(result) => {
                // println!("lookahead for cache hit for {}: {}\n", curr_lookahead, message);
                let result = clone_result_fn(result);
                match result {
                    Ok(response) => {
                        self.reset_lookahead(get_lookahead_fn(&response));
                        return Ok(response)
                    },
                    Err(err) => return Err(err)
                }
            },
            _ => {
                // println!("lookahead for cache missed for {}: {}\n", curr_lookahead, message);
                // print!("cache missed!\n");
            }
        }
        let result = routine_fn(self);
        let result_entry = clone_result_fn(&result);
        // println!("cache missed: pushing the entry = {:?} in map of {}", result_entry, message);
        cache_map.borrow_mut().insert(curr_lookahead, result_entry);
        result
    }

    // ------------------- production rule matching function for terminals and non-terminals declared below -------------------
    // code
    pub fn code(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        components::code::code(self, token_vec)
    }

    pub fn check_block_indentation(&mut self, 
        indent_spaces: i64, err: ParseError, curr_env: &Env, curr_lookahead: usize) -> Result<ParseSuccess, ParseError> {
        components::block::check_block_indentation(self, indent_spaces, err, curr_env, curr_lookahead)
    }

    pub fn block(&mut self, params: Option<&Vec<(Rc<String>, Type)>>) -> Result<ParseSuccess, ParseError> {
        components::block::block(self, params)
    }

    pub fn struct_block(&mut self) -> Result<(ParseSuccess, Vec<(Rc<String>, Type)>), ParseError> {
        components::block::struct_block(self)
    }

    pub fn impl_for_struct_block(&mut self, 
        struct_name: &Rc<String>) -> Result<ParseSuccess, ParseError> {
        components::block::impl_for_struct_block(self, struct_name)
    }

    // statements
    pub fn stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        components::stmt::stmt(self)
    }

    // type declaration
    pub fn type_decl_stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::type_declaration_stmt::type_decl_stmt(self)
    }

    pub fn struct_stmt(&mut self, identifier_name: &Rc<String>, index: usize) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::type_declaration_stmt::struct_stmt(self, identifier_name, index)
    }

    pub fn lambda_stmt(&mut self, identifier_name: &Rc<String>, index: usize) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::type_declaration_stmt::lambda_stmt(self, identifier_name, index)
    }

    pub fn impl_for_struct(&mut self) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::impl_for_struct::impl_for_struct(self)
    }

    // function declaration
    pub fn function_input_output(&mut self) 
    -> Result<(ParseSuccess, Vec<(Rc<String>, Type)>, bool, Option<Type>, Option<ParseError>), ParseError> {
        components::compound_stmt::function_stmt::function_input_output(self)
    }

    pub fn function_declaration(&mut self) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::function_stmt::function_declaration(self)
    }

    pub fn optparams(&mut self) -> Result<(ParseSuccess, Vec<(Rc<String>, Type)>), ParseError> {
        components::compound_stmt::function_stmt::optparams(self)
    }

    pub fn optparams_factor(&mut self) -> Result<(ParseSuccess, Vec<(Rc<String>, Type)>), ParseError> {
        components::compound_stmt::function_stmt::optparams_factor(self)
    }

    // simple statement - variable declaration and assignment
    pub fn simple_stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::core::simple_stmt(self)
    }

    pub fn simple_stmt_alternatives(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::core::simple_stmt_alternatives(self)
    }

    pub fn decls(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::declaration::decls(self)
    }

    pub fn decl(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::declaration::decl(self)
    }

    pub fn decl_factor(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::declaration::decl_factor(self)
    }

    pub fn assign(&mut self, data_type: Option<Type>,
        is_assignable: bool, index: usize) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::assignment::assign(self, data_type, is_assignable, index)
    }

    pub fn function_call(&mut self, 
        response: ParseSuccess, is_function_call: bool, index: usize) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::function_call::function_call(self, response, is_function_call, index)
    }

    pub fn r_assign(&mut self) -> Result<(ParseSuccess, Type, usize), ParseError> {
        components::simple_stmt::helper::r_assign(self)
    }

    pub fn param_decl(&mut self) -> Result<(ParseSuccess, usize, Type, Rc<String>), ParseError> {
        components::simple_stmt::helper::param_decl(self)
    }

    // expression
    pub fn expr(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        let routine_index = 1;  // routine_index for expr is 1
        let cache_map = self.cache[routine_index].clone();
        self.ignore_blanks();
        let curr_lookahead = self.lookahead;
        match cache_map.as_ref() {
            RoutineCache::EXPR(expr_cache_map) => {
                let routine_fn 
                = move |parser: &mut PackratParser| -> Result<(ParseSuccess, bool), ParseError> {
                    components::expression::expression::expr(parser)
                };
                let clone_result_fn 
                = move |result: &Result<(ParseSuccess, bool), ParseError>| -> Result<(ParseSuccess, bool), ParseError> {
                    clone_expr_result(result)
                };
                let get_lookahead_fn 
                = move |response: &(ParseSuccess, bool)| -> usize {
                    response.0.lookahead
                };
                self.get_or_set_cache(expr_cache_map, routine_fn, clone_result_fn, get_lookahead_fn, curr_lookahead, "expr")
            },
            _ => unreachable!("cache map with routine index 1 should be an expr")
        }
    }

    pub fn term(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::term(self)
    }

    pub fn additive(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::additive(self)
    }

    pub fn factor(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::factor(self)
    }

    pub fn multitive(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::multitive(self)
    }

    pub fn factor_expr_in_parenthesis(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::factor_expr_in_parenthesis(self)
    }

    pub fn factor_plus(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::factor_plus(self)
    }

    pub fn factor_minus(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::factor_minus(self)
    }

    pub fn multitive_star(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::multitive_star(self)
    }

    pub fn multitive_slash(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::multitive_slash(self)
    }

    pub fn additive_plus(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::additive_plus(self)
    }

    pub fn additive_minus(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::additive_minus(self)
    }

    // boolean expression
    pub fn bexpr(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bexpr(self)
    }

    pub fn ortive(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::ortive(self)
    }

    pub fn bterm(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bterm(self)
    }

    pub fn bfactor(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor(self)
    }

    pub fn bfactor_lookahead_one(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor_lookahead_one(self)
    }

    pub fn andtive(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::andtive(self)
    }

    pub fn comp_op(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::comp_op(self)
    }

    pub fn bfactor_expr_comp_op_expr(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor_expr_comp_op_expr(self)
    }

    pub fn bfactor_expr_in_parenthesis(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor_expr_in_parenthesis(self)
    }

    pub fn bfactor_not(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::bfactor_not(self)
    }

    pub fn andtive_and(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::andtive_and(self)
    }

    pub fn ortive_or(&mut self) -> Result<ParseSuccess, ParseError> {
        components::expression::bexpression::ortive_or(self)
    }

    // atom
    pub fn atom(&mut self) -> Result<(ParseSuccess, Option<Type>, bool, bool), ParseError> {
        let routine_index = 0;  // routine_index for atom is 0
        let cache_map = self.cache[routine_index].clone();
        self.ignore_blanks();
        let curr_lookahead = self.lookahead;
        match cache_map.as_ref() {
            RoutineCache::ATOM(atom_cache_map) => {
                let routine_fn 
                = move |parser: &mut PackratParser| -> Result<(ParseSuccess, Option<Type>, bool, bool), ParseError> {
                    components::atom::atom(parser)
                };
                let clone_result_fn 
                = move |result: &Result<(ParseSuccess, Option<Type>, bool, bool), ParseError>| -> Result<(ParseSuccess, Option<Type>, bool, bool), ParseError> {
                    clone_atom_result(result)
                };
                let get_lookahead_fn 
                = move |response: &(ParseSuccess, Option<Type>, bool, bool)| -> usize {
                    response.0.lookahead
                };
                self.get_or_set_cache(atom_cache_map, routine_fn, clone_result_fn, get_lookahead_fn, curr_lookahead, "atom")
            },
            _ => unreachable!("cache map with routine index 0 should be an atom")
        }
    }

    pub fn check_atom_factor(&mut self, 
        data_type: Option<Type>, 
        is_assignable: bool, is_function_call: bool) -> Result<(ParseSuccess, Option<Type>, bool, bool), ParseError> {
        components::atom::check_atom_factor(self, data_type, is_assignable, is_function_call)
    }

    pub fn params(&mut self) -> Result<(ParseSuccess, usize, Vec<(Type, usize)>), ParseError> {
        components::function::params(self)
    }

    pub fn param(&mut self) -> Result<(ParseSuccess, (Type, usize)), ParseError> {
        components::function::param(self)
    }

    pub fn atom_factor(&mut self) -> Result<(ParseSuccess, usize, Vec<components::atom::CompoundPart>), ParseError> {
        components::atom::atom_factor(self)
    }
    
    pub fn atom_index_or_propetry_or_method_access(&mut self) -> Result<(ParseSuccess, Option<components::atom::CompoundPart>), ParseError> {
        components::atom::atom_index_or_propetry_or_method_access(self)
    }

    pub fn atom_index_access(&mut self) -> Result<(ParseSuccess, components::atom::CompoundPart), ParseError> {
        components::atom::atom_index_access(self)
    }
    
    pub fn atom_propertry_or_method_access(&mut self) -> Result<(ParseSuccess, components::atom::CompoundPart), ParseError> {
        components::atom::atom_propertry_or_method_access(self)
    }
}