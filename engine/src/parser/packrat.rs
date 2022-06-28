// Default parser for jarvil is Packrat developed by Bryan Ford in his master thesis at MIT. It is essentially a top down 
// recursive parsing with optimized backtracting in order to avoid exponential parse time and provide reliable linear time 
// parsing!
// See https://pdos.csail.mit.edu/~baford/packrat/thesis/ for more information.

use crate::parser::core::Parser;
use crate::lexer::token::{Token, CoreToken, TokenValue};
use crate::parser::ast::AST;
use std::rc::Rc;
use crate::errors::{ParseError, SyntaxError, SemanticError};
use crate::scope::{Env, SymbolData, UserDefinedTypeData, FunctionData};
use crate::parser::components;
use crate::context;
use rustc_hash::FxHashMap;

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
    // TODO - add look up hash table for cached results
    // TODO - add AST data structure
}

impl PackratParser {
    pub fn new(code_lines: Vec<(Rc<String>, usize)>) -> Self {
        let env = Env::new();
        PackratParser {
            token_vec: Vec::new(),
            lookahead: 0,
            indent_level: -1,
            env,
            code_lines,
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
    pub fn get_lookahead(&self) -> usize {
        self.lookahead
    }

    pub fn reset_lookahead(&mut self, reset_index: usize) {
        self.lookahead = reset_index;
    }

    pub fn get_index(&self) -> usize {
        (self.token_vec[self.lookahead].start_index + self.token_vec[self.lookahead].end_index) / 2 as usize
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

    pub fn set_identifier_to_scope(&mut self, identifier_name: &Rc<String>, data_type: &Rc<String>, is_init: bool) {
        self.env.set_identifier(identifier_name, data_type, is_init);
    }

    pub fn set_identifier_init_to_scope(&mut self, identifier_name: &Rc<String>) {
        self.env.set_identifier_init(identifier_name)
    }

    pub fn set_user_defined_struct_type_to_scope(&mut self, 
        identifier_name: &Rc<String>, fields: &Rc<FxHashMap<Rc<String>, Rc<String>>>) {
        self.env.set_user_defined_struct_type(identifier_name, fields);
    }

    pub fn set_user_defined_lambda_type(&mut self, identifier_name: &Rc<String>, 
        params: &Rc<Vec<(Rc<String>, Rc<String>)>>, return_type: &Rc<Option<Rc<String>>>) {
        self.env.set_user_defined_lambda_type(identifier_name, params, return_type);
    }

    pub fn set_function_to_scope(&mut self, identifier_name: &Rc<String>, 
        params: &Rc<Vec<(Rc<String>, Rc<String>)>>, return_type: &Rc<Option<Rc<String>>>) {
        self.env.set_function(identifier_name, params, return_type);
    }

    pub fn set_params_to_scope(&mut self, params: Option<&Vec<(Rc<String>, Rc<String>)>>) {
        if let Some(params) = params {
            for (identifier_name, data_type) in params {
                self.set_identifier_to_scope(identifier_name, data_type, true);
            }
        }
    }

    pub fn has_field_with_name(&self, data_type: &Rc<String>, field_name: &Rc<String>) -> Option<Rc<String>> {
        match self.env.get(data_type) {
            Some(symbol_data) => {
                match &symbol_data.has_field_name(field_name) {
                    Some(val) => Some(val.clone()),
                    None => None,
                }
            },
            None => None
        }
    }

    pub fn has_method_with_name(&self, data_type: &Rc<String>, 
        method_name: &Rc<String>) -> Option<FunctionData> {
        match self.env.get(data_type) {
            Some(symbol_data) => {
                match &symbol_data.has_method_name(method_name) {
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
        match self.env.get(&data_type) {
            Some(type_data) => {
                type_data.get_lambda_data()
            },
            None => {
                None
            }
        }
    }

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
                    "expected an identifier, got '{}'", 
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
                    format!("expected an identifier, got '{}'",
                    PackratParser::parse_for_err_message( token.name.to_string()))))
                )
            }
        }
    }

    pub fn expect_id(&mut self) -> Result<(ParseSuccess, usize, Rc<String>, Rc<String>, bool), ParseError> {
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
                        format!("expected an identifier, got a {} '{}'", 
                        symbol_data.get_type_of_identifier(), token_value.0.clone())))
                    )
                }
            },
            _ => {
                let index = self.get_index();
                Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    self.get_code_line(token.line_number, index),
                    format!("expected an identifier, got '{}'",
                    PackratParser::parse_for_err_message( token.name.to_string()))))
                )
            }
        }
    }

    pub fn expect_type(&mut self)
    -> Result<(ParseSuccess, usize, Rc<String>, Option<UserDefinedTypeData>), ParseError> {
        self.ignore_blanks();
        let token = &self.token_vec[self.lookahead];
        match &token.core_token {
            CoreToken::TYPE(token_value) => {
                self.lookahead = self.lookahead + 1;
                Ok((ParseSuccess{
                    lookahead: self.lookahead,
                    possible_err: None,
                }, token.line_number, token_value.0.clone(), None))
            },
            CoreToken::IDENTIFIER(token_value) => {
                let symbol_data = self.check_declaration(&token)?;
                if let Some(response) = symbol_data.get_user_defined_type_data() {
                    self.lookahead = self.lookahead + 1;
                    Ok((ParseSuccess{
                        lookahead: self.lookahead,
                        possible_err: None,
                    }, token.line_number, token_value.0.clone(), Some(response)))
                } else {
                    let index = self.get_index();
                    Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                        self.get_code_line(token.line_number, index),
                        format!("expected a type, got a {} '{}'", 
                        symbol_data.get_type_of_identifier(), token_value.0.clone()))))
                }
            },
            _ => {
                let index = self.get_index();
                Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    self.get_code_line(token.line_number, index),
                    format!("expected a type, got '{}'", 
                    PackratParser::parse_for_err_message( token.name.to_string())))))
            }
        }
    }

    pub fn expect_callable(&mut self)
    -> Result<(ParseSuccess, usize, Rc<String>, Rc<Vec<(Rc<String>, Rc<String>)>>, Rc<Option<Rc<String>>>), ParseError> {
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
                } else {
                    let index = self.get_index();
                    Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                        self.get_code_line(token.line_number, index),
                        format!("expected a function or an identifier with lambda type, got a {} '{}'", 
                        symbol_data.get_type_of_identifier(), token_value.0.clone())))
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
                CoreToken::TAB => unimplemented!("yet to handle tabs in indentation"),
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
                                "incorrectly indented statement\n    expected indent of {} spaces, got {} spaces", 
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


    // ------------------- production rule matching function for terminals and non-terminals declared below -------------------
    // code
    pub fn code(&mut self, token_vec: Vec<Token>) -> Result<(), ParseError> {
        components::code::code(self, token_vec)
    }

    pub fn block(&mut self, params: Option<&Vec<(Rc<String>, Rc<String>)>>) -> Result<ParseSuccess, ParseError> {
        components::block::block(self, params)
    }

    pub fn struct_block(&mut self) -> Result<(ParseSuccess, FxHashMap<Rc<String>, Rc<String>>), ParseError> {
        components::block::struct_block(self)
    }

    // statements
    pub fn stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        components::stmt::stmt(self)
    }

    pub fn type_decl_stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::type_declaration_stmt::type_decl_stmt(self)
    }

    pub fn struct_stmt(&mut self, identifier_name: &Rc<String>) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::type_declaration_stmt::struct_stmt(self, identifier_name)
    }

    pub fn lambda_stmt(&mut self, identifier_name: &Rc<String>) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::type_declaration_stmt::lambda_stmt(self, identifier_name)
    }

    pub fn function_input_output(&mut self) 
    -> Result<(ParseSuccess, Vec<(Rc<String>, Rc<String>)>, bool, Option<Rc<String>>, Option<ParseError>), ParseError> {
        components::compound_stmt::function_stmt::function_input_output(self)
    }

    pub fn function_declaration(&mut self) -> Result<ParseSuccess, ParseError> {
        components::compound_stmt::function_stmt::function_declaration(self)
    }

    pub fn optparams(&mut self) -> Result<(ParseSuccess, Vec<(Rc<String>, Rc<String>)>), ParseError> {
        components::compound_stmt::function_stmt::optparams(self)
    }

    pub fn optparams_factor(&mut self) -> Result<(ParseSuccess, Vec<(Rc<String>, Rc<String>)>), ParseError> {
        components::compound_stmt::function_stmt::optparams_factor(self)
    }

    pub fn simple_stmts(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::core::simple_stmts(self)
    }

    pub fn simple_stmt(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::core::simple_stmt(self)
    }

    // simple statement - decl, assign
    pub fn decls(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::declaration::decls(self)
    }

    pub fn decl(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::declaration::decl(self)
    }

    pub fn decl_factor(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::declaration::decl_factor(self)
    }

    pub fn assign(&mut self) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::assignment::assign(self)
    }

    pub fn r_assign(&mut self) -> Result<(ParseSuccess, Rc<String>), ParseError> {
        components::simple_stmt::declaration::r_assign(self)
    }

    pub fn param_decl(&mut self) -> Result<(ParseSuccess, usize, Rc<String>, Rc<String>), ParseError> {
        components::simple_stmt::helper::param_decl(self)
    }

    pub fn r_asssign(&mut self, rule_index: usize, line_number: usize) -> Result<ParseSuccess, ParseError> {
        components::simple_stmt::helper::r_asssign(self, rule_index, line_number)
    }

    // expression
    pub fn expr(&mut self) -> Result<(ParseSuccess, bool), ParseError> {
        components::expression::expression::expr(self)
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
    pub fn atom(&mut self) -> Result<(ParseSuccess, Option<Rc<String>>), ParseError> {
        components::atom::atom(self)
    }

    pub fn check_atom_factor(&mut self, 
        data_type: Option<Rc<String>>) -> Result<(ParseSuccess, Option<Rc<String>>), ParseError> {
        components::atom::check_atom_factor(self, data_type)
    }

    pub fn params(&mut self) -> Result<(ParseSuccess, usize, Vec<(Rc<String>, usize)>), ParseError> {
        components::function::params(self)
    }

    pub fn param(&mut self) -> Result<(ParseSuccess, (Rc<String>, usize)), ParseError> {
        components::function::param(self)
    }

    pub fn atom_factor(&mut self) -> Result<(ParseSuccess, usize, Vec<components::atom::CompoundPart>), ParseError> {
        components::atom::atom_factor(self)
    }
    
    pub fn atom_index_or_propetry_access(&mut self) -> Result<(ParseSuccess, Option<components::atom::CompoundPart>), ParseError> {
        components::atom::atom_index_or_propetry_access(self)
    }

    pub fn atom_index_access(&mut self) -> Result<(ParseSuccess, components::atom::CompoundPart), ParseError> {
        components::atom::atom_index_access(self)
    }
    
    pub fn atom_propertry_or_method_access(&mut self) -> Result<(ParseSuccess, components::atom::CompoundPart), ParseError> {
        components::atom::atom_propertry_or_method_access(self)
    }

    pub fn atom_expr_bexpr_literal(&mut self) -> Result<(ParseSuccess, Rc<String>), ParseError> {
        components::atom::atom_expr_bexpr_literal(self)
    }
}