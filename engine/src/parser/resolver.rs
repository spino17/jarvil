use crate::constants::common::EIGHT_BIT_MAX_VALUE;
use crate::error::diagnostics::{
    LocalVariableDeclarationLimitReachedError, MoreThanMaxLimitParamsPassedError,
};
use crate::error::helper::IdentifierKind as IdentKind;
use crate::scope::core::VariableCaptureKind;
use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, CoreAtomStartNode, CoreIdentifierNode, CoreNameTypeSpecsNode,
            CoreRAssignmentNode, CoreStatemenIndentWrapperNode, CoreStatementNode,
            FunctionDeclarationNode, FunctionKind, LambdaDeclarationNode, Node,
            OkFunctionDeclarationNode, OkIdentifierNode, OkLambdaTypeDeclarationNode,
            StructDeclarationNode, TypeExpressionNode, TypeResolveKind, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::Code,
    error::{
        constants::{
            LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG, SCOPE_NOT_SET_TO_BLOCK_MSG,
            STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG,
        },
        diagnostics::{Diagnostics, IdentifierAlreadyDeclaredError, IdentifierNotDeclaredError},
    },
    scope::{
        core::{IdentifierKind, Namespace, SymbolData},
        function::FunctionData,
        user_defined_types::{LambdaTypeData, UserDefinedTypeData},
        variables::VariableData,
    },
    types::core::Type,
};
use rustc_hash::FxHashMap;
use std::{rc::Rc, vec};
use text_size::TextRange;

pub enum ResolverMode {
    DECLARE, // first pass
    RESOLVE, // second pass
}

pub struct RuntimeStackSimulator {
    local_indexes: Vec<usize>,   // simulation of runtime snapshots of stack
    curr_depth: usize,           // curr depth in blocks starting with 0
    curr_local_var_index: usize, // relative index of the local variable
}

impl Default for RuntimeStackSimulator {
    fn default() -> Self {
        RuntimeStackSimulator {
            local_indexes: vec![0],
            curr_depth: 0,
            curr_local_var_index: 0,
        }
    }
}

impl RuntimeStackSimulator {
    pub fn variable_decl_callback(&mut self) -> Result<usize, usize> {
        self.local_indexes[self.curr_depth] += 1;
        let curr_index = self.curr_local_var_index;
        self.curr_local_var_index += 1;
        if self.curr_local_var_index > EIGHT_BIT_MAX_VALUE {
            return Err(curr_index);
        }
        Ok(curr_index)
    }

    pub fn rollback_variable_decl(&mut self) {
        self.local_indexes[self.curr_depth] -= 1;
        self.curr_local_var_index -= 1;
    }

    pub fn open_block(&mut self) {
        self.curr_depth += 1;
        if self.curr_depth >= self.local_indexes.len() {
            self.local_indexes.push(0);
        } else {
            self.local_indexes[self.curr_depth] = 0;
        }
    }

    pub fn close_block(&mut self) -> usize {
        let num_of_popped_elements = self.local_indexes[self.curr_depth];
        self.curr_local_var_index = self.curr_local_var_index - self.local_indexes[self.curr_depth];
        self.curr_depth -= 1;
        num_of_popped_elements
    }
}

#[derive(Clone)]
struct UpValue {
    index: usize, // if is_local is `true` then this would be relative stack_index of the captured local variable
    is_local: bool,
}

pub struct FunctionContext {
    upvalues: Vec<UpValue>,
    frame_stack: RuntimeStackSimulator,
    is_local_var_limit_overflow: bool,
    range: TextRange,
}

impl FunctionContext {
    fn new(range: TextRange) -> Self {
        FunctionContext {
            upvalues: vec![],
            frame_stack: RuntimeStackSimulator::default(),
            is_local_var_limit_overflow: false,
            range,
        }
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool) -> usize {
        let value = UpValue { index, is_local };
        self.upvalues.push(value);
        self.upvalues.len()
    }
}

pub struct Resolver {
    namespace: Namespace,
    pub code: Code,
    errors: Vec<Diagnostics>,
    mode: ResolverMode,
    func_context: Vec<FunctionContext>,
}

impl Resolver {
    pub fn new(code: &Code) -> Self {
        Resolver {
            namespace: Namespace::new(),
            code: code.clone(),
            errors: vec![],
            mode: ResolverMode::DECLARE,
            func_context: vec![],
        }
    }

    pub fn func_context(&mut self) -> &mut FunctionContext {
        let len = self.func_context.len();
        &mut self.func_context[len - 1]
    }

    pub fn add_upvalue_to_func(
        &mut self,
        func_index: usize,
        index: usize,
        is_local: bool,
    ) -> usize {
        self.func_context[func_index].add_upvalue(index, is_local)
    }

    pub fn open_block(&mut self) {
        self.namespace.open_scope();
        self.func_context().frame_stack.open_block();
    }

    pub fn close_block(&mut self) {
        self.namespace.close_scope();
        self.func_context().frame_stack.close_block();
    }

    pub fn variable_decl_callback(&mut self) -> usize {
        let len = self.func_context.len();
        let curr_func_context = &mut self.func_context[len - 1];
        match curr_func_context.frame_stack.variable_decl_callback() {
            Ok(stack_index) => stack_index,
            Err(stack_index) => {
                if !curr_func_context.is_local_var_limit_overflow {
                    let err = LocalVariableDeclarationLimitReachedError::new(
                        EIGHT_BIT_MAX_VALUE,
                        curr_func_context.range,
                    );
                    self.errors
                        .push(Diagnostics::LocalVariableDeclarationLimitReached(err));
                    curr_func_context.is_local_var_limit_overflow = true;
                }
                return stack_index;
            }
        }
    }

    pub fn rollback_variable_decl(&mut self) {
        self.func_context().frame_stack.rollback_variable_decl();
    }

    pub fn open_func(&mut self, range: TextRange) {
        self.namespace.open_scope();
        self.func_context.push(FunctionContext::new(range));
    }

    pub fn close_func(&mut self) -> FunctionContext {
        self.namespace.close_scope();
        self.func_context
            .pop()
            .expect("`func_context` will never be empty")
    }

    pub fn resolve_ast(mut self, ast: &BlockNode) -> (Namespace, Vec<Diagnostics>) {
        self.func_context.push(FunctionContext::new(ast.range()));
        let code_block = ast.0.as_ref().borrow();
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.mode = ResolverMode::RESOLVE;
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        assert!(self.func_context().upvalues.len() == 0); // top-level block cannot have upvalues
        (self.namespace, self.errors)
    }

    pub fn try_declare_and_bind<
        T,
        U: Fn(&Namespace, &Rc<String>, TextRange) -> Result<SymbolData<T>, TextRange>,
        V: Fn(&OkIdentifierNode, &SymbolData<T>),
    >(
        &mut self,
        identifier: &OkIdentifierNode,
        declare_fn: U,
        bind_fn: V,
    ) -> Option<(Rc<String>, TextRange)> {
        let name = Rc::new(identifier.token_value(&self.code));
        let symbol_data = declare_fn(&self.namespace, &name, identifier.range());
        match symbol_data {
            Ok(symbol_data) => {
                bind_fn(identifier, &symbol_data);
                None
            }
            Err(previous_decl_range) => Some((name, previous_decl_range)),
        }
    }

    pub fn try_resolving<
        T,
        U: Fn(&Namespace, &Rc<String>) -> Option<(SymbolData<T>, usize)>,
        V: Fn(&OkIdentifierNode, &SymbolData<T>, usize),
    >(
        &mut self,
        identifier: &OkIdentifierNode,
        lookup_fn: U,
        bind_fn: V,
    ) -> Option<Rc<String>> {
        let name = Rc::new(identifier.token_value(&self.code));
        match lookup_fn(&self.namespace, &name) {
            Some(symbol_data) => {
                bind_fn(identifier, &symbol_data.0, symbol_data.1);
                None
            }
            None => Some(name),
        }
    }

    pub fn try_resolving_variable(&mut self, identifier: &OkIdentifierNode) -> Option<Rc<String>> {
        let lookup_fn =
            |namespace: &Namespace, key: &Rc<String>| namespace.lookup_in_variables_namespace(key);
        let bind_fn =
            |identifier: &OkIdentifierNode,
             symbol_data: &SymbolData<VariableData>,
             depth: usize| { identifier.bind_variable_decl(symbol_data, depth) };
        self.try_resolving(identifier, lookup_fn, bind_fn)
    }

    pub fn lookup_in_variables_namespace_with_upvalues(
        &mut self,
        key: &Rc<String>,
    ) -> Option<(SymbolData<VariableData>, usize, VariableCaptureKind)> {
        let mut curr_func_context_index = self.func_context.len() - 1;
        let mut total_resolved_depth = 0;
        let mut curr_scope = self.namespace.variable_scope().clone();
        while curr_func_context_index >= 0 {
            let curr_depth = self.func_context[curr_func_context_index]
                .frame_stack
                .curr_depth
                + 1;
            let mut curr_scope_depth = 1;
            while curr_depth >= curr_scope_depth {
                match curr_scope.get(&key) {
                    Some(symbol_data) => {
                        let capture_kind = if curr_func_context_index == self.func_context.len() - 1
                        {
                            VariableCaptureKind::LOCAL
                        } else {
                            symbol_data.0.as_ref().borrow_mut().set_is_captured();
                            self.add_upvalue_to_func(
                                curr_func_context_index + 1,
                                symbol_data.0.as_ref().borrow().stack_index(),
                                true,
                            );
                            VariableCaptureKind::UPVALUE(
                                self.func_context
                                    .last()
                                    .expect("`func_context` will never be empty")
                                    .upvalues
                                    .last()
                                    .expect("`upvalues` at this point cannot be empty")
                                    .index,
                            )
                        };
                        return Some((symbol_data, total_resolved_depth, capture_kind));
                    }
                    None => match &curr_scope.parent() {
                        Some(parent_scope) => {
                            curr_scope = parent_scope.clone();
                        }
                        None => return None,
                    },
                };
                curr_scope_depth += 1;
                total_resolved_depth += 1;
            }
            if curr_func_context_index < self.func_context.len() - 1 {
                self.add_upvalue_to_func(
                    curr_func_context_index + 1,
                    self.func_context[curr_func_context_index].upvalues.len(),
                    false,
                );
            }
            curr_func_context_index -= 1;
        }
        None
    }

    pub fn try_resolving_function(&mut self, identifier: &OkIdentifierNode) -> Option<Rc<String>> {
        let lookup_fn =
            |namespace: &Namespace, key: &Rc<String>| namespace.lookup_in_functions_namespace(key);
        let bind_fn =
            |identifier: &OkIdentifierNode,
             symbol_data: &SymbolData<FunctionData>,
             depth: usize| { identifier.bind_function_decl(symbol_data, depth) };
        self.try_resolving(identifier, lookup_fn, bind_fn)
    }

    pub fn try_resolving_user_defined_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<Rc<String>> {
        let lookup_fn =
            |namespace: &Namespace, key: &Rc<String>| namespace.lookup_in_types_namespace(key);
        let bind_fn = |identifier: &OkIdentifierNode,
                       symbol_data: &SymbolData<UserDefinedTypeData>,
                       depth: usize| {
            identifier.bind_user_defined_type_decl(symbol_data, depth)
        };
        self.try_resolving(identifier, lookup_fn, bind_fn)
    }

    pub fn try_declare_and_bind_variable(
        &mut self,
        identifier: &OkIdentifierNode,
        stack_index: usize,
    ) -> Option<(Rc<String>, TextRange)> {
        let declare_fn = |namespace: &Namespace,
                          name: &Rc<String>,
                          stack_index: usize,
                          decl_range: TextRange| {
            namespace.declare_variable(name, stack_index, decl_range)
        };
        let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<VariableData>| {
            identifier.bind_variable_decl(symbol_data, 0)
        };
        let name = Rc::new(identifier.token_value(&self.code));
        let symbol_data = declare_fn(&self.namespace, &name, stack_index, identifier.range());
        match symbol_data {
            Ok(symbol_data) => {
                bind_fn(identifier, &symbol_data);
                return None;
            }
            Err(previous_decl_range) => return Some((name, previous_decl_range)),
        }
    }

    pub fn try_declare_and_bind_function(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, TextRange)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, decl_range: TextRange| {
            namespace.declare_function(name, decl_range)
        };
        let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<FunctionData>| {
            identifier.bind_function_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn try_declare_and_bind_struct_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, TextRange)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, decl_range: TextRange| {
            namespace.declare_struct_type(name, decl_range)
        };
        let bind_fn = |identifier: &OkIdentifierNode,
                       symbol_data: &SymbolData<UserDefinedTypeData>| {
            identifier.bind_user_defined_type_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn try_declare_and_bind_lambda_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, TextRange)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, decl_range: TextRange| {
            namespace.declare_lambda_type(name, decl_range)
        };
        let bind_fn = |identifier: &OkIdentifierNode,
                       symbol_data: &SymbolData<UserDefinedTypeData>| {
            identifier.bind_user_defined_type_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn declare_variable(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        self.walk_r_assignment(&core_variable_decl.r_assign);
        if let CoreRAssignmentNode::LAMBDA(_) = core_variable_decl.r_assign.core_ref() {
            return;
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
            let stack_index = self.variable_decl_callback();
            if let Some((name, previous_decl_range)) =
                self.try_declare_and_bind_variable(ok_identifier, stack_index)
            {
                self.rollback_variable_decl();
                let err = IdentifierAlreadyDeclaredError::new(
                    IdentKind::VARIABLE,
                    name.to_string(),
                    previous_decl_range,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierAlreadyDeclared(err));
            }
        }
    }

    pub fn declare_function(&mut self, func_decl: &OkFunctionDeclarationNode) {
        let core_func_decl = func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let params = &core_func_decl.params;
        let func_body = &core_func_decl.block;
        let kind = &core_func_decl.kind;
        self.open_func(func_decl.range());
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let param_name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = param_name.core_ref() {
                    let stack_index = self.variable_decl_callback();
                    if let Some((name, previous_decl_range)) =
                        self.try_declare_and_bind_variable(ok_identifier, stack_index)
                    {
                        self.rollback_variable_decl();
                        let err = IdentifierAlreadyDeclaredError::new(
                            IdentKind::VARIABLE,
                            name.to_string(),
                            previous_decl_range,
                            ok_identifier.range(),
                        );
                        self.errors
                            .push(Diagnostics::IdentifierAlreadyDeclared(err));
                    }
                }
            }
        }
        self.walk_block(func_body);
        func_body.set_scope(&self.namespace);
        let func_context = self.close_func();
        // TODO - add this func_contex to argument `OkFunctionDeclarationNode` node
        if let Some(identifier) = func_name {
            if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                match kind {
                    FunctionKind::FUNC => {
                        if let Some((name, previous_decl_range)) =
                            self.try_declare_and_bind_function(ok_identifier)
                        {
                            let err = IdentifierAlreadyDeclaredError::new(
                                IdentKind::FUNCTION,
                                name.to_string(),
                                previous_decl_range,
                                ok_identifier.range(),
                            );
                            self.errors
                                .push(Diagnostics::IdentifierAlreadyDeclared(err));
                        }
                    }
                    FunctionKind::LAMBDA => {
                        let stack_index = self.variable_decl_callback();
                        if let Some((name, previous_decl_range)) =
                            self.try_declare_and_bind_variable(ok_identifier, stack_index)
                        {
                            self.rollback_variable_decl();
                            let err = IdentifierAlreadyDeclaredError::new(
                                IdentKind::VARIABLE,
                                name.to_string(),
                                previous_decl_range,
                                ok_identifier.range(),
                            );
                            self.errors
                                .push(Diagnostics::IdentifierAlreadyDeclared(err));
                        }
                    }
                    FunctionKind::METHOD => unimplemented!(),
                }
            }
        }
    }

    pub fn declare_struct(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_struct_decl.name.core_ref() {
            if let Some((name, previous_decl_range)) =
                self.try_declare_and_bind_struct_type(ok_identifier)
            {
                let err = IdentifierAlreadyDeclaredError::new(
                    IdentKind::TYPE,
                    name.to_string(),
                    previous_decl_range,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierAlreadyDeclared(err));
            }
        }
    }

    pub fn declare_lambda_type(&mut self, lambda_type_decl: &OkLambdaTypeDeclarationNode) {
        let core_lambda_type_decl = lambda_type_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            if let Some((name, previous_decl_range)) =
                self.try_declare_and_bind_lambda_type(ok_identifier)
            {
                let err = IdentifierAlreadyDeclaredError::new(
                    IdentKind::TYPE,
                    name.to_string(),
                    previous_decl_range,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierAlreadyDeclared(err));
            }
        }
    }

    pub fn type_obj_from_expression(&mut self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj_before_resolved(&self.namespace, &self.code) {
            TypeResolveKind::RESOLVED(type_obj) => type_obj,
            TypeResolveKind::UNRESOLVED(identifier) => {
                let err = IdentifierNotDeclaredError::new(IdentKind::TYPE, identifier.range());
                self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                return Type::new_with_unknown();
            }
            TypeResolveKind::INVALID => Type::new_with_unknown(),
        }
    }

    pub fn resolve_function(&mut self, func_decl: &OkFunctionDeclarationNode) {
        let core_func_decl = func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let params = &core_func_decl.params;
        let return_type = &core_func_decl.return_type;
        let func_body = &core_func_decl.block;
        let rparen = &core_func_decl.rparen;
        let mut params_vec: Vec<(Rc<String>, Type)> = vec![];
        let return_type: Type = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        let mut params_count: usize = 0;
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                    if let Some(symbol_data) = ok_identifier.variable_symbol_data(
                        "param name should be resolved to `SymbolData<VariableData>`",
                    ) {
                        let variable_name = Rc::new(ok_identifier.token_value(&self.code));
                        let type_obj = self.type_obj_from_expression(&core_param.data_type);
                        symbol_data.0.as_ref().borrow_mut().set_data_type(&type_obj);
                        params_vec.push((variable_name, type_obj));
                        params_count += 1;
                    }
                }
            }
        }
        if params_count > EIGHT_BIT_MAX_VALUE {
            let err = MoreThanMaxLimitParamsPassedError::new(
                params_count,
                EIGHT_BIT_MAX_VALUE,
                rparen.range(),
            );
            self.errors
                .push(Diagnostics::MoreThanMaxLimitParamsPassed(err));
        }
        self.namespace = func_body.scope().expect(SCOPE_NOT_SET_TO_BLOCK_MSG);
        self.walk_block(func_body);
        self.namespace.close_scope();
        let kind = &core_func_decl.kind;
        if let Some(identifier) = func_name {
            if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                if let Some(symbol_data) = ok_identifier.symbol_data() {
                    match symbol_data.0 {
                        IdentifierKind::FUNCTION(func_symbol_data) => {
                            assert!(kind.clone() == FunctionKind::FUNC);
                            func_symbol_data
                                .0
                                .as_ref()
                                .borrow_mut()
                                .set_data(params_vec, return_type);
                        }
                        IdentifierKind::VARIABLE(variable_symbol_data) => {
                            assert!(kind.clone() == FunctionKind::LAMBDA);
                            let symbol_data = UserDefinedTypeData::LAMBDA(LambdaTypeData::new(
                                params_vec,
                                return_type,
                            ));
                            let lambda_type_obj = Type::new_with_lambda(
                                None,
                                &SymbolData::new(symbol_data, ok_identifier.range()),
                            );
                            variable_symbol_data
                                .0
                                .as_ref()
                                .borrow_mut()
                                .set_data_type(&lambda_type_obj);
                        }
                        _ => unreachable!(
                            "function name should be resolved to `SymbolData<FunctionData>` or `SymbolData<VariableData>`"
                        ),
                    }
                }
            }
        }
    }

    pub fn resolve_struct(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        let mut fields_map: FxHashMap<String, (Type, TextRange)> = FxHashMap::default();
        let struct_body = &core_struct_decl.block;
        for stmt in &struct_body.0.as_ref().borrow().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(stmt) => stmt.clone(),
                CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(stmt) => {
                    stmt.core_ref().stmt.clone()
                }
                _ => continue,
            };
            match stmt.core_ref() {
                CoreStatementNode::STRUCT_STATEMENT(struct_stmt) => {
                    let core_struct_stmt = struct_stmt.core_ref();
                    let name = &core_struct_stmt.name_type_spec.core_ref().name;
                    if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                        let field_name = ok_identifier.token_value(&self.code);
                        let type_obj = self.type_obj_from_expression(
                            &core_struct_stmt.name_type_spec.core_ref().data_type
                        );
                        match fields_map.get(&field_name) {
                            Some((_, previous_decl_range)) => {
                                let err = IdentifierAlreadyDeclaredError::new(
                                    IdentKind::FIELD,
                                    field_name,
                                    *previous_decl_range,
                                    ok_identifier.range()
                                );
                                self.errors.push(Diagnostics::IdentifierAlreadyDeclared(err));
                            },
                            None => {
                                fields_map.insert(field_name, (type_obj, ok_identifier.range()));
                            }
                        }
                    }
                },
                _ => unreachable!("statements other than `StructStatementNode` are not allowed in struct declaration block"),
            }
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_struct_decl.name.core_ref() {
            if let Some(symbol_data) = ok_identifier.user_defined_type_symbol_data(
                "struct name should be resolved to `SymbolData<UserDefinedTypeData>`",
            ) {
                symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                    .struct_data_mut(STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG)
                    .set_fields(fields_map);
            }
        }
    }

    pub fn resolve_lambda_type(&mut self, lambda_type_decl: &OkLambdaTypeDeclarationNode) {
        let core_lambda_type_decl = lambda_type_decl.core_ref();
        let mut params_vec: Vec<(Rc<String>, Type)> = vec![];
        let params = &core_lambda_type_decl.params;
        let return_type = &core_lambda_type_decl.return_type;
        let rparen = &core_lambda_type_decl.rparen;
        let return_type: Type = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        let mut params_count = 0;
        if let Some(params) = params {
            let params_iter = params.iter();
            let mut params_map: FxHashMap<Rc<String>, TextRange> = FxHashMap::default();
            for param in params_iter {
                let core_param = param.core_ref();
                let name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                    let variable_name = Rc::new(ok_identifier.token_value(&self.code));
                    let range = ok_identifier.range();
                    if let Some(previous_decl_range) = params_map.get(&variable_name) {
                        let err = IdentifierAlreadyDeclaredError::new(
                            IdentKind::ARGUMENT,
                            variable_name.to_string(),
                            *previous_decl_range,
                            range,
                        );
                        self.errors
                            .push(Diagnostics::IdentifierAlreadyDeclared(err));
                        continue;
                    }
                    let type_obj = self.type_obj_from_expression(&core_param.data_type);
                    params_map.insert(variable_name.clone(), range);
                    params_vec.push((variable_name, type_obj));
                    params_count += 1;
                }
            }
        }
        if params_count > EIGHT_BIT_MAX_VALUE {
            let err = MoreThanMaxLimitParamsPassedError::new(
                params_count,
                EIGHT_BIT_MAX_VALUE,
                rparen.range(),
            );
            self.errors
                .push(Diagnostics::MoreThanMaxLimitParamsPassed(err));
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            if let Some(symbol_data) = ok_identifier.user_defined_type_symbol_data(
                "lambda type name should be resolved to `SymbolData<UserDefinedTypeData>`",
            ) {
                symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                    .lambda_data_mut(LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG)
                    .set_params_and_return_type(params_vec, return_type)
            }
        }
    }
}

impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match self.mode {
            ResolverMode::DECLARE => match node {
                ASTNode::VARIABLE_DECLARATION(variable_decl) => {
                    self.declare_variable(variable_decl);
                    return None;
                }
                ASTNode::OK_FUNCTION_DECLARATION(func_decl) => {
                    self.declare_function(func_decl);
                    return None;
                }
                ASTNode::STRUCT_DECLARATION(struct_decl) => {
                    self.declare_struct(struct_decl);
                    return None;
                }
                ASTNode::OK_LAMBDA_TYPE_DECLARATION(lambda_type_decl) => {
                    self.declare_lambda_type(lambda_type_decl);
                    return None;
                }
                ASTNode::ATOM_START(atom_start) => {
                    match atom_start.core_ref() {
                        CoreAtomStartNode::IDENTIFIER(identifier) => {
                            if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                                if let Some(_) = self.try_resolving_variable(ok_identifier) {
                                    let err = IdentifierNotDeclaredError::new(
                                        IdentKind::VARIABLE,
                                        ok_identifier.range(),
                                    );
                                    self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                                }
                            }
                        }
                        CoreAtomStartNode::CALL(func_call) => {
                            let core_func_call = func_call.core_ref();
                            if let CoreIdentifierNode::OK(ok_identifier) =
                                core_func_call.function_name.core_ref()
                            {
                                let lambda_name = Rc::new(ok_identifier.token_value(&self.code));
                                if let Some(symbol_data) =
                                    self.namespace.lookup_in_variables_namespace(&lambda_name)
                                {
                                    ok_identifier.bind_variable_decl(&symbol_data.0, symbol_data.1);
                                }
                            }
                            if let Some(params) = &core_func_call.params {
                                self.walk_params(params)
                            }
                        }
                        _ => {}
                    }
                    return None;
                }
                ASTNode::BLOCK(block) => {
                    self.open_block();
                    let core_block = block.0.as_ref().borrow();
                    for stmt in &core_block.stmts {
                        self.walk_stmt_indent_wrapper(stmt);
                    }
                    self.close_block();
                    return None;
                }
                _ => return Some(()),
            },
            // TODO - add here all nodes having block
            ResolverMode::RESOLVE => match node {
                ASTNode::OK_FUNCTION_DECLARATION(func_decl) => {
                    self.resolve_function(func_decl);
                    return None;
                }
                ASTNode::STRUCT_DECLARATION(struct_decl) => {
                    self.resolve_struct(struct_decl);
                    return None;
                }
                ASTNode::OK_LAMBDA_TYPE_DECLARATION(lambda_type_decl) => {
                    self.resolve_lambda_type(lambda_type_decl);
                    return None;
                }
                ASTNode::ATOM_START(atom_start) => {
                    match atom_start.core_ref() {
                        CoreAtomStartNode::CALL(func_call) => {
                            let core_func_call = func_call.core_ref();
                            if let CoreIdentifierNode::OK(ok_identifier) =
                                core_func_call.function_name.core_ref()
                            {
                                if !ok_identifier.is_resolved() {
                                    if let Some(_) = self.try_resolving_function(ok_identifier) {
                                        let err = IdentifierNotDeclaredError::new(
                                            IdentKind::FUNCTION,
                                            ok_identifier.range(),
                                        );
                                        self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                                    }
                                    // TODO - check in type namespace for constructor
                                }
                            }
                            if let Some(params) = &core_func_call.params {
                                self.walk_params(params)
                            }
                        }
                        CoreAtomStartNode::CLASS_METHOD_CALL(class_method_call) => {
                            let core_class_method_call = class_method_call.core_ref();
                            if let CoreIdentifierNode::OK(ok_identifier) =
                                core_class_method_call.class_name.core_ref()
                            {
                                if let Some(_) = self.try_resolving_user_defined_type(ok_identifier)
                                {
                                    let err = IdentifierNotDeclaredError::new(
                                        IdentKind::TYPE,
                                        ok_identifier.range(),
                                    );
                                    self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                                }
                            }
                            if let Some(params) = &core_class_method_call.params {
                                self.walk_params(params);
                            }
                        }
                        _ => {}
                    }
                    return None;
                }
                _ => return Some(()),
            },
        }
    }
}
