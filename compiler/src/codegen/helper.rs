use super::python::PythonCodeGenerator;
use crate::ast::ast::{
    AtomNode, CoreAtomNode, CoreAtomStartNode, CoreAtomicExpressionNode, CoreExpressionNode,
    CoreUnaryExpressionNode, ExpressionNode,
};
use crate::ast::walk::Visitor;

impl PythonCodeGenerator {
    pub fn print_atom_node_without_trivia(&mut self, atom: &AtomNode) {
        let core_atom = atom.core_ref();
        match core_atom {
            CoreAtomNode::AtomStart(atom_start) => {
                let core_atom_start = atom_start.core_ref();
                match core_atom_start {
                    CoreAtomStartNode::Identifier(token) => {
                        self.print_identifier_in_use(token, false);
                    }
                    CoreAtomStartNode::SelfKeyword(_) => {
                        self.add_str_to_python_code("self");
                    }
                    CoreAtomStartNode::Call(call_expr) => {
                        let core_call_expr = call_expr.core_ref();
                        self.print_identifier_in_use(&core_call_expr.function_name, false);
                        self.walk_token(&core_call_expr.lparen);
                        if let Some(params) = &core_call_expr.params {
                            self.walk_comma_separated_expressions(params);
                        }
                        self.walk_token(&core_call_expr.rparen);
                    }
                    CoreAtomStartNode::EnumVariantExprOrClassMethodCall(
                        enum_variant_or_class_method,
                    ) => self.print_enum_variant_expr_or_class_method_call(
                        enum_variant_or_class_method,
                        false,
                    ),
                }
            }
            CoreAtomNode::Call(call_node) => {
                let core_call = call_node.core_ref();
                self.print_atom_node_without_trivia(&core_call.atom);
                self.walk_token(&core_call.lparen);
                if let Some(params) = &core_call.params {
                    self.walk_comma_separated_expressions(params);
                }
                self.walk_token(&core_call.rparen);
            }
            CoreAtomNode::PropertyAccess(property_access) => {
                let core_property_access = property_access.core_ref();
                self.print_atom_node_without_trivia(&core_property_access.atom);
                self.walk_token(&core_property_access.dot);
                self.walk_identifier_in_use(&core_property_access.propertry);
            }
            CoreAtomNode::MethodAccess(method_access) => {
                let core_method_access = method_access.core_ref();
                self.print_atom_node_without_trivia(&core_method_access.atom);
                self.walk_token(&core_method_access.dot);
                self.walk_identifier_in_use(&core_method_access.method_name);
                self.walk_token(&core_method_access.lparen);
                if let Some(params) = &core_method_access.params {
                    self.walk_comma_separated_expressions(params);
                }
                self.walk_token(&core_method_access.rparen);
            }
            CoreAtomNode::IndexAccess(index_access) => {
                let core_index_access = index_access.core_ref();
                self.print_atom_node_without_trivia(&core_index_access.atom);
                self.walk_token(&core_index_access.lsquare);
                self.walk_expression(&core_index_access.index);
                self.walk_token(&core_index_access.rsquare);
            }
        }
    }

    pub fn print_expression_without_trivia(&mut self, expr: &ExpressionNode) {
        let core_expr = expr.core_ref();
        match core_expr {
            CoreExpressionNode::Unary(unary_expr) => {
                let core_unary_expr = unary_expr.core_ref();
                match core_unary_expr {
                    CoreUnaryExpressionNode::Atomic(atomic_expr) => {
                        let core_atomic_expr = atomic_expr.core_ref();
                        match core_atomic_expr {
                            CoreAtomicExpressionNode::Bool(token) => {
                                self.print_token_node_without_trivia(token);
                            }
                            CoreAtomicExpressionNode::Integer(token) => {
                                self.print_token_node_without_trivia(token);
                            }
                            CoreAtomicExpressionNode::FloatingPointNumber(token) => {
                                self.print_token_node_without_trivia(token);
                            }
                            CoreAtomicExpressionNode::Literal(token) => {
                                self.print_token_node_without_trivia(token);
                            }
                            CoreAtomicExpressionNode::ParenthesisedExpression(
                                parenthesised_expr,
                            ) => {
                                let core_parenthesised_expr = parenthesised_expr.core_ref();
                                self.print_token_node_without_trivia(
                                    &core_parenthesised_expr.lparen,
                                );
                                self.walk_expression(&core_parenthesised_expr.expr);
                                self.walk_token(&core_parenthesised_expr.rparen);
                            }
                            CoreAtomicExpressionNode::Atom(atom) => {
                                self.print_atom_node_without_trivia(atom);
                            }
                            CoreAtomicExpressionNode::ArrayExpression(array_expr) => {
                                let core_array_expr = array_expr.core_ref();
                                self.print_token_node_without_trivia(&core_array_expr.lsquare);
                                if let Some(initials) = &core_array_expr.initials {
                                    self.walk_comma_separated_expressions(initials);
                                }
                                self.walk_token(&core_array_expr.rsquare);
                            }
                            CoreAtomicExpressionNode::HashMapExpression(hashmap_expr) => {
                                let core_hashmap_expr = hashmap_expr.core_ref();
                                self.print_token_node_without_trivia(&core_hashmap_expr.lcurly);
                                if let Some(initials) = &core_hashmap_expr.initials {
                                    self.walk_comma_separated_key_value_pairs(initials);
                                }
                                self.walk_token(&core_hashmap_expr.rcurly);
                            }
                            CoreAtomicExpressionNode::TupleExpression(tuple_expr) => {
                                let core_tuple_expr = tuple_expr.core_ref();
                                self.print_token_node_without_trivia(&core_tuple_expr.lround);
                                self.walk_comma_separated_expressions(&core_tuple_expr.initials);
                                self.walk_token(&core_tuple_expr.rround);
                            }
                            CoreAtomicExpressionNode::MissingTokens(_) => {
                                unreachable!()
                            }
                        }
                    }
                    CoreUnaryExpressionNode::Unary(only_unary_expr) => {
                        let core_only_unary_expr = only_unary_expr.core_ref();
                        self.print_token_node_without_trivia(&core_only_unary_expr.operator);
                        self.walk_unary_expression(&core_only_unary_expr.unary_expr);
                    }
                }
            }
            CoreExpressionNode::Binary(binary_expr) => {
                let core_binary_expr = binary_expr.core_ref();
                self.print_expression_without_trivia(&core_binary_expr.left_expr);
                self.walk_token(&core_binary_expr.operator);
                self.walk_expression(&core_binary_expr.right_expr);
            }
            CoreExpressionNode::Comparison(comp_expr) => {
                let core_comp_expr = comp_expr.core_ref();
                let operator_len = core_comp_expr.operators.len();
                self.print_expression_without_trivia(&core_comp_expr.operands[0]);
                for i in 0..operator_len {
                    self.walk_token(&core_comp_expr.operators[i]);
                    self.walk_expression(&core_comp_expr.operands[i + 1]);
                }
            }
        }
    }
}
