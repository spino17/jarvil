use crate::types::Type;

trait Node {
    fn eval(&self);
}

pub enum ASTNode {
    STATEMENT,
    VARIABLE_DECLARATION,
    ASSIGNMENT,
    FUNCTION_DECLARATION,
    USER_DEFINED_TYPE_DECLARATION,
    BLOCK,
    STRUCT_BLOCK,
    IMPL_FOR_STRUCT_BLOCK,
    EXPR, 
    BEXPR,
    ATOM,
    IDENTIFIER,
    INDEX_ACCESS,
    PROPERTRY_ACCESS,
    METHOD_ACCESS,
    CLASS_METHOD_ACCESS,
    LITERAL
}

pub struct StatementNode {

}

pub struct VariableDeclarationNode {
    left_side: Box<IdentifierNode>,
    right_side: Box<RAssignmentNode>,
}

impl Node for VariableDeclarationNode {
    fn eval(&self) {
        self.left_side.eval();
        self.right_side.eval();
    }
}

pub struct AssignmentNode {

}

pub struct FunctionDeclarationNode {

}

pub struct UserDefinedTypeDeclarationNode {

}

pub struct IdentifierNode {
    data_type: Type,
}

impl Node for IdentifierNode {
    fn eval(&self) {
        todo!()
    }
}

pub struct RAssignmentNode {
    
}

impl Node for RAssignmentNode {
    fn eval(&self) {
        todo!()
    }
}

pub struct BlockNode {

}

pub struct StructBlockNode {

}

pub struct ImplForStructBlockNode {

}

pub struct ExprNode {

}

pub struct BExprNode {

}

pub struct Atom {
    data_type: Type,
}

pub struct IndexAccessNode {

}

pub struct PropertryAccessNode {

}

pub struct MethodAccessNode {

}

pub struct ClassMethodAccessNode {

}

pub struct LiteralNode {

}