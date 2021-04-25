#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Program {
    pub classes: Vec<Class>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Class {
    pub name: String,
    pub static_variables: Vec<VariableDeclaration>,
    pub field_variables: Vec<VariableDeclaration>,
    pub subroutines: Vec<ClassSubroutine>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct VariableDeclaration {
    pub name: String,
    pub typ: DataType,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataType {
    BuiltinInt,
    BuiltinChar,
    BuiltinBool,
    BuiltinVoid,
    /// Used for class names as data types.
    Other(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Statement {
    Do(Expression),
    Let {
        target: AssignmentExpression,
        value: Expression,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    Return(Option<Expression>),
    If {
        condition: Expression,
        body: Vec<Statement>,
        else_body: Vec<Statement>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Identifier(String),
    This,
    Null,
    BooleanConstant(bool),
    IntegerConstant(i32),
    StringConstant(String),
    ArrayAccess {
        base: Box<Expression>,
        index: Box<Expression>,
    },
    PropertyAccess {
        base: Box<Expression>,
        property_name: String,
    },
    UnaryOperation {
        operator: UnaryOperator,
        rhs: Box<Expression>,
    },
    BinaryOperation {
        lhs: Box<Expression>,
        operator: BinaryOperator,
        rhs: Box<Expression>,
    },
    SubroutineCall {
        subroutine: Box<Expression>,
        args: Vec<Expression>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AssignmentExpression {
    Identifier(String),
    ArrayAccess {
        base: Box<AssignmentExpression>,
        index: Box<Expression>,
    },
    PropertyAccess {
        base: Box<AssignmentExpression>,
        property_name: String,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Negate,
    BitwiseNot,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Equal,
    BitwiseAnd,
    BitwiseOr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ClassSubroutineType {
    StaticFunction,
    Method,
    Constructor,
}
pub use ClassSubroutineType::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassSubroutine {
    pub name: String,
    // 'type' is a rust keyword.
    pub typ: ClassSubroutineType,
    pub return_type: DataType,
    pub parameters: Vec<VariableDeclaration>,
    pub local_variables: Vec<VariableDeclaration>,
    pub body: Vec<Statement>,
}
