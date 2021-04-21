pub struct Program {
    pub classes: Vec<Class>,
}

pub struct Class {
    variables: Vec<ClassVariable>,
    
}

pub struct ClassVariable {
    name: String,
    /// False if this variable is a per-instance field.
    is_static: bool,
}
