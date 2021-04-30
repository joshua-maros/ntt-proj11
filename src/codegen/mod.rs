use crate::syntax_tree::{
    AssignmentExpression, BinaryOperator, Class, ClassSubroutine, Constructor, Expression, Method,
    Statement, UnaryOperator,
};
use std::{collections::HashMap, fmt::Write};

enum SymbolTableEntry {
    Static { index: usize },
    Field { index: usize },
}

enum SubroutineSymbol {
    Argument { index: usize },
    Local { index: usize },
}

struct DataTypeName(String);

struct VarInfo {
    data_type: String,
    vm_address: String,
}

struct CodeGenerator {
    label_counter: usize,
    class_name: String,
    output: String,
    symbol_table: HashMap<String, (SymbolTableEntry, String)>,
    subroutine_symbol_table: HashMap<String, (SubroutineSymbol, String)>,
}

impl CodeGenerator {
    fn var_info(&self, var_name: &str) -> Result<VarInfo, String> {
        if let Some((entry, data_type)) = self.symbol_table.get(var_name) {
            let vm_address = match entry {
                SymbolTableEntry::Field { index } => format!("this {}", index),
                SymbolTableEntry::Static { index } => format!("static {}", index),
            };
            let data_type = data_type.clone();
            Ok(VarInfo {
                data_type,
                vm_address,
            })
        } else if let Some((entry, data_type)) = self.subroutine_symbol_table.get(var_name) {
            let vm_address = match entry {
                SubroutineSymbol::Argument { index } => format!("argument {}", index),
                SubroutineSymbol::Local { index } => format!("local {}", index),
            };
            let data_type = data_type.clone();
            Ok(VarInfo {
                data_type,
                vm_address,
            })
        } else {
            Err(format!(
                concat!(
                    "The variable '{}' either does not exist or ",
                    "was defined in another class or function.",
                ),
                var_name
            ))
        }
    }

    /// Returns a string indicating the internal label of the function that the expression
    /// represents. If the function is being called like a method, this function also generates
    /// code to initialize the 'this' pointer.
    fn generate_call_source(&mut self, expr: &Expression) -> Result<(String, bool), String> {
        match expr {
            Expression::Identifier(ident) => {
                self.output.push_str("push pointer 0\n");
                Ok((format!("{}.{}", self.class_name, ident), true))
            }
            Expression::PropertyAccess {
                base,
                property_name,
            } => {
                if let Expression::Identifier(ident) = &**base {
                    if !self.symbol_table.contains_key(ident)
                        && !self.subroutine_symbol_table.contains_key(ident)
                    {
                        // This is a class name defined in an external file.
                        return Ok((format!("{}.{}", ident, property_name), false));
                    }
                }
                let base_type = self.push_expression(base)?.0;
                Ok((format!("{}.{}", base_type, property_name), true))
            }
            other => Err(format!("{:?} cannot be called.", other)),
        }
    }

    /// Generates code to execute the given expression and leave its resulting value as the value
    /// on the top of the stack.
    fn push_expression(&mut self, expr: &Expression) -> Result<DataTypeName, String> {
        macro_rules! put {
            ($($args:tt)*) => { write!(&mut self.output, $($args)*).unwrap(); }
        }
        match expr {
            Expression::ArrayAccess { base, index } => {
                self.push_expression(base)?;
                self.push_expression(index)?;
                put!("add\n");
                put!("pop pointer 1\n");
                put!("push that 0\n");
                Ok(DataTypeName(format!("int")))
            }
            Expression::BinaryOperation { lhs, operator, rhs } => {
                self.push_expression(lhs)?;
                self.push_expression(rhs)?;
                use BinaryOperator::*;
                let code = match operator {
                    BitwiseAnd => "and",
                    BitwiseOr => "or",
                    Add => "add",
                    Subtract => "sub",
                    Multiply => "call Math.multiply 2",
                    Divide => "call Math.divide 2",
                    LessThan => "lt",
                    GreaterThan => "gt",
                    Equal => "eq",
                };
                put!("{}\n", code);
                Ok(DataTypeName(format!("int")))
            }
            Expression::BooleanConstant(value) => {
                let value = if *value { "1" } else { "0" };
                put!("push constant {}\n", value);
                Ok(DataTypeName(format!("bool")))
            }
            Expression::Identifier(ident) => {
                let info = self.var_info(ident)?;
                put!("push {}\n", info.vm_address);
                Ok(DataTypeName(info.data_type))
            }
            Expression::IntegerConstant(value) => {
                put!("push constant {}\n", value);
                Ok(DataTypeName(format!("int")))
            }
            Expression::Null => {
                put!("push constant 0\n");
                Ok(DataTypeName(format!("int")))
            }
            Expression::PropertyAccess {
                base,
                property_name,
            } => {
                unimplemented!()
            }
            Expression::StringConstant(value) => {
                // Make new string.
                put!("push constant {}\n", value.len());
                put!("call String.new 1\n");
                put!("pop temp 1\n");
                // Push characters.
                for c in value.chars() {
                    if !c.is_ascii() {
                        unimplemented!("Unicode strings are not supported.");
                    }
                    put!("push temp 1\n");
                    put!("push constant {}\n", c as u8);
                    put!("call String.appendChar 2\n");
                    put!("pop temp 0\n");
                }
                put!("push temp 1\n");
                Ok(DataTypeName(format!("String")))
            }
            Expression::SubroutineCall { subroutine, args } => {
                let (call_label, has_this_argument) = self.generate_call_source(subroutine)?;
                for arg in args {
                    self.push_expression(arg)?;
                }
                put!(
                    "call {} {}\n",
                    call_label,
                    args.len() + if has_this_argument { 1 } else { 0 }
                );
                Ok(DataTypeName(format!("int")))
            }
            Expression::This => {
                put!("push pointer 0\n");
                Ok(DataTypeName(self.class_name.clone()))
            }
            Expression::UnaryOperation { operator, rhs } => {
                self.push_expression(rhs)?;
                use UnaryOperator::*;
                let code = match operator {
                    BitwiseNot => "not",
                    Negate => "neg",
                };
                put!("{}\n", code);
                Ok(DataTypeName(format!("int")))
            }
        }
    }

    /// Generates code to pop the top of the stack into the provided assignment expression.
    fn pop_into(&mut self, target: &AssignmentExpression) -> Result<(), String> {
        // This has to be redefined each time because 'self' refers to variables in different
        // functions.
        macro_rules! put {
            ($($args:tt)*) => { write!(&mut self.output, $($args)*).unwrap(); }
        }
        put!("// Pop into\n");
        match target {
            AssignmentExpression::ArrayAccess { base, index } => {
                if let AssignmentExpression::Identifier(base_ident) = &**base {
                    let base_info = self.var_info(base_ident)?;
                    // Load the value of the base variable.
                    put!("push {}\n", base_info.vm_address);
                    // Add it to the index.
                    self.push_expression(index)?;
                    put!("add\n");
                    // Setup 'that' pointer.
                    put!("pop pointer 1\n");
                    // Assign the value.
                    put!("pop that 0\n");
                    Ok(())
                } else {
                    unimplemented!()
                }
            }
            AssignmentExpression::Identifier(ident) => {
                let addr = self.var_info(ident)?.vm_address;
                put!("pop {}\n", addr);
                Ok(())
            }
            AssignmentExpression::PropertyAccess {
                base,
                property_name,
            } => unimplemented!(),
        }
    }

    fn generate_body(&mut self, body: &[Statement]) -> Result<(), String> {
        // This has to be redefined each time because 'self' refers to variables in different
        // functions.
        macro_rules! put {
            ($($args:tt)*) => { write!(&mut self.output, $($args)*).unwrap(); }
        }
        for statement in body {
            match statement {
                Statement::Do(expression) => {
                    self.push_expression(expression)?;
                    put!("pop temp 0\n");
                }
                Statement::If {
                    condition,
                    body,
                    else_body,
                } => {
                    self.label_counter += 1;
                    let label_id = self.label_counter;
                    self.push_expression(condition)?;
                    put!("if-goto IF_{}\n", label_id);
                    put!("goto IF_{}_ELSE\n", label_id);
                    put!("label IF_{}\n", label_id);
                    self.generate_body(body)?;
                    put!("goto IF_{}_END\n", label_id);
                    put!("label IF_{}_ELSE\n", label_id);
                    self.generate_body(else_body)?;
                    put!("label IF_{}_END\n", label_id);
                }
                Statement::Let { target, value } => {
                    self.push_expression(value)?;
                    self.pop_into(target)?;
                }
                Statement::Return(value) => {
                    if let Some(value) = value {
                        self.push_expression(value)?;
                    } else {
                        put!("push constant 0\n");
                    }
                    put!("return\n");
                }
                Statement::While { condition, body } => {
                    let label_id = self.label_counter;
                    self.label_counter += 1;
                    put!("label WHILE_{}_REPEAT\n", label_id);
                    self.push_expression(condition)?;
                    put!("if-goto WHILE_{}_CONTINUE\n", label_id);
                    put!("goto WHILE_{}_END\n", label_id);
                    put!("label WHILE_{}_CONTINUE\n", label_id);
                    self.generate_body(body)?;
                    put!("goto WHILE_{}_REPEAT\n", label_id);
                    put!("label WHILE_{}_END\n", label_id);
                }
            }
            put!("// -------------\n");
        }
        Ok(())
    }

    fn generate_subroutine(
        &mut self,
        class: &Class,
        subroutine: &ClassSubroutine,
    ) -> Result<(), String> {
        macro_rules! put {
            ($($args:tt)*) => { write!(&mut self.output, $($args)*).unwrap(); }
        }
        // Generate header like `function Class.dostuff1`
        put!(
            "// Begin subroutine: {:?} {}(",
            subroutine.typ,
            subroutine.name
        );
        for params in &subroutine.parameters {
            put!("{} {}, ", params.typ.name(), params.name);
        }
        put!(")\n");
        put!("function {}.{}", class.name, subroutine.name);

        put!(" {}\n", subroutine.local_variables.len());
        self.subroutine_symbol_table.clear();
        for (index, parameter) in subroutine.parameters.iter().enumerate() {
            let index = if subroutine.typ == Method {
                index + 1
            } else {
                index
            };
            let entry = SubroutineSymbol::Argument { index };
            let data_type = parameter.typ.name().to_owned();
            self.subroutine_symbol_table
                .insert(parameter.name.clone(), (entry, data_type));
        }
        for (index, local_var) in subroutine.local_variables.iter().enumerate() {
            let entry = SubroutineSymbol::Local { index };
            let data_type = local_var.typ.name().to_owned();
            self.subroutine_symbol_table
                .insert(local_var.name.clone(), (entry, data_type));
        }

        if subroutine.typ == Constructor {
            // Setup this pointer.
            put!("push constant {}\n", class.field_variables.len());
            put!("call Memory.alloc 1\n");
            put!("pop pointer 0\n");
        } else if subroutine.typ == Method {
            put!("push argument 0\n");
            put!("pop pointer 0\n");
        }

        self.generate_body(&subroutine.body[..])?;

        put!("push constant 0\nreturn\n");
        put!("// End subroutine\n\n");
        Ok(())
    }

    fn generate(&mut self, class: &Class) -> Result<(), String> {
        for (index, static_var) in class.static_variables.iter().enumerate() {
            let entry = SymbolTableEntry::Static { index };
            let data_type = static_var.typ.name().to_owned();
            self.symbol_table
                .insert(static_var.name.clone(), (entry, data_type));
        }
        for (index, field_var) in class.field_variables.iter().enumerate() {
            let entry = SymbolTableEntry::Field { index };
            let data_type = field_var.typ.name().to_owned();
            self.symbol_table
                .insert(field_var.name.clone(), (entry, data_type));
        }

        for sub in &class.subroutines {
            self.generate_subroutine(class, sub)?;
        }

        Ok(())
    }
}

pub fn generate_code(for_class: &Class) -> Result<String, String> {
    let mut generator = CodeGenerator {
        label_counter: 0,
        class_name: for_class.name.clone(),
        output: String::new(),
        symbol_table: HashMap::new(),
        subroutine_symbol_table: HashMap::new(),
    };
    generator.generate(for_class)?;
    Ok(generator.output)
}
