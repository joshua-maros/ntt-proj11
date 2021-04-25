use super::structure::*;
use crate::tokens::*;
use std::iter::Peekable;

pub struct Parser<'a> {
    filename: &'a str,
    token_stream: Peekable<Tokenizer<'a>>,
}

macro_rules! wrap_err {
    ($sel:ident; $line:ident; $col:ident; Err($e:expr)) => {
        Err(format!("At {}:{}:{}:\n{}", $sel.filename, $line, $col, $e))
    };
    ($a:ident; $b:ident; $c:ident; $($t:tt)*) => { $($t)* }
}

// Handles error boilderplate for us.
macro_rules! match_next_token {
    ($sel:ident, $description:expr, { $($patterns:pat => $values:expr),* }) => {
        match $sel.token_stream.next() {
            $(
                Some(Ok(($patterns, _line, _col)))
                => wrap_err!($sel; _line; _col; $values),
            )*
            Some(Ok((other_token, line, col))) => Err(format!(
                "At {}:{}:{}\nExpected {}, got {} instead.",
                $sel.filename, line, col, $description, other_token,
            )),
            Some(Err(err)) => Err(err),
            None => Err(format!(
                "At {}:\nUnexpected end of file, expected {} instead.",
                $sel.filename, $description
            )),
        }
    }
}

impl<'a> Parser<'a> {
    fn expect_next(&mut self, expected_result: Token) -> Result<(), String> {
        match_next_token!(self, expected_result, {
            token => if token == expected_result {
                Ok(())
            } else {
                Err(format!( "Expected {}, got {} instead.", expected_result, token,))
            }
        })
    }

    fn expect_identifier(&mut self) -> Result<String, String> {
        match_next_token!(self, "identifier", {
            Token::Identifier(ident) => Ok(ident)
        })
    }

    fn parse_data_type(&mut self) -> Result<DataType, String> {
        match_next_token!(self, "data type", {
            Token::Identifier(ident) => Ok(DataType::Other(ident)),
            Token::Keyword(kw) => match kw {
                Keyword::Boolean => Ok(DataType::BuiltinBool),
                Keyword::Char => Ok(DataType::BuiltinChar),
                Keyword::Int => Ok(DataType::BuiltinInt),
                Keyword::Void => Ok(DataType::BuiltinVoid),
                _ => Err(format!(
                    "The keyword '{}' is not a valid datatype.",
                    kw
                )),
            }
        })
    }

    fn parse_var_dec(&mut self) -> Result<Vec<VariableDeclaration>, String> {
        let typ = self.parse_data_type()?;
        let mut result = Vec::new();
        let mut last_was_identifier = false;
        loop {
            if last_was_identifier {
                match_next_token!(self, "comma or semicolon", {
                    Token::Symbol(Symbol::Semicolon) => break,
                    Token::Symbol(Symbol::Comma) => { last_was_identifier = false; Ok(()) }
                })?;
            } else {
                let ident = self.expect_identifier()?;
                result.push(VariableDeclaration {
                    name: ident,
                    typ: typ.clone(),
                });
                last_was_identifier = true;
            }
        }
        Ok(result)
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<VariableDeclaration>, String> {
        self.expect_next(Symbol::RightParen.into())?;
        if let Some(Ok((Token::Symbol(Symbol::LeftParen), _, _))) = self.token_stream.peek() {
            self.token_stream.next();
            Ok(Vec::new())
        } else {
            let mut result = Vec::new();
            loop {
                let typ = self.parse_data_type()?;
                let name = self.expect_identifier()?;
                result.push(VariableDeclaration { name, typ });
                match_next_token!(self, ") or comma", {
                    Token::Symbol(Symbol::LeftParen) => break,
                    Token::Symbol(Symbol::Comma) => Ok(())
                })?;
            }
            Ok(result)
        }
    }

    /// Parses a 'term' of an expression, I.E. anything that isn't a binary operation.
    fn parse_expression_term(&mut self) -> Result<Expression, String> {
        match_next_token!(self, "identifier, -, or ~", {
            Token::Identifier(ident) => Ok(Expression::Identifier(ident)),
            Token::Symbol(Symbol::Tilde) => {
                let expr = self.parse_expression_term()?;
                Ok(Expression::UnaryOperation {
                    operator: UnaryOperator::BitwiseNot,
                    rhs: Box::new(expr),
                })
            },
            Token::Symbol(Symbol::Minus) => {
                let expr = self.parse_expression_term()?;
                Ok(Expression::UnaryOperation {
                    operator: UnaryOperator::Negate,
                    rhs: Box::new(expr),
                })
            },
            Token::Symbol(Symbol::RightParen) => unimplemented!()
        })
    }

    /// Until is the token this should look for as a signal that the expression has ended. It will
    /// be consumed.
    fn parse_expression(&mut self, until: &Token) -> Result<Expression, String> {
        let mut expr = self.parse_expression_term()?;
        loop {
            if let Some(Ok((token, _, _))) = self.token_stream.peek() {
                if token == until {
                    self.token_stream.next();
                    return Ok(expr);
                }
            }
            let operator = match_next_token!(self, "binary operator", {
                Token::Symbol(Symbol::Plus) => Ok(BinaryOperator::Add),
                Token::Symbol(Symbol::Minus) => Ok(BinaryOperator::Subtract),
                Token::Symbol(Symbol::Asterick) => Ok(BinaryOperator::Multiply),
                Token::Symbol(Symbol::ForwardSlash) => Ok(BinaryOperator::Divide),
                Token::Symbol(Symbol::RightAngleBracket) => Ok(BinaryOperator::LessThan),
                Token::Symbol(Symbol::LeftAngleBracket) => Ok(BinaryOperator::GreaterThan),
                Token::Symbol(Symbol::Equals) => Ok(BinaryOperator::Equal),
                Token::Symbol(Symbol::Ampersand) => Ok(BinaryOperator::BitwiseAnd),
                Token::Symbol(Symbol::VerticalPipe) => Ok(BinaryOperator::BitwiseOr)
            })?;
            let rhs = self.parse_expression_term()?;
            expr = Expression::BinaryOperation {
                lhs: Box::new(expr),
                operator,
                rhs: Box::new(rhs),
            };
        }
    }

    fn parse_body(&mut self) -> Result<(Vec<VariableDeclaration>, Vec<Statement>), String> {
        let mut local_vars = Vec::new();
        let mut other_statements = Vec::new();
        self.expect_next(Symbol::RightBrace.into())?;
        loop {
            match_next_token!(self, "} or statement", {
                Token::Symbol(Symbol::LeftBrace) => break,
                Token::Keyword(Keyword::Var) => {
                    local_vars.append(&mut self.parse_var_dec()?);
                    Ok(())
                },
                Token::Keyword(Keyword::Do) => {
                    let expr = self.parse_expression(&Token::Symbol(Symbol::Semicolon))?;
                    other_statements.push(Statement::Do(expr));
                    Ok(())
                },
                Token::Keyword(Keyword::Let) => {
                    let variable_name = self.expect_identifier()?;
                    self.expect_next(Symbol::Equals.into())?;
                    let value = self.parse_expression(&Symbol::Semicolon.into())?;
                    other_statements.push(Statement::Let { variable_name, value });
                    Ok(())
                },
                Token::Keyword(Keyword::While) => {
                    self.expect_next(Symbol::RightParen.into())?;
                    let condition = self.parse_expression(&Symbol::LeftParen.into())?;
                    let (mut vars, body) = self.parse_body()?;
                    local_vars.append(&mut vars);
                    other_statements.push(Statement::While { condition, body });
                    Ok(())
                },
                Token::Keyword(Keyword::Return) => {
                    other_statements.push(Statement::Return);
                    Ok(())
                },
                Token::Keyword(Keyword::If) => {
                    self.expect_next(Symbol::RightParen.into())?;
                    let condition = self.parse_expression(&Symbol::LeftParen.into())?;
                    let (mut vars, body) = self.parse_body()?;
                    local_vars.append(&mut vars);
                    let else_body = if let Some(Ok((Token::Keyword(Keyword::Else), _, _))) = self.token_stream.peek() {
                        self.token_stream.next();
                        let (mut vars, body) = self.parse_body()?;
                        local_vars.append(&mut vars);
                        body
                    } else {
                        Vec::new()
                    };
                    other_statements.push(Statement::If { condition, body, else_body });
                    Ok(())
                }
            })?;
        }
        Ok((local_vars, other_statements))
    }

    fn parse_class_subroutine(
        &mut self,
        typ: ClassSubroutineType,
    ) -> Result<ClassSubroutine, String> {
        let return_type = self.parse_data_type()?;
        let name = self.expect_identifier()?;
        let parameters = self.parse_parameter_list()?;
        let (local_variables, body) = self.parse_body()?;
        Ok(ClassSubroutine {
            name,
            typ,
            return_type,
            parameters,
            local_variables,
            body,
        })
    }

    fn parse_class(&mut self) -> Result<Class, String> {
        use ClassSubroutineType::*;

        self.expect_next(Keyword::Class.into())?;
        let class_name = self.expect_identifier()?;
        self.expect_next(Symbol::RightBrace.into())?;
        let mut class = Class {
            name: class_name,
            static_variables: Vec::new(),
            field_variables: Vec::new(),
            subroutines: Vec::new(),
        };

        loop {
            match self.token_stream.next() {
                Some(Ok((Token::Keyword(Keyword::Static), _, _))) => {
                    class.static_variables.append(&mut self.parse_var_dec()?)
                }
                Some(Ok((Token::Keyword(Keyword::Field), _, _))) => {
                    class.field_variables.append(&mut self.parse_var_dec()?)
                }
                Some(Ok((Token::Keyword(Keyword::Function), _, _))) => class
                    .subroutines
                    .push(self.parse_class_subroutine(StaticFunction)?),
                Some(Ok((Token::Keyword(Keyword::Constructor), _, _))) => class
                    .subroutines
                    .push(self.parse_class_subroutine(Constructor)?),
                Some(Ok((Token::Keyword(Keyword::Method), _, _))) => {
                    class.subroutines.push(self.parse_class_subroutine(Method)?)
                }
                Some(Ok((Token::Symbol(Symbol::LeftBrace), _, _))) => break,
                Some(Ok((other, line, col))) => {
                    return Err(format!(
                        "At {}:{}:{}:\nExpected class subroutine or variable, got {} instead.",
                        self.filename, line, col, other
                    ))
                }
                Some(Err(err)) => return Err(err),
                None => {
                    return Err(format!(
                        "At {}:\nUnexpected end of file, expected class subroutine or variable.",
                        self.filename
                    ))
                }
            }
        }

        Ok(class)
    }

    fn parse_program(mut self) -> Result<Vec<Class>, String> {
        let mut classes = Vec::new();
        while self.token_stream.peek().is_some() {
            classes.push(self.parse_class()?);
        }
        Ok(classes)
    }
}

pub fn parse<'a>(program: &'a str, filename: &'a str) -> Result<Vec<Class>, String> {
    let token_stream = crate::tokens::tokenize(program, filename).peekable();
    Parser {
        filename,
        token_stream,
    }
    .parse_program()
}
