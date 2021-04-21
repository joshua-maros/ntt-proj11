use std::iter::Peekable;

use super::structure::*;
use crate::tokens::*;

pub struct Parser<'a> {
    filename: &'a str,
    token_stream: Peekable<Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    fn expect_next(&mut self, expected_result: Token) -> Result<(), String> {
        match self.token_stream.next() {
            Some(Ok((token, line, col))) => {
                if token == expected_result {
                    Ok(())
                } else {
                    Err(format!(
                        "At {}:{}:{}:\nExpected {}, got {} instead.",
                        self.filename, line, col, expected_result, token,
                    ))
                }
            }
            Some(Err(err)) => Err(err),
            None => Err(format!(
                "At {}:\nUnexpected end of file, expected {} instead.",
                self.filename, expected_result,
            )),
        }
    }

    fn expect_identifier(&mut self) -> Result<String, String> {
        match self.token_stream.next() {
            Some(Ok((Token::Identifier(ident), _, _))) => Ok(ident),
            Some(Ok((other_token, line, col))) => Err(format!(
                "At {}:{}:{}\nExpected identifier, got {} instead.",
                self.filename, line, col, other_token,
            )),
            Some(Err(err)) => Err(err),
            None => Err(format!(
                "At {}:\nUnexpected end of file, expected identifier instead.",
                self.filename,
            )),
        }
    }

    fn parse_data_type(&mut self) -> Result<DataType, String> {
        unimplemented!()
    }

    fn parse_var_dec(&mut self) -> Result<Vec<VariableDeclaration>, String> {
        let typ = self.parse_data_type()?;
        let mut result = Vec::new();
        let mut last_was_identifier = false;
        loop {
            if last_was_identifier {
                match self.token_stream.next() {
                    Some(Ok((Token::Symbol(Symbol::Semicolon), _, _))) => break,
                    Some(Ok((Token::Symbol(Symbol::Comma), _, _))) => last_was_identifier = false,
                    Some(Ok((other, line, col))) => {
                        return Err(format!(
                            "At {}:{}:{}:\nExpected comma or semicolon, got {} instead.",
                            self.filename, line, col, other
                        ))
                    }
                    Some(Err(err)) => return Err(err),
                    None => {
                        return Err(format!(
                            "At {}:\nUnexpected end of file, expected comma or semicolon instead.",
                            self.filename
                        ))
                    }
                }
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

    fn parse_class_subroutine(
        &mut self,
        typ: ClassSubroutineType,
    ) -> Result<ClassSubroutine, String> {
        use ClassSubroutineType::*;
        Ok(ClassSubroutine {
            typ,
            ..unimplemented!()
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
