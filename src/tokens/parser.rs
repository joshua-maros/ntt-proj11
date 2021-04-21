use super::{Keyword, Symbol, Token, TokenizedProgram};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum TokenizerState {
    /// Used when we are traversing through whitespace.
    LookingForToken,
    /// Used when we are inside double quotes.
    StringLiteral,
    IntegerLiteral,
    KeywordOrIdentifier,
    LineComment,
    BlockComment,
    /// Used when BlockComment sees a * that might end the block.
    BlockComment2,
}

struct Tokenizer {
    tokens: Vec<Token>,
    // Stores the character that will be parsed next, if there is one.
    peek_next: Option<char>,
    state: TokenizerState,
    /// Used when reading keywords and identifiers.
    buffer: String,
}

impl Tokenizer {
    fn finish_integer_literal(&mut self) -> Result<(), String> {
        let literal_text = std::mem::take(&mut self.buffer);
        // Try to parse the value and format a pretty error if it cannot be parsed.
        // (Can only have an error if the value is bigger than an i32 which is rather
        // unlikely but still better safe than sorry.)
        let value: i32 = literal_text.parse().map_err(|err| {
            format!(
                "Failed to parse integer literal '{}': {}",
                literal_text, err
            )
        })?;
        self.tokens.push(Token::IntegerConstant(value));
        Ok(())
    }

    fn finish_keyword_or_identifier(&mut self) {
        let token = if let Ok(keyword) = Keyword::from_text(&self.buffer[..]) {
            // Clear the buffer and use the detected keyword.
            self.buffer.clear();
            Token::Keyword(keyword)
        } else {
            // Take the identifier out of the buffer, leaving behind an empty string.
            Token::Identifier(std::mem::take(&mut self.buffer))
        };
        self.tokens.push(token);
        // Go back to looking for tokens.
        self.state = TokenizerState::LookingForToken;
    }

    fn process_char(&mut self, c: char) -> Result<(), String> {
        use TokenizerState::*;
        match self.state {
            LookingForToken => match c {
                ' ' | '\t' | '\r' | '\n' => (),
                '0'..='9' => {
                    // Reprocess this character as if we are in a keyword or identifier.
                    self.state = IntegerLiteral;
                    return self.process_char(c);
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    // Reprocess this character as if we are in a keyword or identifier.
                    self.state = KeywordOrIdentifier;
                    return self.process_char(c);
                }
                '"' => {
                    self.state = StringLiteral;
                    // Don't reprocess this character because it should not be included in the
                    // contents of the string literal.
                }
                '/' => {
                    // Check if this is just a division operator or if we are starting a comment.
                    match self.peek_next {
                        Some('*') => self.state = BlockComment,
                        Some('/') => self.state = LineComment,
                        _ => {
                            // It is just a symbol and not a comment, so push the appropriate token.
                            self.tokens.push(Token::Symbol(Symbol::ForwardSlash));
                        }
                    }
                }
                _ => {
                    if let Ok(symbol) = Symbol::from_text(&c.to_string()) {
                        self.tokens.push(Token::Symbol(symbol))
                    } else {
                        return Err(format!("unexpected character '{}'", c));
                    }
                }
            },
            StringLiteral => match c {
                '\n' => return Err(format!("unexpected newline in string literal")),
                '"' => {
                    // Take the string content out of the buffer, leaving behind an empty string.
                    let buffer_contents = std::mem::take(&mut self.buffer);
                    self.tokens.push(Token::StringConstant(buffer_contents));
                    self.state = LookingForToken;
                }
                _ => {
                    self.buffer.push(c);
                }
            },
            IntegerLiteral => match c {
                '0'..='9' => self.buffer.push(c),
                // It is easier to fail on letters and suceed by default than to suceed on symbols
                // and fail by default.
                'a'..='z' | 'A'..='Z' => {
                    return Err(format!("unexpected character '{}' in integer literal", c))
                }
                _ => {
                    // If we encounter some other character, that means the code is moving on to
                    // something else. So we convert the chars we have buffered so far into a token.
                    self.finish_integer_literal()?;
                    // Then we reprocess the char we just took as if we are looking for another
                    // token.
                    self.state = LookingForToken;
                    return self.process_char(c);
                }
            },
            KeywordOrIdentifier => match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    self.buffer.push(c);
                }
                _ => {
                    // If we encounter some other character, that means the code is moving on to
                    // something else. So we convert the chars we have buffered so far into a token.
                    self.finish_keyword_or_identifier();
                    // Then we reprocess the char we just took as if we are looking for another
                    // token.
                    self.state = LookingForToken;
                    return self.process_char(c);
                }
            },
            LineComment => match c {
                // If there is a newline, it indicates the end of the line comment.
                '\n' => self.state = LookingForToken,
                // Otherwise, the character is part of the comment and should be disregarded.
                _ => (),
            },
            BlockComment => match c {
                // If there is an asterick, it might have a slash after it indicating the end of
                // the block comment.
                '*' => self.state = BlockComment2,
                // Otherwise, the character is part of the comment and should be disregarded.
                _ => (),
            },
            BlockComment2 => match c {
                // There is just another asterick. This one still might have a slash after it.
                '*' => (),
                // There is a slash after the asterick, the comment has now ended.
                '/' => self.state = LookingForToken,
                // The asterisk was a red herring, comment continues.
                _ => self.state = BlockComment,
            },
        }
        Ok(())
    }

    /// Called when the end of the file is reached so that the last token we were parsing can finish
    /// up. For example if we were parsing an identifier, we need this step to convert it to a token
    /// otherwise it would stay in the buffer and die with the tokenizer.
    fn process_eof(&mut self) -> Result<(), String> {
        use TokenizerState::*;
        match self.state {
            LookingForToken | LineComment => (),
            StringLiteral => return Err(format!("Unterminated string literal")),
            IntegerLiteral => self.finish_integer_literal()?,
            KeywordOrIdentifier => self.finish_keyword_or_identifier(),
            BlockComment | BlockComment2 => return Err(format!("Unterminated block comment")),
        }
        Ok(())
    }

    fn tokenize(mut self, program: &str, filename: &str) -> Result<TokenizedProgram, String> {
        let mut line = 1;
        let mut col = 0;
        let mut iterator = program.chars().peekable();
        while let (Some(c), next_char) = (iterator.next(), iterator.peek()) {
            self.peek_next = next_char.cloned();
            if c == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
            // Process characters and add context information to any errors produced.
            self.process_char(c).map_err(|err| {
                format!(
                    "Error encountered parsing code at {}:{}:{}, details:\n{}",
                    filename, &line, &col, err
                )
            })?;
        }
        self.process_eof().map_err(|err| {
            format!(
                "Error encountered parsing code at {}:{}:{}, details:\n{}",
                filename, &line, &col, err
            )
        })?;
        Ok(TokenizedProgram {
            tokens: self.tokens,
        })
    }
}

pub fn tokenize(program: &str, filename: &str) -> Result<TokenizedProgram, String> {
    Tokenizer {
        tokens: Vec::new(),
        peek_next: None,
        state: TokenizerState::LookingForToken,
        buffer: String::new(),
    }
    .tokenize(program, filename)
}
