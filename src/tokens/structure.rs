#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenizedProgram {
    pub tokens: Vec<Token>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Keyword(Keyword),
    Symbol(Symbol),
    IntegerConstant(i32),
    StringConstant(String),
    Identifier(String),
}

/// This is a macro for creating an enum with functions to convert back and forth from a string
/// representation.
macro_rules! keyword_enum {
    ($EnumName:ident $($VariantName:ident $value:literal)*) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub enum $EnumName {
            $($VariantName),*
        }
        impl $EnumName {
            /// Returns Ok(Variant) if `text` is the name of a variant of this enum,
            /// or Err(()) if not.
            pub fn from_text(text: &str) -> Result<Self, ()> {
                match text {
                    $($value => Ok(Self::$VariantName),)*
                    _ => Err(())
                }
            }

            /// Returns the name of this enum as a string.
            pub fn as_text(self) -> &'static str {
                match self {
                    $(Self::$VariantName => $value,)*
                }
            }
        }
    }
}

keyword_enum! {
    Keyword
    Class "class"
    Constructor "constructor"
    Function "function"
    Method "method"
    Field "field"
    Static "static"
    Var "var"
    Int "int"
    Char "char"
    Boolean "boolean"
    Void "void"
    True "true"
    False "false"
    Null "null"
    This "this"
    Let "let"
    Do "do"
    If "if"
    Else "else"
    While "while"
    Return "return"
}

keyword_enum! {
    Symbol
    RightBrace "{"
    LeftBrace "}"
    RightParen "("
    LeftParen ")"
    RightSquareBracket "["
    LeftSquareBracket "]"
    Dot "."
    Comma ","
    Semicolon ";"
    Plus "+"
    Minus "-"
    // the actual symbol names rather than the name of the operation they represent are used here
    // for clarity regarding what this enum actually does.
    Asterick "*"
    ForwardSlash "/"
    Ampersand "&"
    VerticalPipe "|"
    LeftAngleBracket "<"
    RightAngleBracket ">"
    Equals "="
    Tilde "~"
}
