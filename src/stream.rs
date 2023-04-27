use std::{borrow::Cow, sync::Arc};

use crate::util::{new_cow_string, CowString, CowVec};

// === Token === //

#[derive(Debug, Clone)]
pub enum Token {
    Group(TokenGroup),
    Ident(TokenIdent),
    Punct(TokenPunct),
    Literal(TokenLiteral),
    Comment(TokenComment),
    Spacing(TokenSpacing),
}

impl From<TokenGroup> for Token {
    fn from(value: TokenGroup) -> Self {
        Self::Group(value)
    }
}

impl From<TokenIdent> for Token {
    fn from(value: TokenIdent) -> Self {
        Self::Ident(value)
    }
}

impl From<TokenPunct> for Token {
    fn from(value: TokenPunct) -> Self {
        Self::Punct(value)
    }
}

impl From<TokenLiteral> for Token {
    fn from(value: TokenLiteral) -> Self {
        Self::Literal(value)
    }
}

impl From<TokenComment> for Token {
    fn from(value: TokenComment) -> Self {
        Self::Comment(value)
    }
}

impl From<TokenSpacing> for Token {
    fn from(value: TokenSpacing) -> Self {
        Self::Spacing(value)
    }
}

// === TokenGroup === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum GroupDelimiter {
    Virtual,
    Bracket,
    Brace,
    Parenthesis,
}

#[derive(Debug, Clone)]
pub struct TokenGroup {
    delimiter: GroupDelimiter,
    tokens: CowVec<Token>,
}

impl TokenGroup {
    pub fn new(delimiter: GroupDelimiter, tokens: impl IntoIterator<Item = Token>) -> Self {
        Self {
            delimiter,
            tokens: Arc::new(Vec::from_iter(tokens)),
        }
    }

    pub fn delimiter(&self) -> GroupDelimiter {
        self.delimiter
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn tokens_mut(&mut self) -> &mut Vec<Token> {
        Arc::make_mut(&mut self.tokens)
    }
}

// === TokenIdent === //

#[derive(Debug, Clone)]
pub struct TokenIdent {
    ident: CowString,
}

impl TokenIdent {
    pub fn new(ident: impl Into<Cow<'static, str>>) -> Self {
        Self {
            ident: new_cow_string(ident),
        }
    }

    pub fn ident(&self) -> &str {
        &self.ident
    }
}

// === TokenPunct === //

#[derive(Debug, Clone)]
pub struct TokenPunct {
    char: char,
}

impl TokenPunct {
    pub fn new(char: char) -> Self {
        Self { char }
    }

    pub fn char(&self) -> char {
        self.char
    }
}

// === TokenLiteral === //

#[derive(Debug, Clone)]
pub struct TokenLiteral {
    quoted: CowString,
    decoded: DecodedTokenLiteral,
}

impl TokenLiteral {
    pub fn new(decoded: DecodedTokenLiteral) -> Self {
        todo!()
    }

    pub fn from_quote(quoted: impl Into<Cow<'static, str>>) -> Self {
        let quoted = new_cow_string(quoted);
        let decoded = DecodedTokenLiteral::parse(&quoted);

        Self { quoted, decoded }
    }

    pub fn quoted(&self) -> &str {
        &self.quoted
    }

    pub fn decoded(&self) -> &DecodedTokenLiteral {
        &self.decoded
    }
}

#[derive(Debug, Clone)]
pub enum DecodedTokenLiteral {
    Char(char),
    String(CowString),
    Number(AnyNumericLiteral),
}

impl DecodedTokenLiteral {
    pub fn parse(text: &str) -> Self {
        todo!();
    }
}

#[derive(Debug, Clone)]
pub enum AnyNumericLiteral {
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    U128(u128),
    I128(i128),
    F32(f32),
    F64(f64),
}

// === TokenComment === //

#[derive(Debug, Clone)]
pub struct TokenComment {
    kind: TokenCommentKind,
    style: TokenCommentStyle,
    text: CowString,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum TokenCommentKind {
    Inert,
    InnerDoc,
    OuterDoc,
}

#[derive(Debug, Clone)]
pub enum TokenCommentStyle {
    Line,
    Block,
}

// === TokenFormatting === //

#[derive(Debug, Clone)]
pub struct TokenSpacing {
    lines: u32,
    columns: u32,
}
