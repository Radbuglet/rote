use std::{
    any::{Any, TypeId},
    borrow::Cow,
    fmt,
    sync::Arc,
};

// Binds self as `rote` into the extern prelude for this module so `::rote` can always refer to
// this module.
extern crate self as rote;

// === Source === //

#[derive(Debug, Clone)]
pub enum Source {
    Tree(SourceTree),
    Ident(SourceIdent),
    Punct(SourcePunct),
    RawText(SourceRawText),
    Directive(SourceDirective),
}

// Conversions
impl From<SourceTree> for Source {
    fn from(value: SourceTree) -> Self {
        Self::Tree(value)
    }
}

impl From<SourceIdent> for Source {
    fn from(value: SourceIdent) -> Self {
        Self::Ident(value)
    }
}

impl From<SourcePunct> for Source {
    fn from(value: SourcePunct) -> Self {
        Self::Punct(value)
    }
}

impl From<SourceRawText> for Source {
    fn from(value: SourceRawText) -> Self {
        Self::RawText(value)
    }
}

impl From<SourceDirective> for Source {
    fn from(value: SourceDirective) -> Self {
        Self::Directive(value)
    }
}

// === SourceTree === //

#[derive(Debug, Clone)]
pub struct SourceTree {
    // N.B. this is the position of the tree delimiter w.r.t the parent printing context. The printing
    // context is reset *within* the token and takes off from the span of the first contained token.
    pub open_pos: TokenPos,
    pub close_pos: TokenPos,
    pub delimiter: SourceTreeDelimiter,
    pub tokens: Vec<Source>,
}

#[derive(Debug, Clone)]
pub enum SourceTreeDelimiter {
    Paren,
    Brace,
    Bracket,
    Virtual,
}

impl SourceTree {
    // === Constructors === //

    pub fn new(open_pos: TokenPos, close_pos: TokenPos, delimiter: SourceTreeDelimiter) -> Self {
        Self {
            open_pos,
            close_pos,
            delimiter,
            tokens: Vec::new(),
        }
    }

    pub fn new_root(delimiter: SourceTreeDelimiter) -> Self {
        Self::new(TokenPos::DUMMY, TokenPos::DUMMY, delimiter)
    }

    pub fn with(mut self, token: impl Into<Source>) -> Self {
        self.tokens.push(token.into());
        self
    }
}

// === SourceIdent === //

#[derive(Debug, Clone)]
pub struct SourceIdent {
    pub start: TokenPos,
    pub ident: Cow<'static, str>,
}

impl SourceIdent {
    pub fn new(start: TokenPos, ident: impl Into<Cow<'static, str>>) -> Self {
        Self {
            start,
            ident: ident.into(),
        }
    }
}

// === SourcePunct === //

#[derive(Debug, Clone)]
pub struct SourcePunct {
    pub loc: TokenPos,
    pub punct: char,
}

impl SourcePunct {
    pub fn new(loc: TokenPos, punct: char) -> Self {
        Self { loc, punct }
    }
}

// === SourceRawText === //

#[derive(Debug, Clone)]
pub struct SourceRawText {
    pub start: TokenPos,
    pub text: Cow<'static, str>,
}

impl SourceRawText {
    pub fn new(start: TokenPos, text: impl Into<Cow<'static, str>>) -> Self {
        Self {
            start,
            text: text.into(),
        }
    }
}

// === SourceDirective === //

#[derive(Debug, Clone)]
pub struct SourceDirective {
    pub start_pos: TokenPos,
    pub end_pos: TokenPos,
    pub data: Arc<dyn Directive>,
}

impl SourceDirective {
    pub fn new(start_pos: TokenPos, end_pos: TokenPos, data: impl Directive) -> Self {
        Self {
            start_pos,
            end_pos,
            data: Arc::new(data),
        }
    }
}

pub trait Directive: fmt::Debug + Any {
    fn meta(&self, key: TypeId) -> Option<&dyn Any>;
    fn value(&self) -> &dyn Any;
}

// === TokenPos === //

#[derive(Copy, Clone)]
pub struct TokenPos {
    pub line: u32,
    pub column: u32,
}

impl fmt::Debug for TokenPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_dummy() {
            f.write_str("N/A")
        } else {
            write!(f, "{}:{}", self.line + 1, self.column)
        }
    }
}

impl TokenPos {
    pub const DUMMY: Self = Self::new(u32::MAX, u32::MAX);

    pub const fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }

    pub fn is_dummy(&self) -> bool {
        self.line == u32::MAX
    }
}

// === Macro === //

pub use rote_macros::rote;

#[doc(hidden)]
pub mod macro_internals {
    pub use std::{column, compile_error, line};
}
