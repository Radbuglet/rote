use std::fmt;

use crate::util::UnsizedVec;

// === Core === //

// MatchResult
pub trait MatchResult {
    type Unwrapped;

    fn did_pass(&self) -> bool;

    fn unwrap(self) -> Self::Unwrapped;
}

impl MatchResult for bool {
    type Unwrapped = Self;

    fn did_pass(&self) -> bool {
        *self
    }

    fn unwrap(self) -> Self::Unwrapped {
        self
    }
}

impl<T> MatchResult for Option<T> {
    type Unwrapped = Self;

    fn did_pass(&self) -> bool {
        self.is_some()
    }

    fn unwrap(self) -> Self::Unwrapped {
        self
    }
}

impl<T, E> MatchResult for Result<T, E> {
    type Unwrapped = Self;

    fn did_pass(&self) -> bool {
        self.is_ok()
    }

    fn unwrap(self) -> Self::Unwrapped {
        self
    }
}

// Cursor
pub trait Cursor: Clone {
    fn lookahead_raw<F, R>(&mut self, matcher: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: MatchResult,
    {
        let mut fork = self.clone();
        let res = matcher(&mut fork);

        if res.did_pass() {
            *self = fork;
        }

        res
    }

    fn lookahead<F, R>(&mut self, matcher: F) -> R::Unwrapped
    where
        F: FnOnce(&mut Self) -> R,
        R: MatchResult,
    {
        self.lookahead_raw(matcher).unwrap()
    }
}

// Parser
pub struct Parser<C> {
    expectations: UnsizedVec<dyn ToString>,
    cursor: C,
}

impl<C: fmt::Debug> fmt::Debug for Parser<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("expectations", &self.expectations().collect::<Vec<_>>())
            .field("cursor", &self.cursor)
            .finish()
    }
}

impl<C> Parser<C> {
    pub fn new(cursor: C) -> Self {
        Self {
            expectations: UnsizedVec::default(),
            cursor,
        }
    }

    pub fn cursor(&self) -> &C {
        &self.cursor
    }

    pub fn cursor_mut(&mut self) -> &mut C {
        &mut self.cursor
    }

    pub fn fork_cursor(&self) -> C
    where
        C: Clone,
    {
        self.cursor.clone()
    }

    pub fn clear_expectations(&mut self) {
        self.expectations.clear();
    }

    pub fn expecting<T: 'static + ToString>(&mut self, expectation: T) -> &mut Self {
        self.expectations.push(expectation, |e| e);
        self
    }

    pub fn expectations(&self) -> impl Iterator<Item = String> + '_ {
        self.expectations.iter().map(ToString::to_string)
    }

    pub fn try_match<F, R>(&mut self, matcher: F) -> R::Unwrapped
    where
        C: Cursor,
        F: FnOnce(&mut C) -> R,
        R: MatchResult,
    {
        let mut fork = self.cursor.clone();
        let res = matcher(&mut fork);

        if res.did_pass() {
            self.cursor = fork;
            self.clear_expectations();
        }

        res.unwrap()
    }
}

// === StreamCursor === //

// StreamCursor
pub trait StreamCursor: Cursor {
    type Atom: StreamAtom;

    fn consume(&mut self) -> Self::Atom;

    fn peek(&self) -> Self::Atom {
        self.clone().consume()
    }

    fn skip_to_end(&mut self) {
        while !self.consume().is_eof() {
            // (fallthrough)
        }
    }

    fn span_one(&self) -> SpanCursor<Self>
    where
        Self: Ord,
    {
        let cursor = self.clone();
        let mut end = self.clone();
        end.consume();

        SpanCursor { cursor, end }
    }

    fn span_until(&self, other: &Self) -> SpanCursor<Self>
    where
        Self: Ord,
    {
        SpanCursor::new(self.clone(), other.clone())
    }
}

// StreamAtom
pub trait StreamAtom: Clone {
    fn eof() -> Self;

    fn is_eof(&self) -> bool;
}

impl<T: Clone> StreamAtom for Option<T> {
    fn eof() -> Self {
        None
    }

    fn is_eof(&self) -> bool {
        self.is_none()
    }
}

// SpanCursor
#[derive(Debug, Clone)]
pub struct SpanCursor<C> {
    pub cursor: C,
    pub end: C,
}

impl<C: Ord> SpanCursor<C> {
    pub fn new(a: C, b: C) -> Self {
        let [cursor, end] = {
            let mut ab = [a, b];
            ab.sort();
            ab
        };

        Self { cursor, end }
    }

    pub fn is_unit(&self) -> bool {
        self.cursor == self.end
    }
}

impl<C: Cursor> Cursor for SpanCursor<C> {}

impl<C: StreamCursor + Ord> StreamCursor for SpanCursor<C> {
    type Atom = C::Atom;

    fn consume(&mut self) -> Self::Atom {
        if self.cursor >= self.end {
            StreamAtom::eof()
        } else {
            self.cursor.consume()
        }
    }

    fn peek(&self) -> Self::Atom {
        self.cursor.peek()
    }
}

// === ParseError === //

// ParseError
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct ParseError<C> {
    pub offending: (SpanCursor<C>, String),
    pub expectations: Vec<String>,
    pub hints: Vec<(Option<SpanCursor<C>>, String)>,
}

// Parser Extensions
impl<C: StreamCursor + Ord> Parser<C> {
    pub fn error(&self, unexpected: impl UnexpectedFormatter<C>) -> ParseError<C> {
        self.error_spanned(self.cursor().span_one(), unexpected)
    }

    pub fn error_spanned(
        &self,
        span: SpanCursor<C>,
        unexpected: impl UnexpectedFormatter<C>,
    ) -> ParseError<C> {
        let unexpected = unexpected.format(&span);

        ParseError {
            offending: (span, unexpected),
            expectations: self.expectations().collect(),
            hints: Vec::new(),
        }
    }
}

pub trait UnexpectedFormatter<C: StreamCursor> {
    fn format(self, span: &SpanCursor<C>) -> String;
}

impl<C: StreamCursor, T: ToString> UnexpectedFormatter<C> for T {
    fn format(self, _span: &SpanCursor<C>) -> String {
        self.to_string()
    }
}
