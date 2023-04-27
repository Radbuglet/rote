use std::fmt;

use crate::util::UnsizedVec;

// === Cursor === //

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

impl<T: Clone> Cursor for T {}

pub trait StreamCursor: Cursor {
    type Location;
    type Atom;

    fn prev_location(&self) -> Self::Location;

    fn next_location(&self) -> Self::Location;

    fn consume(&mut self) -> Self::Atom;

    fn peek(&self) -> Self::Atom {
        self.clone().consume()
    }
}

// === Parser === //

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
}

impl<C: Cursor> Parser<C> {
    pub fn try_match<F, R>(&mut self, matcher: F) -> R::Unwrapped
    where
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
