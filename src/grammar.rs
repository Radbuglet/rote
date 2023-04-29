pub mod token {
    use std::{cmp::Ordering, str::Chars};

    use crate::parser::{
        Cursor, ParseError, Parser, SpanCursor, StreamCursor, UnexpectedFormatter,
    };

    // === StrCursor === //

    pub type StrParser<'a> = Parser<StrCursor<'a>>;

    pub type StrParseError<'a> = ParseError<StrCursor<'a>>;

    pub type StrResult<'a, T> = Result<T, StrParseError<'a>>;

    #[derive(Debug, Clone)]
    pub struct StrCursor<'a> {
        pub remaining: Chars<'a>,
    }

    impl<'a> StrCursor<'a> {
        pub fn new(target: &'a str) -> Self {
            Self {
                remaining: target.chars(),
            }
        }
    }

    impl Cursor for StrCursor<'_> {}

    impl StreamCursor for StrCursor<'_> {
        type Atom = Option<char>;

        fn consume(&mut self) -> Self::Atom {
            self.remaining.next()
        }
    }

    impl Eq for StrCursor<'_> {}

    impl PartialEq for StrCursor<'_> {
        fn eq(&self, other: &Self) -> bool {
            // We assume that these two cursors are pointing to the same file.
            self.remaining.as_str().len() == other.remaining.as_str().len()
        }
    }

    impl Ord for StrCursor<'_> {
        fn cmp(&self, other: &Self) -> Ordering {
            self.remaining
                .as_str()
                .len()
                .cmp(&other.remaining.as_str().len())
                // Shorter strings mean further cursors
                .reverse()
        }
    }

    impl PartialOrd for StrCursor<'_> {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    #[derive(Debug, Clone)]
    pub struct StrUnexpectedFromSpan;

    impl<'a> UnexpectedFormatter<StrCursor<'a>> for StrUnexpectedFromSpan {
        fn format(self, span: &SpanCursor<StrCursor<'a>>) -> String {
            format!("{:?}", span.clone().into_iter().collect::<String>())
        }
    }

    // === Literals === //

    pub fn parse_char_escape<'p>(p: &mut StrParser<'p>) -> StrResult<'p, Option<char>> {
        // Try to consume a `\`
        if !p.expecting("\\").try_match(|c| c.consume() == Some('\\')) {
            return Ok(None);
        }

        // Try to parse a quote escape
        {
            let escapes = [
                ('\'', '\''),
                ('"', '"'),
                ('\\', '\\'),
                ('n', '\n'),
                ('r', '\r'),
                ('t', '\t'),
                ('0', '\0'),
            ];

            if let Some((_, escaped)) = escapes.into_iter().find(|(escape, _)| {
                p.expecting(*escape)
                    .try_match(|c| c.consume() == Some(*escape))
            }) {
                return Ok(Some(escaped));
            }
        }

        // Try to parse an ASCII escape
        if p.expecting("x").try_match(|c| c.consume() == Some('x')) {
            // Parse hexdigit
            let char_span = p.fork_cursor();
            let [a, b] = match p
                .expecting("2-digit hexadecimal escape sequence")
                .try_match(|c| {
                    Some([
                        c.consume().filter(|c| c.is_ascii_hexdigit())?,
                        c.consume().filter(|c| c.is_ascii_hexdigit())?,
                    ])
                }) {
                Some([a, b]) => [a, b],
                None => return Err(p.unexpected(StrUnexpectedFromSpan)),
            };
            let char_span = p.span_cursor(&char_span);

            // Decode hexdigit
            let escaped = u8::from_str_radix(&format!("{a}{b}"), 16).unwrap();
            if !escaped.is_ascii() {
                return Err(ParseError::new_invalid(
                    char_span,
                    "Invalid ASCII escape. ASCII escapes must be less than 0x7F.",
                ));
            }

            return Ok(Some(escaped as char));
        }

        // Try to parse a Unicode escape
        if p.expecting("u").try_match(|c| c.consume() == Some('u')) {
            if !p.expecting("{").try_match(|c| c.consume() == Some('{')) {
                p.unexpected(StrUnexpectedFromSpan);
            }
        }

        // Yield error if the escape mode was unexpected
        Err(p.unexpected(StrUnexpectedFromSpan))
    }
}
