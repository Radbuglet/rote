// See reference: https://doc.rust-lang.org/reference/tokens.html
// (Archive link: https://web.archive.org/web/20230415120259/https://doc.rust-lang.org/reference/tokens.html)
pub mod token {
    use std::{cmp::Ordering, str::Chars};

    use unicode_xid::UnicodeXID;

    use crate::parser::{
        Cursor, ParseError, Parser, ParserHinter, SpanCursor, StreamCursor, UnexpectedFormatter,
    };

    // === StrCursor === //

    pub type StrParser<'a> = Parser<'a, StrCursor<'a>>;

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

    pub fn parse_char_escape<'a>(
        p: &mut StrParser<'a>,
        is_non_byte: bool,
    ) -> StrResult<'a, Option<char>> {
        // Match a `\`
        if !p.expecting("\\").try_match(|c| c.consume() == Some('\\')) {
            return Ok(None);
        }

        // Match the actual escape
        if let Some(escaped) = parse_char_escape_no_backslash(p, is_non_byte)? {
            Ok(Some(escaped))
        } else {
            Err(p.unexpected(StrUnexpectedFromSpan))
        }
    }

    pub fn parse_char_escape_no_backslash<'a>(
        p: &mut StrParser<'a>,
        is_non_byte: bool,
    ) -> StrResult<'a, Option<char>> {
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
            if is_non_byte && !escaped.is_ascii() {
                return Err(ParseError::new_invalid(
                    char_span,
                    "Invalid ASCII escape. ASCII escapes must be less than 0x7F.",
                ));
            }

            return Ok(Some(escaped as char));
        }

        // Try to parse a Unicode escape
        if is_non_byte {
            if p.expecting("u").try_match(|c| c.consume() == Some('u')) {
                // TODO
                return Err(ParseError::new_invalid(
                    p.cursor().span_one(),
                    "Unicode escapes are not yet supported!",
                ));
            }
        } else {
            if p.cursor().peek() == Some('u') {
                p.hint("Unicode escapes are not allowed in byte strings.");
            }
        }

        // Yield none if the escape mode was unexpected
        Ok(None)
    }

    pub fn parse_char_literal<'a>(p: &mut StrParser<'a>) -> StrResult<'a, Option<char>> {
        // Try to match the opening quote.
        if !p.expecting("'").try_match(|c| c.consume() == Some('\'')) {
            return Ok(None);
        }

        // Match either an escape or a character
        let char = 'b: {
            // Try to parse an escape.
            if let Some(escaped) = parse_char_escape(p, true)? {
                break 'b escaped;
            }

            // Try to parse a regular character
            if let Some(char) = p.expecting("a character").try_match(|c| {
                c.consume()
                    .filter(|char| !['\'', '\\', '\n', '\r', '\t'].contains(char))
            }) {
                break 'b char;
            }

            return Err(ParseError::new_invalid(
                p.cursor().span_one(),
                format!(
					"Invalid character in character literal. Character literals cannot contained unescaped {:?}.",
					p.cursor().peek().unwrap_or('\0'),
				),
            ));
        };

        // Try to match the closing quote
        if !p.expecting("'").try_match(|c| c.consume() == Some('\'')) {
            return Err(p.unexpected(StrUnexpectedFromSpan));
        }

        Ok(Some(char))
    }

    pub fn consume_line_break(c: &mut StrCursor) -> bool {
        // CRLF
        c.lookahead(|c| c.consume() == Some('\r') && c.consume() == Some('\n'))
			// LF
            || c.lookahead(|c| c.consume() == Some('\n'))
    }

    fn consume_char_in_str_parser(
        c: &mut StrCursor,
        h: &mut ParserHinter<'_, '_, StrCursor>,
        is_non_byte: bool,
    ) -> Option<char> {
        c.consume()
            // Deny isolated carriage returns since they could act weirdly. Since we already
            // match `\r\n` with a higher precedence, matching a lone `\r` here means that
            // this is indeed invalid.
            .filter(|ch| {
                if *ch == '\r' {
                    h.hint("Isolated carriage returns without a linefeed cannot appear in a string literal. Use `\\r` instead.");
                    false
                } else {
                    true
                }
            })
            // Deny characters
            .filter(|ch| {
                if !is_non_byte && !ch.is_ascii() {
                    h.hint("Unicode characters are not allowed in byte strings.");
                    false
                } else {
                    true
                }
            })
    }

    pub fn parse_non_raw_string_quote<'a>(
        p: &mut StrParser<'a>,
        is_non_byte: bool,
    ) -> StrResult<'a, String> {
        // Match string delimiter
        if !p.expecting("\"").try_match(|c| c.consume() == Some('"')) {
            return Err(p.unexpected(StrUnexpectedFromSpan));
        }

        // Match characters
        let mut text = String::new();

        loop {
            // Try to match a character escape
            if p.expecting("\\").try_match(|c| c.consume() == Some('\\')) {
                // Match a regular character escape
                if let Some(escaped) = parse_char_escape_no_backslash(p, is_non_byte)? {
                    text.push(escaped);
                    continue;
                }

                // Try to match a newline
                if p.expecting("newline").try_match(consume_line_break) {
                    // Ignore breaks
                    while p.expecting("whitespace").try_match(|c| {
                        c.consume()
                            .filter(|c| ['\t', ' ', '\r', '\n'].contains(c))
                            .is_some()
                    }) {
                        // (fallthrough)
                    }
                    continue;
                }

                // Failed to match an escaped character
                return Err(p.unexpected(StrUnexpectedFromSpan));
            }

            // Try to match a closing delimiter
            if p.expecting("\"").try_match(|c| c.consume() == Some('"')) {
                break;
            }

            // Try to match a string character
            if let Some(char) = p
                .expecting("string character")
                .try_match_hinted(|c, h| consume_char_in_str_parser(c, h, is_non_byte))
            {
                text.push(char);
                continue;
            }

            return Err(p.unexpected(StrUnexpectedFromSpan));
        }

        Ok(text)
    }

    pub fn parse_raw_string_quote<'a>(
        p: &mut StrParser<'a>,
        pounds: u8,
        is_non_byte: bool,
    ) -> StrResult<'a, Option<String>> {
        // Match the `"` delimiter
        if !p.expecting("\"").try_match(|c| c.consume() == Some('"')) {
            return Ok(None);
        }

        // Match until we find a `"` followed by `pounds` "#" characters.
        let mut text = String::new();
        loop {
            // Try to match the string delimiter
            if p.expecting("string delimiter").try_match(|c| {
                c.consume() == Some('"') && (0..pounds).all(|_| c.consume() == Some('#'))
            }) {
                break;
            }

            // Try to match a newline
            if p.expecting("newline").try_match(consume_line_break) {
                text.push('\n');
                continue;
            }

            // Try to match a string character
            if let Some(char) = p
                .expecting("string character")
                .try_match_hinted(|c, h| consume_char_in_str_parser(c, h, is_non_byte))
            {
                text.push(char);
                continue;
            }

            return Err(p.unexpected(StrUnexpectedFromSpan));
        }

        Ok(Some(text))
    }

    // pub fn parse_number<'a>(p: &mut StrParser<'a>) -> StrResult<'a, Option<String>> {}

    pub fn parse_suffix<'a>(p: &mut StrParser<'a>) -> String {
        let mut builder = String::new();
        while let Some(char) = p
            .expecting("suffix")
            .try_match(|c| c.consume().filter(|c| c.is_xid_continue()))
        {
            builder.push(char);
        }

        builder
    }
}
