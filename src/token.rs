use std::{
    any::{Any, TypeId},
    borrow::Cow,
    fmt,
    sync::Arc,
};

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
    Directive(TokenDirective),
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
    String(StringLiteral),
    Number(NumericLiteral),
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub raw: bool,
    pub byte: bool,
    pub text: CowString,
}

#[derive(Debug, Clone)]
pub struct NumericLiteral {
    prefix: NumberPrefix,
    int_part: u128,
    float_part: Option<NumericLiteralFPart>,
    suffix: CowString,
}

#[derive(Debug, Clone)]
pub struct NumericLiteralFPart {
    fractional: Option<u128>,
    exponent: Option<(FPartSign, u128)>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum NumberPrefix {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FPartSign {
    Positive,
    Negative,
    ImplicitPositive,
}

// See reference: https://doc.rust-lang.org/reference/tokens.html
// (Archive link: https://web.archive.org/web/20230415120259/https://doc.rust-lang.org/reference/tokens.html)
mod literal_parser {
    use std::{cmp::Ordering, iter, str::Chars};

    use unicode_xid::UnicodeXID;

    use crate::{
        parser::{
            Cursor, MatchResult, ParseError, Parser, ParserHinter, SpanCursor, StreamCursor,
            UnexpectedFormatter,
        },
        token::{FPartSign, NumberPrefix, NumericLiteralFPart},
        util::{lazy_format, new_cow_string, unwrap_display},
    };

    use super::{DecodedTokenLiteral, NumericLiteral, StringLiteral};

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

    fn parse_char_escape<'a>(
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

    fn parse_char_escape_no_backslash<'a>(
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

    fn parse_char_literal<'a>(p: &mut StrParser<'a>) -> StrResult<'a, Option<char>> {
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

    fn consume_line_break(c: &mut StrCursor) -> bool {
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

    fn parse_non_raw_string_quote<'a>(
        p: &mut StrParser<'a>,
        is_non_byte: bool,
    ) -> StrResult<'a, Option<String>> {
        // Match string delimiter
        if !p.expecting("\"").try_match(|c| c.consume() == Some('"')) {
            return Ok(None);
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

        Ok(Some(text))
    }

    fn parse_raw_string_quote<'a>(
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

    fn parse_numeric_literal<'a>(p: &mut StrParser<'a>) -> StrResult<'a, Option<NumericLiteral>> {
        fn parse_digits<'a>(
            p: &mut StrParser<'a>,
            mut accum: u128,
            radix: u32,
        ) -> StrResult<'a, u128> {
            let section_start = p.fork_cursor();

            loop {
                // Try to match a digit
                if let Some(digit) = p.expecting("digit").try_match(|c| {
                    c.consume()
                        // Ensure that this is a potential digit for our radix. We always consume all
                        // decimal digits to avoid weird scenarios where `0b00103` technically has
                        // the suffix `3`, which is weird.
                        .filter(|ch| {
                            if radix == 16 {
                                ch.is_ascii_hexdigit()
                            } else {
                                ch.is_ascii_digit()
                            }
                        })
                }) {
                    // Parse the digit and ensure that the digit is actually valid for this radix.
                    let digit = match digit.to_digit(radix) {
                        Some(digit) => digit,
                        None => {
                            return Err(ParseError::new_invalid(
                                section_start.span_until(p.cursor()),
                                lazy_format!("invalid digit for a base {radix} literal"),
                            ));
                        }
                    };

                    // Shift the accumulator and ensure that the number isn't too big.
                    accum = match accum.checked_mul(radix.into()) {
                        Some(accum) => accum,
                        None => {
                            return Err(ParseError::new_invalid(
                                section_start.span_until(p.cursor()),
                                "Digit sequence produces a numeric literal too big to be represented in the program.",
                            ));
                        }
                    };

                    // Add the digit to the accumulator.
                    accum = accum + u128::from(digit);

                    continue;
                }

                // Try to match a spacing
                if p.expecting("_").try_match(|c| c.consume() == Some('_')) {
                    // (ignores the character entirely)
                    continue;
                }

                // Otherwise, break.
                break;
            }

            Ok(accum)
        }

        // Try to match a starting digit
        let section_start = p.fork_cursor();
        let Some(first_digit) = p
            .expecting("digit")
            .try_match(|c| c.consume().filter(|c| c.is_ascii_digit()))
        else {
            return Ok(None);
        };

        // Match the numeric type marker.
        let prefix = 'b: {
            if first_digit != '0' {
                break 'b NumberPrefix::Decimal;
            }

            let modes = [
                ('b', NumberPrefix::Binary),
                ('o', NumberPrefix::Octal),
                ('x', NumberPrefix::Hexadecimal),
            ];

            modes
                .into_iter()
                .find_map(|(mode_char, mode)| {
                    if p.expecting(mode_char).try_match_hinted(|c, h| {
                        let consumed = c.consume();
                        if consumed == Some(mode_char.to_ascii_uppercase()) {
                            h.hint(lazy_format!(
                                "The number prefix `0{mode_char}` should be in lowercase."
                            ));
                        }

                        consumed == Some(mode_char)
                    }) {
                        Some(mode)
                    } else {
                        None
                    }
                })
                .unwrap_or(NumberPrefix::Decimal)
        };

        // Match the integer part
        let int_part = parse_digits(
            p,
            if prefix == NumberPrefix::Decimal {
                (first_digit as u32 - '0' as u32).into()
            } else {
                0
            },
            prefix as u32,
        )?;

        // Match the floating part if relevant
        let float_part = if prefix == NumberPrefix::Decimal {
            // Match the fractional part...
            let fractional = 'b: {
                // Match the `.`
                if !p.expecting(".").try_match(|c| c.consume() == Some('.')) {
                    break 'b None;
                }

                // Match the digits
                Some(parse_digits(p, 0, NumberPrefix::Decimal as u32)?)
            };

            // Match the exponential part
            let exponent = 'b: {
                // Match the `E`
                if !p.expecting("E").try_match(|c| c.consume() == Some('E')) {
                    break 'b None;
                }

                // Match an optional sign
                let sign = 'c: {
                    if p.expecting("+").try_match(|c| c.consume() == Some('+')) {
                        break 'c FPartSign::Positive;
                    }

                    if p.expecting("-").try_match(|c| c.consume() == Some('-')) {
                        break 'c FPartSign::Negative;
                    }

                    FPartSign::ImplicitPositive
                };

                // Match the digits
                Some((sign, parse_digits(p, 0, NumberPrefix::Decimal as u32)?))
            };

            Some(NumericLiteralFPart {
                fractional,
                exponent,
            })
        } else {
            // Otherwise, deny decimal points or exponents.
            if p.cursor().peek() == Some('.') || p.cursor().peek() == Some('E') {
                return Err(ParseError::new_invalid(
                    section_start.span_until(p.cursor()),
                    lazy_format!(
                        "Floating-point {} numbers are not supported",
                        match prefix {
                            NumberPrefix::Binary => "binary",
                            NumberPrefix::Octal => "octal",
                            NumberPrefix::Decimal => unreachable!(),
                            NumberPrefix::Hexadecimal => "hexadecimal",
                        }
                    ),
                ));
            }

            None
        };

        // Match the numeric suffix
        let suffix = new_cow_string(parse_suffix(p));

        // Construct a numeric literal
        Ok(Some(NumericLiteral {
            prefix,
            int_part,
            float_part,
            suffix,
        }))
    }

    fn parse_suffix<'a>(p: &mut StrParser<'a>) -> String {
        let mut builder = String::new();
        while let Some(char) = p
            .expecting("suffix")
            .try_match(|c| c.consume().filter(|c| c.is_xid_continue()))
        {
            builder.push(char);
        }

        builder
    }

    // === Driver === //

    impl DecodedTokenLiteral {
        fn try_parse<'a>(text: &'a str) -> StrResult<'a, DecodedTokenLiteral> {
            // Construct the parser
            let p = &mut Parser::new(StrCursor::new(text));

            // Try to parse the literal
            let literal = 'b: {
                // Try to parse the literal as a character
                if let Some(char) = parse_char_literal(p)? {
                    break 'b DecodedTokenLiteral::Char(char);
                }

                // Try to parse the literal as a number
                if let Some(number) = parse_numeric_literal(p)? {
                    break 'b DecodedTokenLiteral::Number(number);
                }

                // Try to parse the literal as a string
                {
                    // Match the byte prefix
                    let is_byte = p.expecting("b").try_match(|c| c.consume() == Some('b'));

                    // Match the raw string prefix
                    let is_raw = p.expecting("r").try_match(|c| c.consume() == Some('r'));

                    if is_raw {
                        // Match the delimiters
                        let pounds_start = p.fork_cursor();
                        let pounds = iter::from_fn(|| {
                            p.expecting("#")
                                .try_match(|c| c.consume().filter(|c| *c == '#'))
                        })
                        .count();

                        let Ok(pounds) = u8::try_from(pounds) else {
                            return Err(ParseError::new_invalid(
                                pounds_start.span_until(p.cursor()),
                                "A raw string can have at most 255 `#` characters in its delimiter.",
                            ));
                        };

                        // Match the string text
                        let Some(text) = parse_raw_string_quote(p, pounds, !is_byte)? else {
                            return Err(p.unexpected(StrUnexpectedFromSpan));
                        };

                        return Ok(DecodedTokenLiteral::String(StringLiteral {
                            raw: true,
                            byte: is_byte,
                            text: new_cow_string(text),
                        }));
                    } else {
                        match (parse_non_raw_string_quote(p, !is_byte)?, is_byte) {
                            (Some(text), is_byte) => {
                                return Ok(DecodedTokenLiteral::String(StringLiteral {
                                    raw: false,
                                    byte: is_byte,
                                    text: new_cow_string(text),
                                }));
                            }
                            (None, false) => {
                                // (fallthrough)
                            }
                            (None, true) => {
                                return Err(p.unexpected(StrUnexpectedFromSpan));
                            }
                        };
                    }
                }

                return Err(p.unexpected(StrUnexpectedFromSpan));
            };

            // Ensure that we're at the end of the stream
            if p.expecting("literal end")
                .try_match(|c| c.consume().is_none())
            {
                Ok(literal)
            } else {
                Err(p.unexpected(StrUnexpectedFromSpan))
            }
        }

        pub fn parse(text: &str) -> Self {
            unwrap_display(Self::try_parse(text))
        }
    }
}

mod literal_builder {}

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

// === Directive === //

#[derive(Debug, Clone)]
pub struct TokenDirective {
    data: Arc<dyn Directive>,
}

pub trait Directive: 'static + fmt::Debug + Send + Sync {
    fn meta(&self, id: TypeId) -> Option<&(dyn Any + Send + Sync)>;
}
