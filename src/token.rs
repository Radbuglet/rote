use std::{
    any::{Any, TypeId},
    borrow::Cow,
    fmt::{self, Debug, Write},
    mem,
    sync::Arc,
};

use unicode_xid::UnicodeXID;

// === ToToken === //

// Conversions
pub trait ToToken: Sized {
    fn to_token(self) -> Token;
}

impl<T: Clone + ToToken> ToToken for &'_ T {
    fn to_token(self) -> Token {
        self.clone().to_token()
    }
}

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

impl ToToken for Token {
    fn to_token(self) -> Token {
        self
    }
}

macro_rules! impl_token_to_instance_conversions {
	($({
		to=$to_converter:ident,
		as=$as_converter:ident,
		as_mut=$as_mut_converter:ident,
		variant=$variant:ident,
		ty=$ty:ty,
	},)*) => {
		$(
			impl From<$ty> for Token {
				fn from(value: $ty) -> Self {
					Self::$variant(value)
				}
			}

			impl $ty {
				pub fn to_token(self) -> Token {
					self.into()
				}
			}

			impl ToToken for $ty {
				fn to_token(self) -> Token {
					// Inherent impl takes priority during name resolution
					self.to_token()
				}
			}
		)*

		impl Token {
			$(
				pub fn $to_converter(self) -> Option<$ty> {
					match self {
						Self::$variant(inner) => Some(inner),
						_ => None,
					}
				}

				pub fn $as_converter(&self) -> Option<&$ty> {
					match self {
						Self::$variant(inner) => Some(inner),
						_ => None,
					}
				}

				pub fn $as_mut_converter(&mut self) -> Option<&mut $ty> {
					match self {
						Self::$variant(inner) => Some(inner),
						_ => None,
					}
				}
			)*
		}
	};
}

impl_token_to_instance_conversions!(
    {
        to = to_group,
        as = as_group,
        as_mut = as_group_mut,
        variant = Group,
        ty = TokenGroup,
    },
    {
        to = to_ident,
        as = as_ident,
        as_mut = as_ident_mut,
        variant = Ident,
        ty = TokenIdent,
    },
    {
        to = to_punct,
        as = as_punct,
        as_mut = as_punct_mut,
        variant = Punct,
        ty = TokenPunct,
    },
    {
        to = to_literal,
        as = as_literal,
        as_mut = as_literal_mut,
        variant = Literal,
        ty = TokenLiteral,
    },
    {
        to = to_comment,
        as = as_comment,
        as_mut = as_comment_mut,
        variant = Comment,
        ty = TokenComment,
    },
    {
        to = to_spacing,
        as = as_spacing,
        as_mut = as_spacing_mut,
        variant = Spacing,
        ty = TokenSpacing,
    },
    {
        to = to_directive,
        as = as_directive,
        as_mut = as_directive_mut,
        variant = Directive,
        ty = TokenDirective,
    },
);

// === TokenGroup === //

#[derive(Debug, Clone)]
pub struct TokenGroup {
    delimiter: GroupDelimiter,

    // The margin indicates the space present in front of each newline. The margin level must
    // strictly increase with each group nesting. See [`GroupMargin`] documentation for more
    // details on the semantics of each mode.
    margin: GroupMargin,

    // Head spacing denotes the space between the left-most margin and the first actual token
    // contained in the group. The actual number of spaces inserted, therefore, depends upon the
    // actual delimiter chosen.
    head_spacing: u32,
    head_spacing_visible: bool,

    // To reduce the cost of a clone operation, groups are stored in an `Arc` and given `Cow`
    // semantics.
    tokens: Arc<Vec<Token>>,
    is_normalized: bool,
}

impl TokenGroup {
    // === Constructors === //

    pub fn new(delimiter: GroupDelimiter, margin: GroupMargin) -> Self {
        Self {
            delimiter,
            margin,
            head_spacing: 0,
            head_spacing_visible: false,
            is_normalized: true,
            tokens: Arc::new(Vec::new()),
        }
    }

    pub fn new_virtual(margin: GroupMargin) -> Self {
        Self::new(GroupDelimiter::Virtual, margin)
    }

    pub fn new_bracket(margin: GroupMargin) -> Self {
        Self::new(GroupDelimiter::Bracket, margin)
    }

    pub fn new_brace(margin: GroupMargin) -> Self {
        Self::new(GroupDelimiter::Brace, margin)
    }

    pub fn new_paren(margin: GroupMargin) -> Self {
        Self::new(GroupDelimiter::Parenthesis, margin)
    }

    // === Setters and Getters === //

    // delimiter
    pub fn delimiter(&self) -> GroupDelimiter {
        self.delimiter
    }

    pub fn set_delimiter(&mut self, delimiter: GroupDelimiter) {
        self.delimiter = delimiter;
    }

    // margin
    pub fn margin(&self) -> GroupMargin {
        self.margin
    }

    pub fn set_margin(&mut self, margin: GroupMargin) {
        self.margin = margin;
    }

    pub fn with_margin(mut self, margin: GroupMargin) -> Self {
        self.set_margin(margin);
        self
    }

    // head_spacing
    pub fn head_spacing(&self) -> u32 {
        self.head_spacing
    }

    pub fn set_head_spacing(&mut self, spacing: u32) {
        self.head_spacing = spacing;
    }

    pub fn head_spacing_visible(&self) -> bool {
        self.head_spacing_visible
    }

    pub fn set_head_spacing_visible(&mut self, enabled: bool) {
        self.head_spacing_visible = enabled;
    }

    pub fn with_head_spacing_visible(mut self) -> Self {
        self.set_head_spacing_visible(true);
        self
    }

    pub fn with_head_spacing_invisible(mut self) -> Self {
        self.set_head_spacing_visible(false);
        self
    }

    // tokens
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn tokens_mut_raw(&mut self) -> &mut Vec<Token> {
        self.is_normalized = false;
        self.tokens_mut_no_invalidate()
    }

    pub fn push_raw(&mut self, token: impl ToToken) {
        self.is_normalized = false;
        self.tokens_mut_raw().push(token.to_token());
    }

    pub fn extend_raw<T: ToToken>(&mut self, tokens: impl IntoIterator<Item = T>) {
        self.is_normalized = false;
        self.tokens_mut_raw()
            .extend(tokens.into_iter().map(ToToken::to_token));
    }

    pub fn with_raw(mut self, token: impl ToToken) -> Self {
        self.is_normalized = false;
        self.push_raw(token);
        self
    }

    pub fn with_many_raw<T: ToToken>(mut self, tokens: impl IntoIterator<Item = T>) -> Self {
        self.extend_raw(tokens);
        self
    }

    // === Helpers === //

    fn tokens_mut_no_invalidate(&mut self) -> &mut Vec<Token> {
        Arc::make_mut(&mut self.tokens)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum GroupDelimiter {
    Virtual,
    Bracket,
    Brace,
    Parenthesis,
}

impl GroupDelimiter {
    pub fn open_char(&self) -> &'static str {
        match self {
            GroupDelimiter::Virtual => "",
            GroupDelimiter::Bracket => "[",
            GroupDelimiter::Brace => "{",
            GroupDelimiter::Parenthesis => "(",
        }
    }

    pub fn close_char(&self) -> &'static str {
        match self {
            GroupDelimiter::Virtual => "",
            GroupDelimiter::Bracket => "]",
            GroupDelimiter::Brace => "}",
            GroupDelimiter::Parenthesis => ")",
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum GroupMargin {
    // Line-relative margins are relative to the left of the first non-whitespace character on the
    // currently-printed line.
    RelativeToLineStart(i32),

    // Cursor-relative margins are measured relative to the left side of the location right before
    // the group is printed.
    RelativeToCursor(i32),

    // Margin-relative margins are relative to the parent margin setting. It's as shrimple as that.
    RelativeToMargin(u32),
}

impl GroupMargin {
    pub const AT_MARGIN: Self = Self::RelativeToMargin(0);
    pub const AT_LINE: Self = Self::RelativeToLineStart(0);
    pub const AT_CURSOR: Self = Self::RelativeToCursor(0);
}

// === TokenGroup Normalization === //

impl TokenGroup {
    // This method enforces the following invariants:
    //
    // - Pure white-spaces are only produced by a single isolated `Spacing` token.
    //   - This implies that trailing whitespace is impossible to produce in a normalized stream.
    // - Virtual groups are flattened into their equivalent token sequence.
    // - There are no zero-sized tokens.
    // - Identifiers are combined into single valid identifiers.
    // - Identifiers and literals can never be glued to one another.
    //
    pub fn push_normalized_inner(tokens: &mut Vec<Token>, token: impl ToToken) {
        match token.to_token() {
            Token::Group(mut group) => {
                group.normalize();

                // If the delimiter is virtual, merge its contents into the stream directly.
                if group.delimiter() == GroupDelimiter::Virtual {
                    // Compute the group's internal margin
                    let cursor_info = Self::compute_line_margin_info({
                        let mut tokens = tokens.iter().rev();
                        move |tmp| {
                            tokens.next().map(|token| {
                                token.display(tmp, 0);
                                tmp.as_str()
                            })
                        }
                    });
                    let margin = group.compute_inner_margin(0, cursor_info);

                    // Transform visible head spaces into regular spaces
                    if let Some(head_spacing) = group.head_spacing_size() {
                        group.set_head_spacing_visible(false);
                        Self::push_normalized_inner(tokens, TokenSpacing::new_spaces(head_spacing));
                    }

                    // Dump the inner tokens into the stream, adjusting spaces where necessary.
                    for mut token in group.tokens().iter().cloned() {
                        if let Some(spacing) = token.as_spacing_mut() {
                            if spacing.lines() > 0 {
                                spacing.set_spaces(spacing.spaces() + margin);
                            }
                        }

                        Self::push_normalized_inner(tokens, token);
                    }
                } else {
                    // Transform visible head spaces into regular spaces
                    if let Some(head_spacing) = group.head_spacing_size() {
                        group.set_head_spacing_visible(false);
                        Self::push_normalized_inner(tokens, TokenSpacing::new_spaces(head_spacing));
                    }

                    // And add the group in directly.
                    tokens.push(group.into());
                }
            }
            Token::Ident(ident) => {
                // Ignore empty identifiers
                if ident.is_empty() {
                    return;
                }

                // Otherwise, try to combine them with the previous identifier.
                match tokens.last_mut() {
                    Some(Token::Ident(prev)) => {
                        // Combine with the previous identifier
                        prev.ident_mut().push_str(ident.ident());
                    }
                    Some(Token::Literal(lit)) => {
                        panic!("Identifier {ident:?} cannot be placed immediately after literal {lit:?}.");
                    }
                    _ => {
                        tokens.push(ident.into());
                    }
                }
            }
            Token::Punct(punct) => {
                // TODO: Enforce additional validation for `'` puncts.
                tokens.push(punct.into());
            }
            Token::Literal(lit) => {
                // Ensure that the literal does not come immediately after an identifier or another
                // literal.
                match tokens.last() {
                    Some(Token::Ident(ident)) => {
                        panic!("Literal {lit:?} cannot be placed immediately after an identifier {ident:?}.");
                    }
                    Some(Token::Literal(prev)) => {
                        panic!("Literal {lit:?} cannot be placed immediately after another literal {prev:?}.");
                    }
                    // Otherwise, add the token
                    _ => {
                        tokens.push(lit.into());
                    }
                }
            }
            Token::Comment(comment) => {
                // Comments are subject to no additional normalization.
                tokens.push(comment.into());
            }
            Token::Spacing(spacing) => {
                // Ignore empty spacings.
                if spacing.is_empty() {
                    return;
                }

                // Spaces are merged with previous spaces.
                if let Some(Token::Spacing(prev)) = tokens.last_mut() {
                    if spacing.lines() > 0 {
                        prev.set_lines(prev.lines() + spacing.lines());
                        prev.set_spaces(spacing.spaces());
                    } else {
                        prev.set_spaces(prev.spaces() + spacing.spaces());
                    }
                } else {
                    // But otherwise left alone.
                    tokens.push(spacing.into());
                }
            }
            Token::Directive(directive) => {
                // Directives are subject to no additional normalization.
                tokens.push(directive.into());
            }
        }
    }

    pub fn push_normalized(&mut self, token: impl ToToken) {
        // This method enforces the following invariants:
        //
        // - Pure white-spaces are only produced by a single isolated `Spacing` token.
        //   - This implies that trailing whitespace is impossible to produce in a normalized stream.
        // - Virtual groups are flattened into their equivalent token sequence.
        // - There are no zero-sized tokens.
        // - Identifiers are combined into single valid identifiers.
        // - Identifiers and literals can never be glued to one another.
        //

        // We ensure that the entire group is already normalized to avoid wasting the effort of
        // re-normalizing an already normalized sequence portion.
        self.normalize();

        // Then, we normalize the token additions.
        let tokens = self.tokens_mut_no_invalidate();
        Self::push_normalized_inner(tokens, token);
    }

    pub fn extend_normalized<T: ToToken>(&mut self, tokens: impl IntoIterator<Item = T>) {
        for token in tokens {
            self.push_normalized(token);
        }
    }

    pub fn with_normalized(mut self, token: impl ToToken) -> Self {
        self.push_normalized(token);
        self
    }

    pub fn with_many_normalized<T: ToToken>(mut self, tokens: impl IntoIterator<Item = T>) -> Self {
        self.extend_normalized(tokens);
        self
    }

    // TODO: Optimize
    pub fn normalize(&mut self) -> &mut Self {
        if self.is_normalized {
            return self;
        }

        self.is_normalized = true;

        let old_tokens = mem::replace(&mut self.tokens, Arc::new(Vec::new()));
        let new_tokens = Arc::get_mut(&mut self.tokens).unwrap();

        for token in old_tokens.iter() {
            Self::push_normalized_inner(new_tokens, token.clone());
        }

        self
    }

    pub fn into_normalized(mut self) -> Self {
        self.normalize();
        self
    }

    // TODO: Optimize
    pub fn clone_normalized(&self) -> Self {
        let mut cloned = self.clone();
        cloned.normalize();
        cloned
    }
}

impl Token {
    pub fn normalize_if_group(&mut self) -> &mut Self {
        if let Some(group) = self.as_group_mut() {
            group.normalize();
        }

        self
    }
}

// === TokenIdent === //

#[derive(Debug, Clone)]
pub struct TokenIdent {
    ident: Cow<'static, str>,
}

impl TokenIdent {
    // === Constructors, getters, and setters === //

    pub fn new(ident: impl Into<Cow<'static, str>>) -> Self {
        Self {
            ident: Self::assert_possibly_valid(ident.into()),
        }
    }

    pub fn ident(&self) -> &str {
        &self.ident
    }

    pub fn ident_mut(&mut self) -> &mut String {
        self.ident.to_mut()
    }

    pub fn set_ident(&mut self, ident: impl Into<Cow<'static, str>>) {
        self.ident = Self::assert_possibly_valid(ident.into());
    }

    // === Validation === //

    pub fn is_empty(&self) -> bool {
        self.ident.is_empty()
    }

    pub fn is_valid(&self) -> bool {
        Self::is_ident_str_valid(&self.ident)
    }

    pub fn is_valid_and_nonempty(&self) -> bool {
        !self.is_empty() && self.is_valid()
    }

    pub fn is_ident_str_valid(text: &str) -> bool {
        let mut text = text.chars();

        // Ensure that the first character is either missing or is an `xid_start`.
        text.next().map_or(true, |c| c.is_xid_start()) &&
			// Ensure that all subsequent characters are `xid_continue`.
			text.all(|c| c.is_xid_continue())
    }

    fn assert_possibly_valid(text: Cow<'static, str>) -> Cow<'static, str> {
        debug_assert!(
            text.chars()
                .all(|c| c.is_xid_start() || c.is_xid_continue()),
            "The identifier fragment {text:?} could never possibly be valid."
        );

        text
    }

    // TODO: handle identifier normalization
}

// === TokenPunct === //

#[derive(Debug, Clone)]
pub struct TokenPunct {
    char: char,
}

impl TokenPunct {
    pub fn new(char: char) -> Self {
        assert!(
            Self::is_valid_punct_char(char),
            "{char:?} is not a valid punctuation character"
        );

        Self { char }
    }

    pub fn char(&self) -> char {
        self.char
    }

    pub fn set_char(&mut self, char: char) {
        self.char = char;
    }

    pub fn is_valid_punct_char(char: char) -> bool {
        // Copied from `proc_macro`.
        const LEGAL_CHARS: &[char] = &[
            '=', '<', '>', '!', '~', '+', '-', '*', '/', '%', '^', '&', '|', '@', '.', ',', ';',
            ':', '#', '$', '?', '\'',
        ];

        LEGAL_CHARS.contains(&char)
    }
}

// === TokenLiteral === //

#[derive(Debug, Clone)]
pub struct TokenLiteral {
    quoted: Cow<'static, str>,
    decoded: DecodedTokenLiteral,
}

impl TokenLiteral {
    // TODO: Constructors

    pub fn from_quote(quoted: impl Into<Cow<'static, str>>) -> Self {
        let quoted = quoted.into();
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

impl From<char> for DecodedTokenLiteral {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

impl From<StringLiteral> for DecodedTokenLiteral {
    fn from(value: StringLiteral) -> Self {
        Self::String(value)
    }
}

impl From<NumericLiteral> for DecodedTokenLiteral {
    fn from(value: NumericLiteral) -> Self {
        Self::Number(value)
    }
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    byte: bool,
    text: Cow<'static, str>,
}

impl StringLiteral {
    pub fn new(byte: bool, text: impl Into<Cow<'static, str>>) -> Self {
        Self {
            byte,
            text: text.into(),
        }
    }

    pub fn new_text(text: impl Into<Cow<'static, str>>) -> Self {
        Self::new(false, text)
    }

    pub fn new_byte(text: impl Into<Cow<'static, str>>) -> Self {
        Self::new(true, text)
    }

    pub fn is_byte(&self) -> bool {
        self.byte
    }

    pub fn text(&self) -> &str {
        &self.text
    }
}

#[derive(Debug, Clone)]
pub struct NumericLiteral {
    pub integral: u128,
    pub fractional: Option<u128>,
    pub exponent: Option<i128>,
    pub suffix: Option<NumberSuffix>,
}

impl NumericLiteral {
    // TODO: Constructors
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum NumberSuffix {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    U128,
    I128,
    F32,
    F64,
}

impl NumberSuffix {
    const SUFFIXES: [(&'static str, Self); 12] = [
        ("u8", Self::U8),
        ("i8", Self::I8),
        ("u16", Self::U16),
        ("i16", Self::I16),
        ("u32", Self::U32),
        ("i32", Self::I32),
        ("u64", Self::U64),
        ("i64", Self::I64),
        ("u128", Self::U128),
        ("i128", Self::I128),
        ("f32", Self::F32),
        ("f64", Self::F64),
    ];

    pub fn suffix(&self) -> &'static str {
        Self::SUFFIXES[*self as usize].0
    }
}

// See reference: https://doc.rust-lang.org/reference/tokens.html
// (Archive link: https://web.archive.org/web/20230415120259/https://doc.rust-lang.org/reference/tokens.html)
mod literal_parser {
    use std::{cmp::Ordering, iter, str::Chars};

    use unicode_xid::UnicodeXID;

    use crate::{
        parser::{
            Cursor, ParseError, Parser, ParserHinter, SpanCursor, StreamCursor, UnexpectedFormatter,
        },
        token::NumberSuffix,
        util::{lazy_format, unwrap_display},
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
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        enum NumberPrefix {
            Binary = 2,
            Octal = 8,
            Decimal = 10,
            Hexadecimal = 16,
        }

        fn parse_digits<'a>(
            p: &mut StrParser<'a>,
            mut accum: i128,
            radix: u32,
        ) -> StrResult<'a, i128> {
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
                    accum = accum + i128::from(digit);

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
        let integral = parse_digits(
            p,
            if prefix == NumberPrefix::Decimal {
                (first_digit as u32 - '0' as u32).into()
            } else {
                0
            },
            prefix as u32,
        )?;

        // Match the floating part if relevant
        let (fractional, exponent) = if prefix == NumberPrefix::Decimal {
            // Match the fractional part...
            let fractional = 'b: {
                // Match the `.`
                if !p.expecting(".").try_match(|c| c.consume() == Some('.')) {
                    break 'b None;
                }

                // Match the digits
                Some(parse_digits(p, 0, NumberPrefix::Decimal as u32)? as u128)
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
                        break 'c 1i128;
                    }

                    if p.expecting("-").try_match(|c| c.consume() == Some('-')) {
                        break 'c -1i128;
                    }

                    1i128
                };

                // Match the digits
                Some(sign * parse_digits(p, 0, NumberPrefix::Decimal as u32)?)
            };

            (fractional, exponent)
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

            (None, None)
        };

        // Match the numeric suffix
        let suffix_start = p.fork_cursor();
        let suffix = if p.expecting("numeric suffix").try_match(|c| {
            let suffix_len = iter::from_fn(|| {
                if c.lookahead(|c| c.consume().filter(|c| c.is_xid_continue()).is_some()) {
                    Some(())
                } else {
                    None
                }
            })
            .count();

            suffix_len > 0
        }) {
            let suffix_span = suffix_start.span_until(p.cursor());

            match NumberSuffix::SUFFIXES
                .iter()
                .find(|(text, _)| suffix_span.clone().into_iter().eq(text.chars()))
            {
                Some((_, suffix)) => Some(*suffix),
                None => {
                    return Err(ParseError::new_invalid(
                        suffix_span.clone(),
                        format!(
                            "Unknown suffix {:?}. Expected one of: {:?}",
                            suffix_span.into_iter().collect::<String>(),
                            NumberSuffix::SUFFIXES
                                .iter()
                                .map(|(v, _)| v)
                                .collect::<Vec<_>>(),
                        ),
                    ))
                }
            }
        } else {
            None
        };

        // Construct a numeric literal
        Ok(Some(NumericLiteral {
            integral: integral as u128,
            fractional,
            exponent,
            suffix,
        }))
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
                            byte: is_byte,
                            text: text.into(),
                        }));
                    } else {
                        match (parse_non_raw_string_quote(p, !is_byte)?, is_byte) {
                            (Some(text), is_byte) => {
                                return Ok(DecodedTokenLiteral::String(StringLiteral {
                                    byte: is_byte,
                                    text: text.into(),
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

// === TokenComment === //

#[derive(Debug, Clone)]
pub struct TokenComment {
    kind: TokenCommentKind,
    style: TokenCommentStyle,
    text: Cow<'static, str>,
}

impl TokenComment {
    pub fn new(
        kind: TokenCommentKind,
        style: TokenCommentStyle,
        text: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self {
            kind,
            style,
            text: text.into(),
        }
    }

    pub fn new_line(text: impl Into<Cow<'static, str>>) -> Self {
        Self::new(TokenCommentKind::Inert, TokenCommentStyle::Line, text)
    }

    pub fn new_block(text: impl Into<Cow<'static, str>>) -> Self {
        Self::new(TokenCommentKind::Inert, TokenCommentStyle::Block, text)
    }

    pub fn new_doc_line(text: impl Into<Cow<'static, str>>) -> Self {
        Self::new(TokenCommentKind::OuterDoc, TokenCommentStyle::Line, text)
    }

    pub fn new_doc_block(text: impl Into<Cow<'static, str>>) -> Self {
        Self::new(TokenCommentKind::OuterDoc, TokenCommentStyle::Block, text)
    }

    pub fn new_mod_doc_line(text: impl Into<Cow<'static, str>>) -> Self {
        Self::new(TokenCommentKind::InnerDoc, TokenCommentStyle::Line, text)
    }

    pub fn new_mod_doc_block(text: impl Into<Cow<'static, str>>) -> Self {
        Self::new(TokenCommentKind::InnerDoc, TokenCommentStyle::Block, text)
    }

    pub fn kind(&self) -> TokenCommentKind {
        self.kind
    }

    pub fn set_kind(&mut self, kind: TokenCommentKind) {
        self.kind = kind;
    }

    pub fn style(&self) -> TokenCommentStyle {
        self.style
    }

    pub fn set_style(&mut self, style: TokenCommentStyle) {
        self.style = style;
    }

    pub fn is_inert(&self) -> bool {
        self.kind.is_inert()
    }

    pub fn is_doc(&self) -> bool {
        self.kind.is_doc()
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn set_text(&mut self, text: impl Into<Cow<'static, str>>) {
        self.text = text.into();
    }

    pub fn text_mut(&mut self) -> &mut String {
        self.text.to_mut()
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum TokenCommentKind {
    Inert,
    InnerDoc,
    OuterDoc,
}

impl TokenCommentKind {
    pub fn is_inert(&self) -> bool {
        *self == Self::Inert
    }

    pub fn is_doc(&self) -> bool {
        matches!(self, Self::InnerDoc | Self::OuterDoc)
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum TokenCommentStyle {
    Line,
    Block,
}

// === TokenSpacing === //

#[derive(Debug, Copy, Clone)]
pub struct TokenSpacing {
    lines: u32,
    spaces: u32,
}

impl TokenSpacing {
    // TODO: Add back support for outdentation?

    pub const NEWLINE: Self = Self::new_lines(1);
    pub const SPACE: Self = Self::new_spaces(1);

    pub const fn new(lines: u32, spaces: u32) -> Self {
        Self { lines, spaces }
    }

    pub const fn new_lines(count: u32) -> Self {
        Self::new(count, 0)
    }

    pub const fn new_spaces(count: u32) -> Self {
        Self::new(0, count)
    }

    pub fn lines(&self) -> u32 {
        self.lines
    }

    pub fn set_lines(&mut self, count: u32) {
        self.lines = count;
    }

    pub fn spaces(&self) -> u32 {
        self.spaces
    }

    pub fn set_spaces(&mut self, count: u32) {
        self.spaces = count;
    }

    pub fn is_empty(&self) -> bool {
        self.spaces == 0 && self.lines == 0
    }
}

// === TokenDirective === //
#[derive(Debug)]
pub struct TokenDirective {
    data: Box<dyn ErasedDirective>,
}

impl TokenDirective {
    pub fn new<T: Directive>(directive: T) -> Self {
        Self {
            data: Box::new(directive),
        }
    }

    pub fn data(&self) -> &(dyn Any + Send + Sync) {
        self.data.as_any()
    }

    pub fn data_mut(&mut self) -> &mut (dyn Any + Send + Sync) {
        self.data.as_any_mut()
    }

    pub fn try_downcast<T: 'static>(&self) -> Option<&T> {
        self.data().downcast_ref()
    }

    pub fn downcast<T: 'static>(&self) -> &T {
        self.try_downcast().unwrap()
    }

    pub fn try_downcast_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.data_mut().downcast_mut()
    }

    pub fn downcast_mut<T: 'static>(&mut self) -> &mut T {
        self.try_downcast_mut().unwrap()
    }

    pub fn try_meta<T: 'static>(&self) -> Option<&T> {
        let mut demand = Demand {
            requested: TypeId::of::<T>(),
            target: None,
        };
        self.data.meta(&mut demand);
        demand.target.map(|v| v.downcast_ref().unwrap())
    }

    pub fn meta<T: 'static>(&self) -> &T {
        self.try_meta().unwrap()
    }
}

impl Clone for TokenDirective {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone_boxed(),
        }
    }
}

#[derive(Debug)]
pub struct Demand<'p> {
    requested: TypeId,
    target: Option<&'p dyn Any>,
}

impl<'p> Demand<'p> {
    pub fn requested(&self) -> TypeId {
        self.requested
    }

    pub fn with<T: 'static>(&mut self, value: &'p T) -> &mut Self {
        if self.requested == TypeId::of::<T>() {
            self.target = Some(value);
        }
        self
    }
}

pub trait Directive: 'static + fmt::Debug + Send + Sync + Clone {
    fn meta(&self, demand: &mut Demand<'_>) {
        let _ = demand;
    }
}

trait ErasedDirective: 'static + fmt::Debug + Send + Sync {
    fn as_any(&self) -> &(dyn Any + Send + Sync);

    fn as_any_mut(&mut self) -> &mut (dyn Any + Send + Sync);

    fn meta(&self, demand: &mut Demand<'_>);

    fn clone_boxed(&self) -> Box<dyn ErasedDirective>;
}

impl<T: Directive> ErasedDirective for T {
    fn as_any(&self) -> &(dyn Any + Send + Sync) {
        self
    }

    fn as_any_mut(&mut self) -> &mut (dyn Any + Send + Sync) {
        self
    }

    fn meta(&self, demand: &mut Demand<'_>) {
        self.meta(demand)
    }

    fn clone_boxed(&self) -> Box<dyn ErasedDirective> {
        Box::new(self.clone())
    }
}

// === Display === //

impl Token {
    fn display(&self, buffer: &mut String, margin: u32) {
        match self {
            Token::Group(token) => token.display(buffer, margin),
            Token::Ident(token) => token.display(buffer, margin),
            Token::Punct(token) => token.display(buffer, margin),
            Token::Literal(token) => token.display(buffer, margin),
            Token::Comment(token) => token.display(buffer, margin),
            Token::Spacing(token) => token.display(buffer, margin),
            Token::Directive(token) => token.display(buffer, margin),
        }
    }
}

impl TokenGroup {
    // TODO: Merge this implementation with the group formatter's implementation to avoid character
    //  counting inconsistencies.
    fn compute_line_margin_info(
        mut next_fragment: impl FnMut(&mut String) -> Option<&str>,
    ) -> (u32, u32) {
        let mut char_count = 0u32;
        let mut accumulated_space_count = 0u32;
        let mut tmp = String::new();

        while let Some(fragment) = next_fragment(&mut tmp) {
            // Find the last line in this fragment.
            let (line_idx, line_text) = fragment
                // Unlike `.lines()`, `.split()` contains a trailing line. It also
                // contains `\r` but we don't worry about any lines besides the last
                // one anyways so that isn't too big of a deal.
                .split("\n")
                .enumerate()
                .last()
                // N.B. After normalization, there should never be a token resolving to
                // an empty character sequence. Hence, this should always return a
                // "line."
                .unwrap();

            // For every character, going from left to right...
            let mut local_space_count = 0;
            let mut matching_spaces = true;

            for char in line_text.chars() {
                // Increment the total character counter
                char_count += 1;

                // If we're still matching spaces...
                if matching_spaces {
                    // Increment the space counter if applicable
                    if char.is_whitespace() {
                        local_space_count += 1;
                    } else {
                        // If we reached the end of the whitespace section before the
                        // end of the fragment, set the accumulated space count to zero.
                        accumulated_space_count = 0;
                        matching_spaces = false;
                    }
                }
            }

            // Add the `local_space_count` to the `accumulated_space_count`. If we
            // found a non-whitespace character in our fragment, the accumulated
            // space count will be zero and will thus correctly reflect the fact
            // that only the most recent fragment has contributed white-spaces.
            accumulated_space_count += local_space_count;

            // If the `line_idx` is not zero, we know that our sequence contained `\n`,
            // telling us that we're done processing the line.
            if line_idx != 0 {
                break;
            }

            // If we're not breaking, clear the temporary buffer.
            tmp.clear();
        }

        (char_count, accumulated_space_count)
    }

    fn compute_inner_margin(
        &self,
        base_margin: u32,
        (cursor_pos, line_start_pos): (u32, u32),
    ) -> u32 {
        match self.margin() {
            GroupMargin::RelativeToLineStart(rel) => line_start_pos.saturating_add_signed(rel),
            GroupMargin::RelativeToCursor(rel) => cursor_pos.saturating_add_signed(rel),
            GroupMargin::RelativeToMargin(rel) => base_margin.saturating_add(rel),
        }
    }

    fn head_spacing_size(&self) -> Option<u32> {
        if self.head_spacing_visible() {
            Some(self.head_spacing() - self.delimiter.open_char().len() as u32)
        } else {
            None
        }
    }

    fn display(&self, buffer: &mut String, margin: u32) {
        // Determine the margin for the line
        let last_line = buffer.lines().last().unwrap_or("");
        let margin = self.compute_inner_margin(
            margin,
            (
                last_line.len() as u32,
                last_line.chars().take_while(|c| c.is_whitespace()).count() as u32,
            ),
        );

        // Push the delimiter
        if let Some(head_spacing) = self.head_spacing_size() {
            buffer.extend((0..head_spacing).map(|_| ' '));
        }

        buffer.push_str(self.delimiter().open_char());

        for token in self.tokens() {
            token.display(buffer, margin);
        }
        buffer.push_str(self.delimiter().close_char());
    }
}

impl TokenIdent {
    fn display(&self, buffer: &mut String, _margin: u32) {
        buffer.push_str(self.ident());
    }
}

impl TokenPunct {
    fn display(&self, buffer: &mut String, _margin: u32) {
        buffer.push(self.char());
    }
}

impl TokenLiteral {
    fn display(&self, buffer: &mut String, _margin: u32) {
        // FIXME: Multiline literals inherit spacing from the file, not the relative context.
        buffer.push_str(self.quoted());
    }
}

impl TokenComment {
    fn display(&self, _buffer: &mut String, _margin: u32) {
        todo!()
    }
}

impl TokenSpacing {
    fn display(&self, buffer: &mut String, margin: u32) {
        if self.lines > 0 {
            buffer.extend((0..self.lines).map(|_| '\n'));
            buffer.extend((0..margin).map(|_| ' '));
        }

        buffer.extend((0..self.spaces).map(|_| ' '));
    }
}

impl TokenDirective {
    fn display(&self, buffer: &mut String, _margin: u32) {
        write!(buffer, "${:?}", self.data()).unwrap();
    }
}

macro_rules! impl_display {
	($($ty:ty),*$(,)?) => {$(
		impl fmt::Display for $ty {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				let mut buffer = String::new();
				self.display(&mut buffer, 0);
				f.write_str(&buffer)
			}
		}
	)*};
}

impl_display!(
    Token,
    TokenGroup,
    TokenIdent,
    TokenPunct,
    TokenLiteral,
    TokenComment,
    TokenSpacing,
    TokenDirective,
);
