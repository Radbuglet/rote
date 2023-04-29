use rote::grammar::token::{
    parse_char_escape, parse_char_literal, parse_non_raw_string_quote, StrCursor, StrParser,
};

fn main() {
    let _ = dbg!(parse_char_escape(
        &mut StrParser::new(StrCursor::new(r"\x0A")),
        true,
    ));

    let _ = dbg!(parse_char_literal(&mut StrParser::new(StrCursor::new(
        r"'\u'"
    )),));

    let _ = dbg!(parse_non_raw_string_quote(
        &mut StrParser::new(StrCursor::new(
            r#""Hello, world!\
		Foo
		sdjfklas""#,
        )),
        true
    ));
}
