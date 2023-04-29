use rote::grammar::token::{parse_char_escape, parse_char_literal, StrCursor, StrParser};

fn main() {
    let _ = dbg!(parse_char_escape(
        &mut StrParser::new(StrCursor::new(r"\x0A")),
        true,
    ));

    let _ = dbg!(parse_char_literal(&mut StrParser::new(StrCursor::new(
        r"'\u'"
    )),));
}
