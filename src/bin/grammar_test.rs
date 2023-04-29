use rote::grammar::token::{parse_char_escape, StrCursor, StrParser};

fn main() {
    let _ = dbg!(parse_char_escape(&mut StrParser::new(StrCursor::new(
        r"\x0A"
    ))));
}
