use rote::token::TokenLiteral;

fn main() {
    let _ = dbg!(TokenLiteral::from_quote(r##"br#"Whee"#""##));
    let _ = dbg!(TokenLiteral::from_quote(r#"0xDEAD_BEEF"#));
    let _ = dbg!(TokenLiteral::from_quote(r#"3.1__4E-3a32"#));
}
