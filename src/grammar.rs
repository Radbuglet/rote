pub(crate) mod token {
    //     pub fn parse_char(p: &mut CharParser, allow_unicode: bool) -> anyhow::Result<char> {
    //         // Try to parse a character escape.
    //         if p.expecting("character escape")
    //             .try_match(|c| c.next() == Some('\\'))
    //         {
    //             // Try to parse a hexadecimal ASCII escape
    //             if p.expecting('x').try_match(|c| c.next() == Some('x')) {
    //                 // Match the two digits
    //                 let (a, b) = match p.expecting("2 hexadecimal digits").try_match(|c| {
    //                     match (c.next(), c.next()) {
    //                         (Some(a), Some(b)) if a.is_ascii_hexdigit() && b.is_ascii_hexdigit() => {
    //                             Some((a, b))
    //                         }
    //                         _ => None,
    //                     }
    //                 }) {
    //                     Some((a, b)) => (a, b),
    //                     None => {
    //                         bail!("Unexpected character.");
    //                     }
    //                 };
    //
    //                 let parsed = u8::from_str_radix(&format!("{a}{b}"), 16).unwrap() as char;
    //
    //                 // Ensure that this is a valid ASCII character
    //                 if !parsed.is_ascii() {
    //                     bail!("Hexadecimal escape {a}{b} must be lower than 0x7F to be a valid ASCII character.");
    //                 }
    //
    //                 return Ok(parsed);
    //             }
    //
    //             // Try to parse a unicode escape
    //             if allow_unicode && p.expecting('u').try_match(|c| c.next() == Some('u')) {
    //                 bail!("Unicode escapes are unsupported"); // TODO
    //             }
    //
    //             // Try to parse a regular escape
    //             for (char, res) in [
    //                 ('\'', '\''),
    //                 ('"', '"'),
    //                 ('n', '\n'),
    //                 ('r', '\r'),
    //                 ('t', '\t'),
    //                 ('\\', '\\'),
    //                 ('0', '\0'),
    //             ] {
    //                 if p.expecting(char).try_match(|c| c.next() == Some(char)) {
    //                     return Ok(res);
    //                 }
    //             }
    //
    //             bail!("Unexpected.");
    //         }
    //
    //         // Try to parse a regular character
    //         if let Some(char) = p.expecting("character").try_match(|c| c.next()) {
    //             return Ok(char);
    //         }
    //
    //         bail!("Unexpected.");
    //     }
}
