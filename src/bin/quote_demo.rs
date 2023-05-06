use rote::{quote::rote, token::TokenIdent};

#[allow(unused_braces)]
fn main() {
    let m = TokenIdent::new("xxxxxxxxxxxxxxxxxxxxxxxx");

    println!(
        "===\n{}.",
        dbg!(rote! {
            $${m}{
                This is overly indented.
            }
            ^ Brace closer should be here
        })
    );
}
