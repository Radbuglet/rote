use rote::{quote::rote, token::Token};

fn main() {
    println!(
        "===\n{}.",
        dbg!(Token::from(rote! {
            whee
                {example 'a}
            margin
        }))
    );
}
