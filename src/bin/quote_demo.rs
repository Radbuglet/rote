use rote::{quote::rote, token::Directive};

#[rustfmt::skip]
fn main() {
    println!("{}.", rote! {
		pub struct Whee<'a> {
			ceci
			est un meme
			"whee? sqfklsj"
			woo
		}
	});
}

#[derive(Debug, Clone)]
pub struct MyDirective;

impl Directive for MyDirective {}
