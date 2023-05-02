use rote::{quote::rote, token::Directive};

#[rustfmt::skip]
fn main() {
    dbg!(rote! {
		#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
		pub struct Whee<'a> {
			$${rote! {
				woo {
					waz
				}
			}}
			${MyDirective}
		}
    });
}

#[derive(Debug, Clone)]
pub struct MyDirective;

impl Directive for MyDirective {}
