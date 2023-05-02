use rote::quote::rote;

fn main() {
    dbg!(rote! {
        #[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
        pub struct Vec3N<'abc, 'd___ef,,##,> {

        }
    });
}
