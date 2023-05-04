use rote::{
    quote::rote,
    token::{debug_show_margin, Token},
};

fn main() {
    println!(
        "===\n{}.",
        dbg!(Token::from(rote! {
                whee
            woo
            $${debug_show_margin()}
                {example 'a}
                $${debug_show_margin()}
                The list starts here: START=>$${rote! {
                    1. this
                    2. is
                    3. a
                    4. multiline example!
                    $${debug_show_margin()}
                }}<=END
                $${debug_show_margin()}
            margin
        }))
    );
}
