use rote::{
    quote::rote,
    token::{GroupMargin, Token, TokenGroup, TokenIdent, TokenPunct, TokenSpacing},
};

fn main() {
    println!("{}", vector(3, CompTy::I32));
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum CompTy {
    I32,
    U32,
    F32,
    F64,
}

impl CompTy {
    pub fn prefix(&self) -> &str {
        match self {
            CompTy::I32 => "I",
            CompTy::U32 => "U",
            CompTy::F32 => "",
            CompTy::F64 => "D",
        }
    }

    pub fn is_floating(&self) -> bool {
        matches!(self, Self::F32 | Self::F64)
    }

    pub fn display(&self) -> Token {
        TokenIdent::new(match self {
            CompTy::I32 => "i32",
            CompTy::U32 => "u32",
            CompTy::F32 => "f32",
            CompTy::F64 => "f64",
        })
        .to_token()
    }
}

#[allow(non_snake_case)]
#[allow(unused_braces)]
pub fn vector(N: usize, CompTy: CompTy) -> Token {
    let VecTy = TokenIdent::new(format!("{}Vec{}", CompTy.prefix(), N));
    let ALPHA = ["x", "y", "z", "w"];

    rote! {
        $${if CompTy.is_floating() {
            rote! { #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Default)] }
        } else {
            rote! { #[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Default)] }
        }}
        pub struct $${VecTy.clone()} {
            $${{
                let mut builder = TokenGroup::new_virtual(GroupMargin::AT_LINE);
                for i in 0..N {
                    builder.push_raw(rote! { pub $${TokenIdent::new(ALPHA[i])}: $${CompTy.display()}, });
                    if i != N - 1 {
                        builder.push_raw(TokenSpacing::NEWLINE);
                    }
                }

                builder
            }}
        }

        impl $${VecTy.clone()} {
            pub const fn new($${
                let mut builder = TokenGroup::new_virtual(GroupMargin::AT_CURSOR);
                for i in 0..N {
                    builder.push_raw(rote! { $${TokenIdent::new(ALPHA[i])}: $${CompTy.display()} });
                    if i != N - 1 {
                        builder.push_raw(TokenPunct::new(','));
                        builder.push_raw(TokenSpacing::SPACE);
                    }
                }

                builder
            }) -> Self {
                Self { $${
                    let mut builder = TokenGroup::new_virtual(GroupMargin::AT_CURSOR);
                    for i in 0..N {
                        builder.push_raw(rote! { $${TokenIdent::new(ALPHA[i])} });
                        if i != N - 1 {
                            builder.push_raw(TokenPunct::new(','));
                            builder.push_raw(TokenSpacing::SPACE);
                        }
                    }

                    builder
                } }
            }
        }
    }
    .to_token()
}
