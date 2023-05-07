use rote::{
    quote::{rote, TokenIterator},
    token::{GroupMargin, Token, TokenIdent, TokenSpacing},
};

fn main() {
    println!("===\n{}.", dbg!(vector(4, CompTy::F64)));
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
        #[repr(C)]
        pub struct $${&VecTy} {
            $${(0..N)
                .map(|i| rote! { pub $${TokenIdent::new(ALPHA[i])}: $${CompTy.display()}, })
                .sep(TokenSpacing::NEWLINE)
                .to_group(GroupMargin::AT_CURSOR)
            }
        }

        impl $${&VecTy} {
            pub const fn new($${(0..N)
                .map(|i| rote! { pub $${TokenIdent::new(ALPHA[i])}: $${CompTy.display()} })
                .sep(rote! { , }.with_raw(TokenSpacing::SPACE))
                .to_group(GroupMargin::AT_CURSOR)
            }) -> Self {
                Self { $${(0..N)
                    .map(|i| TokenIdent::new(ALPHA[i]))
                    .sep(rote! { , }.with_raw(TokenSpacing::SPACE))
                    .to_group(GroupMargin::AT_CURSOR)
                } }
            }
        }
    }
    .to_token()
}
