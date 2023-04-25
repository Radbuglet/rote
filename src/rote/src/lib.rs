use std::borrow::Cow;

pub type Rote = Cow<'static, str>;

#[doc(hidden)]
pub mod macro_internals {
    // Re-exports
    pub use core::{column, line};
    use std::cell::RefCell;

    // Builder
    #[derive(Debug)]
    pub struct Builder(RefCell<BuilderInner>);

    #[derive(Debug)]
    struct BuilderInner {
        builder: String,
        base_column: u32,
        last_line: u32,
        last_column: u32,
    }

    impl Builder {
        pub fn new() -> Self {
            Self(RefCell::new(BuilderInner {
                builder: String::new(),
                base_column: u32::MAX,
                last_line: 0,
                last_column: 0,
            }))
        }

        pub fn write<D>(&self, elem: impl BuilderElem<D>) -> &Self {
            let me = &mut *self.0.borrow_mut();
            elem.append_to_builder(&mut me.builder, me.last_column - me.base_column);
            self
        }

        pub fn write_macro<'a, D>(&self, line: u32, column: u32, elem: impl BuilderElem<D>) {
            let me = &mut *self.0.borrow_mut();

            // If this is the first token, set our margin.
            if me.base_column == u32::MAX {
                me.base_column = column;
                me.last_line = line;
                me.last_column = column;
            }

            // Add necessary spacing
            match elem.mode() {
                BuilderElemMode::Align => {
                    let delta_line = line.checked_sub(me.last_line).expect(
						"Line numbers somehow went backwards. Was a `rote` macro produced by another macro \
						expansion without proper attention to spans?",
					);
                    me.last_line = line;

                    if delta_line > 0 {
                        // Push newline and align marker
                        me.builder.push('\n');
                        me.builder
                            .extend((0..(column - me.base_column)).map(|_| ' '));
                    } else {
                        // Align marker
                        let delta_column = column.checked_sub(me.last_column).expect(
						"Column numbers somehow went backwards. Was the first token provided at the lowest \
						indentation level? Was a `rote` macro produced by another macro expansion without \
						proper attention to spans?"
					);
                        me.builder.extend((0..delta_column).map(|_| ' '));
                    }
                    me.last_column = column;
                }
                BuilderElemMode::Jump => {
                    me.last_line = line;
                    me.last_column = column;
                }
            }

            // Update the buffer
            let old_len = me.builder.len();
            elem.append_to_builder(&mut me.builder, me.last_column - me.base_column);
            me.last_column += (me.builder.len() - old_len) as u32;
        }

        pub fn finish(self) -> String {
            self.0.into_inner().builder
        }
    }

    // === BuilderElem === //

    // Traits
    #[derive(Debug, Copy, Clone)]
    pub enum BuilderElemMode {
        Align,
        Jump,
    }

    pub trait BuilderElem<Disambiguator> {
        fn mode(&self) -> BuilderElemMode;

        fn append_to_builder(self, target: &mut String, leading_spaces: u32);
    }

    // Core impls
    pub struct CoreDisambiguator;

    impl<T: ToString> BuilderElem<CoreDisambiguator> for T {
        fn mode(&self) -> BuilderElemMode {
            BuilderElemMode::Align
        }

        fn append_to_builder(self, target: &mut String, leading_spaces: u32) {
            for char in self.to_string().chars() {
                target.push(char);
                // Rust normalizes newlines to `LF` when stringifying tokens.
                if char == '\n' {
                    target.extend((0..leading_spaces).map(|_| ' '));
                }
            }
        }
    }

    pub struct BuilderJumpInstr;

    impl BuilderElem<CoreDisambiguator> for BuilderJumpInstr {
        fn mode(&self) -> BuilderElemMode {
            BuilderElemMode::Jump
        }

        fn append_to_builder(self, _: &mut String, _: u32) {}
    }

    // Iter impls
    pub struct IterDisambiguator;

    impl<T: IntoIterator<Item = String>> BuilderElem<IterDisambiguator> for T {
        fn mode(&self) -> BuilderElemMode {
            BuilderElemMode::Align
        }

        fn append_to_builder(self, target: &mut String, leading_spaces: u32) {
            for elem in self {
                elem.append_to_builder(target, leading_spaces);
            }
        }
    }

    // Special impls
    pub struct SpecialDisambiguator;

    impl BuilderElem<SpecialDisambiguator> for () {
        fn mode(&self) -> BuilderElemMode {
            BuilderElemMode::Align
        }

        fn append_to_builder(self, _target: &mut String, _leading_spaces: u32) {}
    }
}

pub use rote_macro::rote;
