use crate::token::{GroupMargin, Token};

// === rote! macro === //

#[doc(hidden)]
pub mod macro_internals {
    use crate::token::{GroupMargin, Token, TokenGroup, TokenSpacing};

    // === Re-exports === //

    pub use {
        crate::token::{GroupDelimiter, TokenDirective, TokenIdent, TokenLiteral, TokenPunct},
        std::{column, line},
    };

    // === GroupBuilder === //

    pub struct GroupBuilder {
        group: TokenGroup,
        last_line: u32,
        last_column: u32,
        first_column: u32,
        margin_column: u32,
    }

    impl GroupBuilder {
        pub fn new(delimiter: GroupDelimiter) -> Self {
            Self {
                group: {
                    // N.B. `GroupMargin::AT_CURSOR` is a placeholder that will be replaced during
                    // `.finish()`.
                    let mut group = TokenGroup::new(delimiter, GroupMargin::AT_CURSOR, []);
                    if delimiter == GroupDelimiter::Virtual {
                        // Our delimiter is `Virtual` iff we are called on the root-most group of a
                        // `rote!` invocation. In these cases, we always want to ensure that the
                        // spacing between the margin and the first token is actually visible, since
                        // this is part of the block.
                        group.set_head_spacing_visible(true);
                    }
                    group
                },
                last_line: 0,
                last_column: 0,
                first_column: u32::MAX,
                margin_column: u32::MAX,
            }
        }

        pub fn with_warped_cursor(mut self, line: u32, column: u32) -> Self {
            // Update the margin
            if column < self.margin_column {
                self.margin_column = column;
            }

            // Update the first column
            if self.first_column == u32::MAX {
                self.first_column = column;
            }

            // Warp the cursor
            self.last_line = line;
            self.last_column = column;

            self
        }

        pub fn with_moved_cursor(mut self, line: u32, column: u32) -> Self {
            // Update the margin
            if column < self.margin_column {
                self.margin_column = column;
            }

            // Warp cursor to first position
            if self.first_column == u32::MAX {
                self.last_line = line;
                self.last_column = column;
                self.first_column = column;
            }

            // Insert relative cursor spacing
            const BACKWARDS_ERR: &'static str =
                "The location of the token being quoted has seem to have gone backwards. \
                 Is `rote!` mixing spans?";

            let delta_line = line.checked_sub(self.last_line).expect(BACKWARDS_ERR);

            if delta_line > 0 {
                self.group.push_raw(TokenSpacing::new(delta_line, column));
            } else {
                let delta_column = column.checked_sub(self.last_column).expect(BACKWARDS_ERR);
                if delta_column > 0 {
                    self.group.push_raw(TokenSpacing::new_spaces(delta_column));
                }
            }

            self
        }

        pub fn with_token(mut self, token: impl Into<Token>) -> Self {
            self.group.push_raw(token.into());
            self
        }

        pub fn finish(mut self) -> TokenGroup {
            // Normalize token groups such that they don't have any common spacing in front of them.
            //
            // A desired property of token groups is that their whitespace is maximally compact.
            // That is, there shouldn't be a space-introduced margin common to every line. Rather,
            // the margin should be part of the token group's regular formatting margin. This ensures
            // that groups can be intuitively moved around without also accidentally copying
            // whitespace that logically originates from a parent group.
            {
                // Determine the head token margin spacing
                let head_spacing = self.first_column - self.margin_column;
                self.group.set_head_spacing(head_spacing);

                // Define the margin to be relative to the cursor position at the open delimiter.
                // Users can safely overwrite this later, although the position of the first token,
                // if it's not at the shared margin of the group, could change positions. This is
                // almost certainly the desired behavior, however.
                self.group
                    .set_margin(GroupMargin::RelativeToCursor(-(head_spacing as i32)));

                // Normalize line starts to the minimum margin.
                // FIXME: This clobbers raw newlines injected directly into the stream.
                for token in self.group.tokens_mut_raw() {
                    if let Token::Spacing(spacing) = token {
                        if spacing.lines() > 0 {
                            spacing.set_spaces(spacing.spaces() - self.margin_column);
                        }
                    }
                }
            }

            self.group
        }
    }
}

pub use rote_macros::rote;

// === Construction helpers === //

pub fn debug_show_margin() -> Token {
    rote! {
        $$<debug_show_margin>
        <- the margin is here!
    }
    .with_margin(GroupMargin::AT_MARGIN)
    .to_token()
}
