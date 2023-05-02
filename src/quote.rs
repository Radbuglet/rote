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
        column_margin: u32,
    }

    impl GroupBuilder {
        pub fn new(delimiter: GroupDelimiter) -> Self {
            Self {
                group: TokenGroup::new(delimiter, GroupMargin::INERT, []),
                last_line: u32::MAX,
                last_column: u32::MAX,
                column_margin: u32::MAX,
            }
        }

        pub fn with_warped_cursor(mut self, line: u32, column: u32) -> Self {
            self.last_line = line;
            self.last_column = column;
            if self.column_margin == u32::MAX {
                self.column_margin = column;
            }

            self
        }

        pub fn with_moved_cursor(mut self, line: u32, column: u32) -> Self {
            // Warp cursor to first position
            if self.last_line == u32::MAX {
                self.last_line = line;
                self.last_column = column;
                self.column_margin = column;
            }

            // Insert relative cursor spacing
            const BACKWARDS_ERR: &'static str =
                "The location of the token being quoted has seem to have gone backwards. \
				 Was the first line of the quote not at the minimum indentation level? \
				 Is `rote!` mixing spans?";

            let delta_line = line.checked_sub(self.last_line).expect(BACKWARDS_ERR);

            if delta_line > 0 {
                let column = column.checked_sub(self.column_margin).expect(BACKWARDS_ERR);
                self.group.push_token(TokenSpacing::new(delta_line, column));
            } else {
                let delta_column = column.checked_sub(self.last_column).expect(BACKWARDS_ERR);
                if delta_column > 0 {
                    self.group
                        .push_token(TokenSpacing::new_spaces(delta_column));
                }
            }

            self
        }

        pub fn with_token(mut self, token: impl Into<Token>) -> Self {
            self.group.push_token(token.into());
            self
        }

        pub fn finish(self) -> Token {
            self.group.into()
        }
    }
}

pub use rote_macros::rote;
