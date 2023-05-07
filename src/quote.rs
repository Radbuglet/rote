#[doc(hidden)]
pub mod quote_macro_internals {
    use crate::token::{GroupMargin, ToToken, Token, TokenGroup, TokenSpacing};

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
        needs_update: Vec<usize>,
    }

    impl GroupBuilder {
        pub fn new(delimiter: GroupDelimiter) -> Self {
            Self {
                group: {
                    // `AT_CURSOR` is just a sensible default which users can easily overwrite if
                    // need be. In particular, the macro replaces this with a margin-relative margin
                    // if the group is a literal group embedded in a parent group builder.
                    let mut group = TokenGroup::new(delimiter, GroupMargin::AT_CURSOR);
                    if delimiter == GroupDelimiter::Virtual {
                        // Our delimiter is `Virtual` *iff* we are called on the root-most group of a
                        // `wrote!` invocation. In these cases, we always want to ensure that the
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
                needs_update: Vec::new(),
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
                 Is `wrote!` mixing spans?";

            let delta_line = line.checked_sub(self.last_line).expect(BACKWARDS_ERR);

            if delta_line > 0 {
                self.push_managed(TokenSpacing::new(delta_line, column));
            } else {
                let delta_column = column.checked_sub(self.last_column).expect(BACKWARDS_ERR);
                if delta_column > 0 {
                    self.group.push_raw(TokenSpacing::new_spaces(delta_column));
                }
            }

            self
        }

        pub fn with_token(mut self, token: impl ToToken) -> Self {
            self.group.push_raw(token.to_token());
            self
        }

        pub fn push_managed(&mut self, token: impl ToToken) {
            self.needs_update.push(self.group.tokens().len());
            self.group.push_raw(token);
        }

        pub fn with_managed_group(
            mut self,
            _start_line: u32,
            start_col: u32,
            mut group: TokenGroup,
        ) -> Self {
            // Determine the absolute location of the group's margin column relative to the left of
            // the file. Because this is called by a macro, we assume that `head_spacing` has been
            // defined to be relative to the margin.
            let abs_margin_column = start_col - group.head_spacing();

            // Set the group's margin to that. Because this group is managed, this will be updated
            // later.
            group.set_margin(GroupMargin::RelativeToMargin(abs_margin_column));

            // Groups may need margins lower than we currently provide so we extend the `margin_column`
            // here if needed.
            if abs_margin_column < self.margin_column {
                self.margin_column = abs_margin_column;
            }

            self.push_managed(group);
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

                // Normalize macro-generated line starts to the minimum margin.
                let tokens = self.group.tokens_mut_raw();
                for i in self.needs_update {
                    match &mut tokens[i] {
                        // This space was produced relative to the start of the file.
                        // Let's adjust it!
                        Token::Spacing(spacing) => {
                            if spacing.lines() > 0 {
                                // N.B. this never underflows because `margin_column` is set to the
                                // minimum newline `spaces` count.
                                spacing.set_spaces(spacing.spaces() - self.margin_column);
                            }
                        }
                        // This group's margin was produced relative to the start of the file.
                        // Let's adjust it!
                        Token::Group(group) => {
                            if let GroupMargin::RelativeToMargin(old_rel) = group.margin() {
                                // N.B. this also cannot underflow because we take the minimum margin
                                // into account
                                group.set_margin(GroupMargin::RelativeToMargin(
                                    old_rel - self.margin_column,
                                ));
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }

            self.group
        }
    }
}

pub use rote_macros::wrote;
