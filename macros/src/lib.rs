use proc_macro::TokenStream as NativeTokenStream;
use proc_macro2::{Delimiter, Group, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned};

#[proc_macro]
pub fn rote(input: NativeTokenStream) -> NativeTokenStream {
    let crate_ = quote!(::rote::quote::macro_internals);
    let mut output = make_group_builder(
        &crate_,
        input.into(),
        quote! {
            #crate_::GroupBuilder::new(#crate_::GroupDelimiter::Virtual)
        },
    );
    output.extend(quote! { .finish() });

    output.into()
}

fn make_group_builder(
    crate_: &TokenStream,
    input: TokenStream,
    output_prelude: TokenStream,
) -> TokenStream {
    let mut main_group = output_prelude;

    let token_pos_ctor = |span, line_offset: u32, column_offset: u32| {
        let crate_ = re_span(crate_.clone(), span);

        quote_spanned! { span =>
            #crate_::line!() + #line_offset, #crate_::column!() + #column_offset
        }
    };

    // For every token...
    let mut input = input.into_iter();

    while let Some(first) = input.next() {
        match first {
            // If we find a quoted group
            TokenTree::Group(group) => {
                // Determine the delimiter locations
                let start_getter = token_pos_ctor(group.span_open(), 0, 0);
                let pre_end_getter = token_pos_ctor(group.span_close(), 0, 0);
                let end_getter = token_pos_ctor(group.span_close(), 0, 1);

                // Create a builder for the sub-group
                let delimiter = match group.delimiter() {
                    Delimiter::Parenthesis => quote! { Parenthesis },
                    Delimiter::Brace => quote! { Brace },
                    Delimiter::Bracket => quote! { Bracket },
                    // These are generated by expanding macro output. They shouldn't really be part
                    // of the program output so we just treat them as invisible virtual tokens.
                    // Although this technically creates a new formatting context, this is fine
                    // because we're still internally consistent.
                    Delimiter::None => quote! { Virtual },
                };

                let mut sub_group = make_group_builder(
                    crate_,
                    group.stream(),
                    quote! {
                        #crate_::GroupBuilder::new(#crate_::GroupDelimiter::#delimiter)
                            // Align our sub group cursor to the start delimiter
                            .with_warped_cursor(#start_getter)
                    },
                );
                sub_group.extend(quote! {
                    // Pad our sub group cursor to right before the end delimiter
                    .with_moved_cursor(#pre_end_getter)
                    // Transform our subgroup into a token
                    .finish()
                });

                // Add our subgroup to the main group
                main_group.extend(quote! {
                    // Pad our main group up to the start location
                    .with_moved_cursor(#start_getter)
                    // Add our subgroup
                    .with_token(#sub_group)
                    // Align our main group cursor to the end delimiter
                    .with_warped_cursor(#end_getter)
                });
            }
            TokenTree::Ident(ident) => {
                let ident_text = ident.to_string();

                // Determine the start and end of the identifier
                let start_getter = token_pos_ctor(ident.span(), 0, 0);
                let end_getter = token_pos_ctor(ident.span(), 0, ident_text.len() as u32);

                main_group.extend(quote! {
                    // Pad our main group up to the identifier start
                    .with_moved_cursor(#start_getter)
                    // Add our identifier to the main group
                    .with_token(#crate_::TokenIdent::new(#ident_text))
                    // Warp to the end of the identifier
                    .with_warped_cursor(#end_getter)
                });
            }
            TokenTree::Punct(punct) => 'a: {
                // Determine the start of the punct (sequence?)
                let start_getter = token_pos_ctor(punct.span(), 0, 0);

                // Handle lifetime in a special way because the `token_pos_ctor` for the following
                // identifier gets the span of the start of the lifetime.
                // FIXME: Is this behavior intentional?
                if punct.as_char() == '\'' {
                    if let Some(TokenTree::Ident(ident)) = input.clone().next() {
                        let ident_text = ident.to_string();
                        let end_getter =
                            token_pos_ctor(punct.span(), 0, ident.to_string().len() as u32 + 1);

                        let _ = input.next();

                        main_group.extend(quote! {
                            .with_moved_cursor(#start_getter)
                            .with_token(#crate_::TokenPunct::new('\''))
                            .with_token(#crate_::TokenIdent::new(#ident_text))
                            .with_warped_cursor(#end_getter)
                        });

                        break 'a;
                    }
                }

                // Try to parse this as either a directive or an embedding.
                if punct.as_char() == '$' {
                    // Fork our input
                    let mut input_fork = input.clone();

                    // Attempt to parse this input
                    match input_fork.next() {
                        // We are an embedding
                        Some(TokenTree::Punct(punct)) if punct.as_char() == '$' => {
                            // Consume the embedded group
                            if let Some(TokenTree::Group(embedded)) = input_fork.next() {
                                // Build the main group extension
                                let end_getter = token_pos_ctor(embedded.span_close(), 0, 0);

                                main_group.extend(quote! {
                                    .with_moved_cursor(#start_getter)
                                    .with_token(#embedded)
                                    .with_warped_cursor(#end_getter)
                                });

                                // Apply the successful parse fork and break
                                input = input_fork;
                                break 'a;
                            }
                        }

                        // We are a group-formed directive
                        Some(TokenTree::Group(directive))
                            if matches!(
                                directive.delimiter(),
                                Delimiter::Parenthesis | Delimiter::Brace | Delimiter::None,
                            ) =>
                        {
                            // Build the main group extension
                            let end_getter = token_pos_ctor(directive.span_close(), 0, 0);
                            let crate_ = re_span(crate_.clone(), directive.span());
                            let directive_inner = directive.stream();

                            main_group.extend(quote_spanned! { directive.span() =>
                                .with_moved_cursor(#start_getter)
                                .with_token(#crate_::TokenDirective::new(#directive_inner))
                                .with_warped_cursor(#end_getter)
                            });

                            // Apply the successful parse fork and break
                            input = input_fork;
                            break 'a;
                        }

                        // We are a "path + arguments" directives
                        // TODO: Implement

                        // We are just a regular `$`
                        _ => { /* fallthrough */ }
                    }
                }

                // Otherwise, emit this as just another token
                let end_getter = token_pos_ctor(punct.span(), 0, 1);
                let ident_char = punct.as_char();

                main_group.extend(quote! {
                    .with_moved_cursor(#start_getter)
                    .with_token(#crate_::TokenPunct::new(#ident_char))
                    .with_warped_cursor(#end_getter)
                });
            }
            TokenTree::Literal(literal) => {
                let source = literal.to_string();
                if !source.is_empty() {
                    let start_getter = token_pos_ctor(literal.span(), 0, 0);
                    let end_getter = token_pos_ctor(
                        literal.span(),
                        source.lines().count() as u32 - 1,
                        source.lines().last().unwrap().len() as u32,
                    );

                    main_group.extend(quote! {
                        .with_moved_cursor(#start_getter)
                        .with_token(#crate_::TokenLiteral::from_quote(#source))
                        .with_warped_cursor(#end_getter)
                    });
                }
            }
        };
    }

    main_group
}

fn re_span(stream: TokenStream, span: Span) -> TokenStream {
    TokenStream::from_iter(stream.into_iter().map(|token| match token {
        TokenTree::Group(old_group) => {
            let mut group = Group::new(old_group.delimiter(), re_span(old_group.stream(), span));
            group.set_span(span);
            TokenTree::Group(group)
        }
        mut token => {
            token.set_span(span);
            token
        }
    }))
}
