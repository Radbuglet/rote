use proc_macro::TokenStream as NativeTokenStream;
use proc_macro2::{Delimiter, Group, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned};

#[proc_macro]
pub fn rote(input: NativeTokenStream) -> NativeTokenStream {
    let crate_ = quote!(::rote);

    make_group_builder(
        &crate_,
        input.into(),
        quote! {
            #crate_::SourceTree::new_root(#crate_::SourceTreeDelimiter::Virtual)
        },
    )
    .into()
}

fn make_group_builder(
    crate_: &TokenStream,
    input: TokenStream,
    output_prelude: TokenStream,
) -> TokenStream {
    let mut output = output_prelude;

    let token_pos_ctor = |span| {
        let crate_ = re_span(crate_.clone(), span);
        quote_spanned! { span =>
            #crate_::TokenPos::new(#crate_::macro_internals::line!(), #crate_::macro_internals::column!())
        }
    };

    // For every token...
    let mut input = input.into_iter().peekable();

    while let Some(first) = input.next() {
        let sub_token = match first {
            TokenTree::Group(group) => {
                let start_getter = token_pos_ctor(group.span_open());
                let end_getter = token_pos_ctor(group.span_close());
                let delimiter = match group.delimiter() {
                    Delimiter::Parenthesis => quote! { Paren },
                    Delimiter::Brace => quote! { Brace },
                    Delimiter::Bracket => quote! { Bracket },
                    // These are generated by expanding macro output. They shouldn't really be part
                    // of the program output so we just treat them as invisible virtual tokens.
                    // Although this technically creates a new formatting context, this is fine
                    // because we're still internally consistent.
                    Delimiter::None => quote! { Virtual },
                };

                make_group_builder(
                    crate_,
                    group.stream(),
                    quote! {
                        #crate_::SourceTree::new(
                            #start_getter, #end_getter,
                            #crate_::SourceTreeDelimiter::#delimiter,
                        )
                    },
                )
            }
            TokenTree::Ident(ident) => {
                let start_getter = token_pos_ctor(ident.span());
                let ident_text = ident.to_string();
                quote! { #crate_::SourceIdent::new(#start_getter, #ident_text) }
            }
            TokenTree::Punct(punct) => 'a: {
                let start_getter = token_pos_ctor(punct.span());

                // Try to parse this as either a directive or an embedding.
                if punct.as_char() == '$' {
                    match input.peek() {
                        // We are an embedding
                        Some(TokenTree::Punct(punct)) if punct.as_char() == '$' => {
                            input.next(); // Consume the second `$`
                            let embedded = input.next(); // Consume the embedded token target, whatever it may be.

                            // FIXME: This embedding will not be given a proper location.
                            break 'a quote! { #embedded };
                        }

                        // We are a group-formed directive
                        Some(TokenTree::Group(directive))
                            if matches!(
                                directive.delimiter(),
                                Delimiter::Parenthesis | Delimiter::Brace | Delimiter::None,
                            ) =>
                        {
                            let directive = directive.clone();
                            let end_getter = token_pos_ctor(directive.span_close());

                            input.next(); // Consume the directive group

                            let crate_ = re_span(crate_.clone(), directive.span());
                            break 'a quote_spanned! { directive.span() =>
                                #crate_::SourceDirective::new(#start_getter, #end_getter, #directive)
                            };
                        }

                        // We are a "path + arguments" directives
                        // TODO: Implement

                        // We are just a regular `$`
                        _ => { /* fallthrough */ }
                    }
                }

                let ident_char = punct.as_char();
                quote! { #crate_::SourcePunct::new(#start_getter, #ident_char) }
            }
            TokenTree::Literal(literal) => {
                let start_getter = token_pos_ctor(literal.span());
                let text = literal.to_string();
                quote! { #crate_::SourceRawText::new(#start_getter, #text) }
            }
        };

        output.extend(quote! { .with(#sub_token) });
    }

    // Wrap in a `Source` constructor
    output = quote! { #crate_::Source::Tree(#output) };

    output
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
