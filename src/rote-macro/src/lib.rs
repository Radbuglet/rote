use proc_macro::TokenStream as NativeTokenStream;
use proc_macro2::{Delimiter, Ident, Spacing, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned};

#[proc_macro]
pub fn rote(input: NativeTokenStream) -> NativeTokenStream {
    let input = TokenStream::from(input);

    // Emit output header
    let f_builder = Ident::new("builder", Span::call_site());
    let output = quote_spanned! {Span::call_site()=>
        let #f_builder = ::rote::macro_internals::Builder::new();
    };

    // Recurse through the tree to collect literals and quasi-quotes
    let mut output = output;
    let mut push = |start_span: Span, f_text_getter: TokenStream, offset_by: u32| {
        let f_line_getter = quote_spanned! { start_span => ::rote::macro_internals::line!() };
        let f_column_getter =
            quote_spanned! { start_span => ::rote::macro_internals::column!() + #offset_by };

        output.extend(quote! {
            #[allow(unused_braces)]
            #f_builder.write_macro(#f_line_getter, #f_column_getter, #f_text_getter);
        });
    };

    fn recurse(stream: TokenStream, push: &mut impl FnMut(Span, TokenStream, u32)) {
        let mut stream = stream.into_iter().peekable();

        while let Some(first_node) = stream.next() {
            // Try to parse this as a quasi-quote
            let quasi = 'a: {
                // If this is a punct...
                let TokenTree::Punct(first_punct) = first_node.clone() else {
					break 'a None;
				};

                // and that punct is `$`...
                if first_punct.as_char() != '$' {
                    break 'a None;
                }

                // Parse the subsequent character
                match stream.peek() {
                    // If we're something to quasi-quote, yield that.
                    Some(TokenTree::Group(_)) => {
                        Some(TokenStream::from_iter([stream.next().unwrap()]))
                    }
                    Some(TokenTree::Ident(ident)) => {
                        let quasi = quote_spanned! { ident.span() => &#ident };
                        stream.next();
                        Some(quasi)
                    }
                    Some(TokenTree::Literal(literal)) => {
                        let quasi = quote_spanned! { literal.span() => #literal };
                        stream.next();
                        Some(quasi)
                    }
                    // Otherwise, handle the escaped `$` but otherwise treat the `$` as a literal and
                    // let its subsequent character be handled independently.
                    Some(TokenTree::Punct(punct)) => {
                        if punct.as_char() == '$' && punct.spacing() == Spacing::Joint {
                            stream.next();
                        }
                        None
                    }
                    None => None,
                }
            };

            // Process the parsed unit
            match quasi {
                Some(quasi_getter) => {
                    // Push the quasi-quoter begin location
                    push(first_node.span(), quote! { "" }, 0);

                    // Push the quasi-quoter
                    push(first_node.span(), quasi_getter.clone(), 0);

                    // Push the end location padding
                    let last_quasi = quasi_getter.into_iter().last().unwrap();
                    push(
                        last_quasi.span(),
                        quote! { ::rote::macro_internals::BuilderJumpInstr },
                        last_quasi.to_string().len() as _,
                    );
                }
                None => match first_node {
                    TokenTree::Group(group) => {
                        let (open_delim, close_delim) = match group.delimiter() {
                            Delimiter::Parenthesis => ("(", ")"),
                            Delimiter::Brace => ("{", "}"),
                            Delimiter::Bracket => ("[", "]"),
                            Delimiter::None => ("", ""),
                        };
                        push(group.span_open(), quote! { #open_delim }, 0);
                        recurse(group.stream(), push);
                        push(group.span_close(), quote! { #close_delim }, 0);
                    }
                    first_node => {
                        let text = first_node.to_string();
                        push(first_node.span(), quote! { #text }, 0);
                    }
                },
            }
        }
    }

    recurse(input, &mut push);

    // Emit the output footer
    output.extend(quote! {
        #f_builder.finish()
    });
    let output = quote! {{ #output }};

    // Yield to the consumer
    output.into()
}
