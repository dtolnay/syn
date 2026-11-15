use crate::buffer::Cursor;
use crate::error::Result;
use crate::ext::{IdentExt as _, TokenStreamExt as _};
use crate::ident::parsing::accept_as_ident;
use crate::parse::ParseStream;
use crate::token;
use alloc::string::ToString;
use core::cmp::Ordering;
use proc_macro2::{Delimiter, Ident, TokenStream};

pub(crate) fn between(begin: Cursor, end: Cursor) -> TokenStream {
    let mut cursor = begin;
    assert!(crate::buffer::same_buffer(end, cursor));

    let mut tokens = TokenStream::new();
    while cursor != end {
        let (tt, next) = cursor.token_tree().unwrap();

        if crate::buffer::cmp_assuming_same_buffer(end, next) == Ordering::Less {
            // A syntax node can cross the boundary of a None-delimited group
            // due to such groups being transparent to the parser in most cases.
            // Any time this occurs the group is known to be semantically
            // irrelevant. https://github.com/dtolnay/syn/issues/1235
            if let Some((inside, _span, after)) = cursor.group(Delimiter::None) {
                assert!(next == after);
                cursor = inside;
                continue;
            } else {
                panic!("verbatim end must not be inside a delimited group");
            }
        }

        tokens.append(tt);
        cursor = next;
    }
    tokens
}

pub(crate) fn peek_builtin(input: ParseStream) -> bool {
    let Some((ident, _)) = input.cursor().ident() else {
        return false;
    };
    if input.peek2(Token![#]) {
        return ident == "builtin";
    }
    // NOTE: `k#foo` is passed as one identifier.
    input.peek2(token::Paren)
        && ident
            .to_string()
            .strip_prefix("k#")
            .is_some_and(accept_as_ident)
}

pub(crate) fn parse_builtin(begin: Cursor, input: ParseStream) -> Result<TokenStream> {
    if input.call(Ident::parse_any)? == "builtin" {
        input.parse::<Token![#]>()?;
        input.parse::<Ident>()?;
    }
    let args;
    parenthesized!(args in input);
    args.parse::<TokenStream>()?;
    Ok(between(begin, input.cursor()))
}
