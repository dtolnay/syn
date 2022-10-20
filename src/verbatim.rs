use crate::parse::{ParseBuffer, ParseStream};
use proc_macro2::{Delimiter, TokenStream};
use std::iter;

pub fn between<'a>(begin: ParseBuffer<'a>, end: ParseStream<'a>) -> TokenStream {
    let end = end.cursor();
    let mut cursor = begin.cursor();
    let mut tokens = TokenStream::new();
    while cursor != end {
        let (tt, next) = cursor.token_tree().unwrap();

        if next > end {
            // In some edge cases, a syntax node can actually cross the border
            // of a None-delimited group, due to such groups being transparent
            // to the parser in most cases. In the cases that this can occur,
            // the presence of the group is known to be semantically irrelevant,
            // so we should just ignore the presence of the group. (Issue #1235)
            if let Some((inside, _span, after)) = cursor.group(Delimiter::None) {
                assert!(next == after);
                cursor = inside;
                continue;
            } else {
                panic!("verbatim end must not be inside a delimited group");
            }
        }

        tokens.extend(iter::once(tt));
        cursor = next;
    }
    tokens
}
