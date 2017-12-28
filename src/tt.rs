use proc_macro2::{TokenNode, TokenTree};
use cursor::Cursor;
use parse_error;
use synom::PResult;

#[cfg(feature = "full")]
use proc_macro2::Delimiter;

pub fn delimited(input: Cursor) -> PResult<TokenTree> {
    match input.token_tree() {
        Some((
            rest,
            token @ TokenTree {
                kind: TokenNode::Group(..),
                ..
            },
        )) => Ok((rest, token)),
        _ => parse_error(),
    }
}

#[cfg(feature = "full")]
pub fn braced(input: Cursor) -> PResult<TokenTree> {
    match input.token_tree() {
        Some((
            rest,
            token @ TokenTree {
                kind: TokenNode::Group(Delimiter::Brace, ..),
                ..
            },
        )) => Ok((rest, token)),
        _ => parse_error(),
    }
}

#[cfg(feature = "full")]
pub fn parenthesized(input: Cursor) -> PResult<TokenTree> {
    match input.token_tree() {
        Some((
            rest,
            token @ TokenTree {
                kind: TokenNode::Group(Delimiter::Parenthesis, ..),
                ..
            },
        )) => Ok((rest, token)),
        _ => parse_error(),
    }
}
