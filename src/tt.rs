use proc_macro2::{TokenNode, TokenStream, TokenTree, Delimiter};
use cursor::Cursor;
use parse_error;
use synom::PResult;
use MacroDelimiter;
use token::{Paren, Brace, Bracket};

pub fn delimited(input: Cursor) -> PResult<(MacroDelimiter, TokenStream)> {
    match input.token_tree() {
        Some((
            rest,
            TokenTree {
                span,
                kind: TokenNode::Group(delimiter, tts),
            },
        )) => {
            let delimiter = match delimiter {
                Delimiter::Parenthesis => MacroDelimiter::Paren(Paren(span)),
                Delimiter::Brace => MacroDelimiter::Brace(Brace(span)),
                Delimiter::Bracket => MacroDelimiter::Bracket(Bracket(span)),
                Delimiter::None => return parse_error(),
            };
            Ok((rest, (delimiter, tts)))
        }
        _ => parse_error(),
    }
}

#[cfg(feature = "full")]
pub fn braced(input: Cursor) -> PResult<(Brace, TokenStream)> {
    match input.token_tree() {
        Some((
            rest,
            TokenTree {
                span,
                kind: TokenNode::Group(Delimiter::Brace, tts),
            },
        )) => Ok((rest, (Brace(span), tts))),
        _ => parse_error(),
    }
}

#[cfg(feature = "full")]
pub fn parenthesized(input: Cursor) -> PResult<(Paren, TokenStream)> {
    match input.token_tree() {
        Some((
            rest,
            TokenTree {
                span,
                kind: TokenNode::Group(Delimiter::Parenthesis, tts),
            },
        )) => Ok((rest, (Paren(span), tts))),
        _ => parse_error(),
    }
}
