#![feature(proc_macro, core_intrinsics)]

extern crate syn;
extern crate proc_macro;

use proc_macro::{TokenStream, Span, Diagnostic};

use syn::*;
use syn::spanned::Spanned;
use syn::synom::{Synom, Cursor, SynomBuffer};

struct Parser {
    buffer: Box<SynomBuffer>,
    cursor: Cursor<'static>,
}

impl Parser {
    fn new(tokens: TokenStream) -> Parser {
        let buffer = Box::new(SynomBuffer::new(tokens.into()));
        let cursor = unsafe {
            let buffer: &'static SynomBuffer = ::std::mem::transmute(&*buffer);
            buffer.begin()
        };

        Parser {
            buffer: buffer,
            cursor: cursor,
        }
    }

    fn current_span(&self) -> Span {
        self.cursor.current_span()
            .map(|sp| sp.into_inner())
            .unwrap_or_else(|| Span::def_site())
    }

    fn parse<T: Synom>(&mut self) -> Result<T, Diagnostic> {
        let (cursor, val) = T::parse(self.cursor)
            .map_err(|e| {
                let expected = match T::description() {
                    Some(desc) => desc,
                    None => unsafe { ::std::intrinsics::type_name::<T>() }
                };

                self.current_span().error(format!("{}: expected {}", e, expected))
            })?;

        self.cursor = cursor;
        Ok(val)
    }

    fn eof(&mut self) -> Result<(), Diagnostic> {
        if !self.cursor.is_eof() {
            return Err(self.current_span()
                       .error("trailing characters; expected eof"));
        }

        Ok(())
    }
}

fn eval(input: TokenStream) -> Result<TokenStream, Diagnostic> {
    let mut parser = Parser::new(input);

    let a = parser.parse::<ExprTuple>()?;
    parser.parse::<token::Eq>()?;
    let b = parser.parse::<ExprTuple>()?;
    parser.eof()?;

    let (a_len, b_len) = (a.elems.len(), b.elems.len());
    if a_len != b_len {
        let diag = b.span()
            .error(format!("expected {} element(s), got {}", a_len, b_len))
            .span_note(a.span(), "because of this");

        return Err(diag);
    }

    Ok("println!(\"All good!\")".parse().unwrap())
}

#[proc_macro]
pub fn demo(input: TokenStream) -> TokenStream {
    match eval(input) {
        Ok(val) => val,
        Err(diag) => {
            diag.emit();
            "".parse().unwrap()
        }
    }
}
