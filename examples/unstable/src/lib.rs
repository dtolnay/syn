#![feature(core_intrinsics, proc_macro_diagnostic)]

extern crate proc_macro;
extern crate syn;

use proc_macro::{Diagnostic, Span, TokenStream};
use syn::buffer::{Cursor, TokenBuffer};
use syn::spanned::Spanned;
use syn::synom::Synom;
use syn::{token, ExprTuple};

struct Parser {
    cursor: Cursor<'static>,

    #[allow(dead_code)]
    buffer: Box<TokenBuffer>,
}

impl Parser {
    fn new(tokens: TokenStream) -> Parser {
        let buffer = Box::new(TokenBuffer::new(tokens));
        let cursor = unsafe {
            let buffer: *const TokenBuffer = &*buffer;
            let buffer: &'static TokenBuffer = &*buffer;
            buffer.begin()
        };

        Parser { cursor, buffer }
    }

    fn current_span(&self) -> Span {
        self.cursor.span().unstable()
    }

    fn parse<T: Synom>(&mut self) -> Result<T, Diagnostic> {
        let (val, cursor) = T::parse(self.cursor).map_err(|e| {
            let expected = match T::description() {
                Some(desc) => desc,
                None => unsafe { std::intrinsics::type_name::<T>() },
            };

            self.current_span()
                .error(format!("{}: expected {}", e, expected))
        })?;

        self.cursor = cursor;
        Ok(val)
    }

    fn eof(&mut self) -> Result<(), Diagnostic> {
        if !self.cursor.eof() {
            return Err(self
                .current_span()
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
        let diag = b
            .span()
            .unstable()
            .error(format!("expected {} element(s), got {}", a_len, b_len))
            .span_note(a.span().unstable(), "because of this");

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
