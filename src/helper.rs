#![cfg(feature = "parsing")]

use nom::{self, IResult};

macro_rules! punct {
    ($i:expr, $punct:expr) => {
        $crate::helper::punct($i, $punct)
    };
}

pub fn punct<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, &'a str> {
    let mut chars = input.char_indices();
    while let Some((i, ch)) = chars.next() {
        if !ch.is_whitespace() {
            return if input[i..].starts_with(token) {
                let end = i + token.len();
                IResult::Done(&input[end..], &input[i..end])
            } else {
                IResult::Error(nom::Err::Position(nom::ErrorKind::TagStr, input))
            };
        }
    }
    IResult::Error(nom::Err::Position(nom::ErrorKind::TagStr, input))
}

macro_rules! option (
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        match $submac!($i, $($args)*) {
            ::nom::IResult::Done(i, o) => ::nom::IResult::Done(i, Some(o)),
            ::nom::IResult::Error(_) => ::nom::IResult::Done($i, None),
            ::nom::IResult::Incomplete(_) => ::nom::IResult::Done($i, None),
        }
    });
    ($i:expr, $f:expr) => (
        option!($i, call!($f));
    );
);

macro_rules! opt_vec (
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        match $submac!($i, $($args)*) {
            ::nom::IResult::Done(i, o) => ::nom::IResult::Done(i, o),
            ::nom::IResult::Error(_) => ::nom::IResult::Done($i, Vec::new()),
            ::nom::IResult::Incomplete(_) => ::nom::IResult::Done($i, Vec::new()),
        }
    });
);

macro_rules! epsilon {
    ($i:expr,) => {
        call!($i, {
            fn epsilon<T>(input: T) -> ::nom::IResult<T, ()> {
                ::nom::IResult::Done(input, ())
            }
            epsilon
        })
    };
}
