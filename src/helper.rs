#![cfg(feature = "parsing")]

use nom::IResult;

macro_rules! punct {
    ($i:expr, $punct:expr) => {
        $crate::helper::punct($i, $punct)
    };
}

pub fn punct<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, &'a str> {
    for (i, ch) in input.char_indices() {
        if !ch.is_whitespace() {
            return if input[i..].starts_with(token) {
                let end = i + token.len();
                IResult::Done(&input[end..], &input[i..end])
            } else {
                IResult::Error
            };
        }
    }
    IResult::Error
}

macro_rules! option {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            $crate::nom::IResult::Done(i, o) => $crate::nom::IResult::Done(i, Some(o)),
            $crate::nom::IResult::Error => $crate::nom::IResult::Done($i, None),
        }
    };

    ($i:expr, $f:expr) => {
        option!($i, call!($f));
    };
}

macro_rules! opt_vec {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            $crate::nom::IResult::Done(i, o) => $crate::nom::IResult::Done(i, o),
            $crate::nom::IResult::Error => $crate::nom::IResult::Done($i, Vec::new()),
        }
    };
}

macro_rules! epsilon {
    ($i:expr,) => {
        $crate::nom::IResult::Done($i, ())
    };
}

macro_rules! tap {
    ($i:expr, $name:ident : $submac:ident!( $($args:tt)* ) => $e:expr) => {
        match $submac!($i, $($args)*) {
            $crate::nom::IResult::Done(i, o) => {
                let $name = o;
                $e;
                $crate::nom::IResult::Done(i, ())
            }
            $crate::nom::IResult::Error => $crate::nom::IResult::Error,
        }
    };

    ($i:expr, $name:ident : $f:expr => $e:expr) => {
        tap!($i, $name: call!($f) => $e);
    };
}
