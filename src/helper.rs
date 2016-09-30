#![cfg(feature = "parsing")]

use nom::{IResult, multispace};
use unicode_xid::UnicodeXID;

macro_rules! punct {
    ($i:expr, $punct:expr) => {
        $crate::helper::punct($i, $punct)
    };
}

pub fn punct<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, &'a str> {
    for (i, ch) in input.char_indices() {
        if !ch.is_whitespace() {
            return if input[i..].starts_with(token) {
                IResult::Done(&input[i + token.len()..], token)
            } else {
                IResult::Error
            };
        }
    }
    IResult::Error
}

macro_rules! keyword {
    ($i:expr, $keyword:expr) => {
        $crate::helper::keyword($i, $keyword)
    };
}

pub fn keyword<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, &'a str> {
    match punct(input, token) {
        IResult::Done(rest, _) => {
            match word_break(rest) {
                IResult::Done(_, _) => IResult::Done(rest, token),
                IResult::Error => IResult::Error,
            }
        }
        IResult::Error => IResult::Error,
    }
}

pub fn word_break<'a>(input: &'a str) -> IResult<&'a str, ()> {
    match input.chars().next() {
        Some(ch) if UnicodeXID::is_xid_continue(ch) => {
            IResult::Error
        }
        Some(_) | None => {
            IResult::Done(input, ())
        }
    }
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

pub fn eat_spaces(input: &str) -> &str {
    match multispace(input) {
        IResult::Done(rest, _) => rest,
        IResult::Error => input,
    }
}
