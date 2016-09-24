/// Literal kind.
///
/// E.g. `"foo"`, `42`, `12.34` or `bool`
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Lit {
    /// A string literal (`"foo"`)
    Str(String, StrStyle),
    /// A byte string (`b"foo"`)
    ByteStr(Vec<u8>),
    /// A byte char (`b'f'`)
    Byte(u8),
    /// A character literal (`'a'`)
    Char(char),
    /// An integer literal (`1`)
    Int(u64, IntTy),
    /// A float literal (`1f64` or `1E10f64` or `1.0E10`)
    Float(String, FloatTy),
    /// A boolean literal
    Bool(bool),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum StrStyle {
    /// A regular string, like `"foo"`
    Cooked,
    /// A raw string, like `r##"foo"##`
    ///
    /// The uint is the number of `#` symbols used
    Raw(usize)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum IntTy {
    Isize,
    I8,
    I16,
    I32,
    I64,
    Usize,
    U8,
    U16,
    U32,
    U64,
    Unsuffixed
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FloatTy {
    F32,
    F64,
    Unsuffixed,
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use escape::escaped_string;
    use nom::{IResult, multispace};

    named!(pub lit -> Lit, alt!(
        quoted => { |q| Lit::Str(q, StrStyle::Cooked) }
        // TODO: ByteStr
        // TODO: Byte
        // TODO: Char
        |
        int => { |(value, ty)| Lit::Int(value, ty) }
        // TODO: Float
        // TODO: Bool
    ));

    named!(quoted -> String, delimited!(
        punct!("\""),
        escaped_string,
        tag!("\"")
    ));

    named!(pub int -> (u64, IntTy), preceded!(
        option!(multispace),
        tuple!(
            digits,
            alt!(
                tag!("isize") => { |_| IntTy::Isize }
                |
                tag!("i8") => { |_| IntTy::I8 }
                |
                tag!("i16") => { |_| IntTy::I16 }
                |
                tag!("i32") => { |_| IntTy::I32 }
                |
                tag!("i64") => { |_| IntTy::I64 }
                |
                tag!("usize") => { |_| IntTy::Usize }
                |
                tag!("u8") => { |_| IntTy::U8 }
                |
                tag!("u16") => { |_| IntTy::U16 }
                |
                tag!("u32") => { |_| IntTy::U32 }
                |
                tag!("u64") => { |_| IntTy::U64 }
                |
                epsilon!() => { |_| IntTy::Unsuffixed }
            )
        )
    ));

    fn digits(input: &str) -> IResult<&str, u64> {
        let mut value = 0u64;
        let mut len = 0;
        let mut bytes = input.bytes().peekable();
        while let Some(&b) = bytes.peek() {
            match b {
                b'0' ... b'9' => {
                    value = match value.checked_mul(10) {
                        Some(value) => value,
                        None => return IResult::Error,
                    };
                    value = match value.checked_add((b - b'0') as u64) {
                        Some(value) => value,
                        None => return IResult::Error,
                    };
                    bytes.next();
                    len += 1;
                }
                _ => break,
            }
        }
        if len > 0 {
            IResult::Done(&input[len..], value)
        } else {
            IResult::Error
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};
    use std::fmt::{self, Display};

    impl ToTokens for Lit {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Lit::Str(ref s, StrStyle::Cooked) => s.to_tokens(tokens),
                Lit::Int(value, ty) => tokens.append(&format!("{}{}", value, ty)),
                _ => unimplemented!(),
            }
        }
    }

    impl Display for IntTy {
        fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match *self {
                IntTy::Isize => formatter.write_str("isize"),
                IntTy::I8 => formatter.write_str("i8"),
                IntTy::I16 => formatter.write_str("i16"),
                IntTy::I32 => formatter.write_str("i32"),
                IntTy::I64 => formatter.write_str("i64"),
                IntTy::Usize => formatter.write_str("usize"),
                IntTy::U8 => formatter.write_str("u8"),
                IntTy::U16 => formatter.write_str("u16"),
                IntTy::U32 => formatter.write_str("u32"),
                IntTy::U64 => formatter.write_str("u64"),
                IntTy::Unsuffixed => Ok(()),
            }
        }
    }
}
