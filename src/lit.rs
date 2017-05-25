/// Literal kind.
///
/// E.g. `"foo"`, `42`, `12.34` or `bool`
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Lit {
    /// A string literal (`"foo"`)
    Str(String, StrStyle),
    /// A byte string (`b"foo"`)
    ByteStr(Vec<u8>, StrStyle),
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

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum StrStyle {
    /// A regular string, like `"foo"`
    Cooked,
    /// A raw string, like `r##"foo"##`
    ///
    /// The uint is the number of `#` symbols used
    Raw(usize),
}

impl From<String> for Lit {
    fn from(input: String) -> Lit {
        Lit::Str(input, StrStyle::Cooked)
    }
}

impl<'a> From<&'a str> for Lit {
    fn from(input: &str) -> Lit {
        Lit::Str(input.into(), StrStyle::Cooked)
    }
}

impl From<Vec<u8>> for Lit {
    fn from(input: Vec<u8>) -> Lit {
        Lit::ByteStr(input, StrStyle::Cooked)
    }
}

impl<'a> From<&'a [u8]> for Lit {
    fn from(input: &[u8]) -> Lit {
        Lit::ByteStr(input.into(), StrStyle::Cooked)
    }
}

impl From<char> for Lit {
    fn from(input: char) -> Lit {
        Lit::Char(input)
    }
}

impl From<bool> for Lit {
    fn from(input: bool) -> Lit {
        Lit::Bool(input)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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
    Unsuffixed,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum FloatTy {
    F32,
    F64,
    Unsuffixed,
}

macro_rules! impl_from_for_lit {
    (Int, [$($rust_type:ty => $syn_type:expr),+]) => {
        $(
            impl From<$rust_type> for Lit {
                fn from(input: $rust_type) -> Lit {
                    Lit::Int(input as u64, $syn_type)
                }
            }
        )+
    };
    (Float, [$($rust_type:ty => $syn_type:expr),+]) => {
        $(
            impl From<$rust_type> for Lit {
                fn from(input: $rust_type) -> Lit {
                    Lit::Float(format!("{}", input), $syn_type)
                }
            }
        )+
    };
}

impl_from_for_lit! {Int, [
    isize => IntTy::Isize,
    i8 => IntTy::I8,
    i16 => IntTy::I16,
    i32 => IntTy::I32,
    i64 => IntTy::I64,
    usize => IntTy::Usize,
    u8 => IntTy::U8,
    u16 => IntTy::U16,
    u32 => IntTy::U32,
    u64 => IntTy::U64
]}

impl_from_for_lit! {Float, [
    f32 => FloatTy::F32,
    f64 => FloatTy::F64
]}

#[cfg(feature = "parsing")]
#[derive(Debug, Clone)]
pub struct StrLit {
    pub value: String,
    pub style: StrStyle,
}

#[cfg(feature = "parsing")]
#[derive(Debug, Clone)]
pub struct ByteStrLit {
    pub value: Vec<u8>,
    pub style: StrStyle,
}

#[cfg(feature = "parsing")]
#[derive(Debug, Clone)]
pub struct IntLit {
    pub value: u64,
    pub suffix: IntTy,
}

#[cfg(feature = "parsing")]
#[derive(Debug, Clone)]
pub struct FloatLit {
    pub value: String,
    pub suffix: FloatTy,
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use synom::{IResult, TokenTree, TokenKind};
    use relex;

    pub fn lit(i: &[TokenTree]) -> IResult<&[TokenTree], Lit> {
        match i.first() {
            Some(&TokenTree{ kind: TokenKind::Literal(ref l), .. }) => {
                // XXX: I'm using my lexer for this temporarially, as it makes
                // my life easier. A final version shouldn't be this hacky,
                // though we'll probably want `proc_macro::Literal` -> actual
                // literal value conversions to be in a separate crate, rather
                // than requiring syn (which seems a bit heavyweight for that).
                let tok = if let Ok(tok) = relex::relex_literal(&l.to_string()) {
                    tok
                } else {
                    return IResult::Error
                };
                let lit = match tok {
                    relex::LToken::Lit(relex::Lit::Byte(b)) => {
                        Lit::Byte(b)
                    }
                    relex::LToken::Lit(relex::Lit::Char(c)) => {
                        Lit::Char(c)
                    }
                    relex::LToken::Lit(relex::Lit::Integer(v, suffix)) => {
                        let suffix = match suffix {
                            relex::IntSuffix::Unsuffixed =>
                                IntTy::Unsuffixed,
                            relex::IntSuffix::Isize =>
                                IntTy::Isize,
                            relex::IntSuffix::Usize =>
                                IntTy::Usize,
                            relex::IntSuffix::I8 =>
                                IntTy::I8,
                            relex::IntSuffix::U8 =>
                                IntTy::U8,
                            relex::IntSuffix::I16 =>
                                IntTy::I16,
                            relex::IntSuffix::U16 =>
                                IntTy::U16,
                            relex::IntSuffix::I32 =>
                                IntTy::I32,
                            relex::IntSuffix::U32 =>
                                IntTy::U32,
                            relex::IntSuffix::I64 =>
                                IntTy::I64,
                            relex::IntSuffix::U64 =>
                                IntTy::U64,
                        };
                        Lit::Int(v, suffix)
                    }
                    relex::LToken::Lit(relex::Lit::Float(v, suffix)) => {
                        let suffix = match suffix {
                            relex::FloatSuffix::Unsuffixed =>
                                FloatTy::Unsuffixed,
                            relex::FloatSuffix::F32 =>
                                FloatTy::F32,
                            relex::FloatSuffix::F64 =>
                                FloatTy::F64,
                        };
                        Lit::Float(v, suffix)
                    }
                    relex::LToken::Lit(relex::Lit::Str(s, relex::StrStyle::Cooked)) => {
                        Lit::Str(s, StrStyle::Cooked)
                    }
                    relex::LToken::Lit(relex::Lit::Str(s, relex::StrStyle::Raw(n))) => {
                        Lit::Str(s, StrStyle::Raw(n))
                    }
                    relex::LToken::Lit(relex::Lit::ByteStr(s, relex::StrStyle::Cooked)) => {
                        Lit::ByteStr(s, StrStyle::Cooked)
                    }
                    relex::LToken::Lit(relex::Lit::ByteStr(s, relex::StrStyle::Raw(n))) => {
                        Lit::ByteStr(s, StrStyle::Raw(n))
                    }
                    _ => return IResult::Error
                };

                IResult::Done(&i[1..], lit)
            }
            Some(&TokenTree{ kind: TokenKind::Word(ref w), .. }) => {
                if &**w == "true" {
                    IResult::Done(&i[1..], Lit::Bool(true))
                } else if &**w == "false" {
                    IResult::Done(&i[1..], Lit::Bool(false))
                } else {
                    IResult::Error
                }
            }
            _ => IResult::Error
        }
    }

    #[cfg(feature = "full")]
    pub fn digits(i: &[TokenTree]) -> IResult<&[TokenTree], u64> {
        if let IResult::Done(r, Lit::Int(v, IntTy::Unsuffixed)) = lit(i) {
            IResult::Done(r, v)
        } else {
            IResult::Error
        }
    }

    pub fn string(i: &[TokenTree]) -> IResult<&[TokenTree], String> {
        if let IResult::Done(r, Lit::Str(v, _)) = lit(i) {
            IResult::Done(r, v)
        } else {
            IResult::Error
        }
    }

    pub fn byte_string(i: &[TokenTree]) -> IResult<&[TokenTree], Vec<u8>> {
        if let IResult::Done(r, Lit::ByteStr(v, _)) = lit(i) {
            IResult::Done(r, v)
        } else {
            IResult::Error
        }
    }

    pub fn byte(i: &[TokenTree]) -> IResult<&[TokenTree], u8> {
        if let IResult::Done(r, Lit::Byte(b)) = lit(i) {
            IResult::Done(r, b)
        } else {
            IResult::Error
        }
    }

    pub fn character(i: &[TokenTree]) -> IResult<&[TokenTree], char> {
        if let IResult::Done(r, Lit::Char(c)) = lit(i) {
            IResult::Done(r, c)
        } else {
            IResult::Error
        }
    }

    pub fn float(i: &[TokenTree]) -> IResult<&[TokenTree], String> {
        if let IResult::Done(r, Lit::Float(f, _)) = lit(i) {
            IResult::Done(r, f)
        } else {
            IResult::Error
        }
    }

    pub fn int(i: &[TokenTree]) -> IResult<&[TokenTree], u64> {
        if let IResult::Done(r, Lit::Int(v, _)) = lit(i) {
            IResult::Done(r, v)
        } else {
            IResult::Error
        }
    }

    pub fn boolean(i: &[TokenTree]) -> IResult<&[TokenTree], bool> {
        if let IResult::Done(r, Lit::Bool(b)) = lit(i) {
            IResult::Done(r, b)
        } else {
            IResult::Error
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};
    use std::{ascii, iter};
    use std::fmt::{self, Display};
    use std::str;

    impl ToTokens for Lit {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Lit::Str(ref s, StrStyle::Cooked) => s.to_tokens(tokens),
                Lit::Str(ref s, StrStyle::Raw(n)) => {
                    tokens.append(&format!("r{delim}\"{string}\"{delim}",
                                           delim = iter::repeat("#").take(n).collect::<String>(),
                                           string = s));
                }
                Lit::ByteStr(ref v, StrStyle::Cooked) => {
                    let mut escaped = "b\"".to_string();
                    for &ch in v.iter() {
                        match ch {
                            0 => escaped.push_str(r"\0"),
                            b'\'' => escaped.push('\''),
                            _ => escaped.extend(ascii::escape_default(ch).map(|c| c as char)),
                        }
                    }
                    escaped.push('"');
                    tokens.append(&escaped);
                }
                Lit::ByteStr(ref vec, StrStyle::Raw(n)) => {
                    tokens.append(&format!("br{delim}\"{string}\"{delim}",
                                           delim = iter::repeat("#").take(n).collect::<String>(),
                                           string = str::from_utf8(vec).unwrap()));
                }
                Lit::Byte(b) => {
                    match b {
                        0 => tokens.append(r"b'\0'"),
                        b'\"' => tokens.append("b'\"'"),
                        _ => {
                            let mut escaped = "b'".to_string();
                            escaped.extend(ascii::escape_default(b).map(|c| c as char));
                            escaped.push('\'');
                            tokens.append(&escaped);
                        }
                    }
                }
                Lit::Char(ch) => ch.to_tokens(tokens),
                Lit::Int(value, ty) => tokens.append(&format!("{}{}", value, ty)),
                Lit::Float(ref value, ty) => tokens.append(&format!("{}{}", value, ty)),
                Lit::Bool(true) => tokens.append("true"),
                Lit::Bool(false) => tokens.append("false"),
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

    impl Display for FloatTy {
        fn fmt(&self, formatter: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            match *self {
                FloatTy::F32 => formatter.write_str("f32"),
                FloatTy::F64 => formatter.write_str("f64"),
                FloatTy::Unsuffixed => Ok(()),
            }
        }
    }
}
