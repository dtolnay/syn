use std::fmt;
use std::hash::{Hash, Hasher};

use proc_macro2::{self, Literal, TokenKind};

use {Span, TokenTree};

#[derive(Clone)]
pub struct Lit {
    pub value: LitKind,
    pub span: Span,
}

#[derive(Clone)]
pub enum LitKind {
    Bool(bool),
    Other(Literal),
}

impl Lit {
    pub fn into_token_tree(self) -> TokenTree {
        let kind = match self.value {
            LitKind::Bool(true) => TokenKind::Word("true".into()),
            LitKind::Bool(false) => TokenKind::Word("false".into()),
            LitKind::Other(l) => TokenKind::Literal(l),
        };
        TokenTree(proc_macro2::TokenTree {
            span: self.span.0,
            kind: kind,
        })
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, f)
    }
}

impl fmt::Debug for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, f)
    }
}

impl PartialEq for Lit {
    fn eq(&self, other: &Lit) -> bool {
        self.value == other.value
    }
}

impl Eq for Lit {}

impl Hash for Lit {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.value.hash(hasher)
    }
}

impl fmt::Display for LitKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitKind::Bool(b) => b.fmt(f),
            LitKind::Other(ref l) => l.fmt(f),
        }
    }
}

impl fmt::Debug for LitKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LitKind::Bool(b) => b.fmt(f),
            LitKind::Other(ref l) => fmt::Display::fmt(l, f),
        }
    }
}

impl PartialEq for LitKind {
    fn eq(&self, other: &LitKind) -> bool {
        match (self, other) {
            (&LitKind::Bool(b1), &LitKind::Bool(b2)) => b1 == b2,
            (&LitKind::Other(ref l1), &LitKind::Other(ref l2)) => {
                l1.to_string() == l2.to_string()
            }
            _ => false,
        }
    }
}

impl Eq for LitKind {}

impl Hash for LitKind {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        match *self {
            LitKind::Bool(b) => (0u8, b).hash(hasher),
            LitKind::Other(ref l) => (1u8, l.to_string()).hash(hasher),
        }
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use escape::{cooked_byte, cooked_byte_string, cooked_char, cooked_string, raw_string};
    use proc_macro2::Literal;
    use synom::IResult;
    use synom::space::skip_whitespace;
    use unicode_xid::UnicodeXID;

    fn l<T: Into<Literal>>(t: T) -> Lit {
        Lit {
            value: LitKind::Other(t.into()),
            span: Default::default(),
        }
    }

    named!(pub lit -> Lit, alt!(
        string
        |
        byte_string
        |
        byte
        |
        character
        |
        float
        |
        int
        |
        boolean
    ));

    named!(pub string -> Lit, alt!(
        quoted_string => { |s: String| l(&s[..]) }
        |
        preceded!(
            punct!("r"),
            raw_string
        ) => { |(s, n): (String, _)| l(Literal::raw_string(&s[..], n)) }
    ));

    named!(pub quoted_string -> String, delimited!(
        punct!("\""),
        cooked_string,
        tag!("\"")
    ));

    named!(pub byte_string -> Lit, alt!(
        delimited!(
            punct!("b\""),
            cooked_byte_string,
            tag!("\"")
        ) => { |vec: Vec<u8>| l(Literal::byte_string(&vec)) }
        |
        preceded!(
            punct!("br"),
            raw_string
        ) => { |(s, n): (String, _)| l(Literal::raw_byte_string(&s, n)) }
    ));

    named!(pub byte -> Lit, do_parse!(
        punct!("b") >>
        tag!("'") >>
        b: cooked_byte >>
        tag!("'") >>
        (l(Literal::byte_char(b)))
    ));

    named!(pub character -> Lit, do_parse!(
        punct!("'") >>
        ch: cooked_char >>
        tag!("'") >>
        (l(ch))
    ));

    named!(pub float -> Lit, do_parse!(
        value: float_string >>
        suffix: alt!(
            tag!("f32")
            |
            tag!("f64")
            |
            epsilon!() => { |_| "" }
        ) >>
        (l(Literal::float(&format!("{}{}", value, suffix))))
    ));

    named!(pub int -> Lit, do_parse!(
        value: digits >>
        suffix: alt!(
            tag!("isize")
            |
            tag!("i8")
            |
            tag!("i16")
            |
            tag!("i32")
            |
            tag!("i64")
            |
            tag!("usize")
            |
            tag!("u8")
            |
            tag!("u16")
            |
            tag!("u32")
            |
            tag!("u64")
            |
            epsilon!() => { |_| "" }
        ) >>
        (l(Literal::integer(&format!("{}{}", value, suffix))))
    ));

    named!(pub boolean -> Lit, alt!(
        keyword!("true") => { |_| Lit {
            span: Span::default(),
            value: LitKind::Bool(true),
        } }
        |
        keyword!("false") => { |_| Lit {
            span: Span::default(),
            value: LitKind::Bool(false),
        } }
    ));

    fn float_string(mut input: &str) -> IResult<&str, String> {
        input = skip_whitespace(input);

        let mut chars = input.chars().peekable();
        match chars.next() {
            Some(ch) if ch >= '0' && ch <= '9' => {}
            _ => return IResult::Error,
        }

        let mut len = 1;
        let mut has_dot = false;
        let mut has_exp = false;
        while let Some(&ch) = chars.peek() {
            match ch {
                '0'...'9' | '_' => {
                    chars.next();
                    len += 1;
                }
                '.' => {
                    if has_dot {
                        break;
                    }
                    chars.next();
                    if chars.peek()
                           .map(|&ch| ch == '.' || UnicodeXID::is_xid_start(ch))
                           .unwrap_or(false) {
                        return IResult::Error;
                    }
                    len += 1;
                    has_dot = true;
                }
                'e' | 'E' => {
                    chars.next();
                    len += 1;
                    has_exp = true;
                    break;
                }
                _ => break,
            }
        }

        let rest = &input[len..];
        if !(has_dot || has_exp || rest.starts_with("f32") || rest.starts_with("f64")) {
            return IResult::Error;
        }

        if has_exp {
            let mut has_exp_value = false;
            while let Some(&ch) = chars.peek() {
                match ch {
                    '+' | '-' => {
                        if has_exp_value {
                            break;
                        }
                        chars.next();
                        len += 1;
                    }
                    '0'...'9' => {
                        chars.next();
                        len += 1;
                        has_exp_value = true;
                    }
                    '_' => {
                        chars.next();
                        len += 1;
                    }
                    _ => break,
                }
            }
            if !has_exp_value {
                return IResult::Error;
            }
        }

        IResult::Done(&input[len..], input[..len].replace("_", ""))
    }

    pub fn digits(mut input: &str) -> IResult<&str, &str> {
        input = skip_whitespace(input);

        let base = if input.starts_with("0x") {
            16
        } else if input.starts_with("0o") {
            8
        } else if input.starts_with("0b") {
            2
        } else {
            10
        };

        let mut value = 0u64;
        let mut len = if base == 10 {0} else {2};
        let mut empty = true;
        for b in input[len..].bytes() {
            let digit = match b {
                b'0'...b'9' => (b - b'0') as u64,
                b'a'...b'f' => 10 + (b - b'a') as u64,
                b'A'...b'F' => 10 + (b - b'A') as u64,
                b'_' => {
                    if empty && base == 10 {
                        return IResult::Error;
                    }
                    len += 1;
                    continue;
                }
                _ => break,
            };
            if digit >= base {
                return IResult::Error;
            }
            value = match value.checked_mul(base) {
                Some(value) => value,
                None => return IResult::Error,
            };
            value = match value.checked_add(digit) {
                Some(value) => value,
                None => return IResult::Error,
            };
            len += 1;
            empty = false;
        }
        if empty {
            IResult::Error
        } else {
            IResult::Done(&input[len..], &input[..len])
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    use proc_macro2::{TokenTree, TokenKind};

    impl ToTokens for Lit {
        fn to_tokens(&self, tokens: &mut Tokens) {
            let kind = match self.value {
                LitKind::Bool(true) => TokenKind::Word("true".into()),
                LitKind::Bool(false) => TokenKind::Word("false".into()),
                LitKind::Other(ref l) => TokenKind::Literal(l.clone()),
            };
            tokens.append(TokenTree {
                span: self.span.0,
                kind: kind,
            });
        }
    }
}
