use proc_macro2::{Literal, Span};
use std::fmt::{self, Display};
use std::str;

#[cfg(feature = "printing")]
use proc_macro2::Ident;

#[cfg(feature = "parsing")]
use proc_macro2::TokenStream;

use proc_macro2::TokenTree;

#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};

#[cfg(feature = "parsing")]
use crate::lookahead;
#[cfg(feature = "parsing")]
use crate::parse::{Parse, Parser, Result};

ast_enum_of_structs! {
    /// A Rust literal such as a string or integer or boolean.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    ///
    /// # Syntax tree enum
    ///
    /// This type is a [syntax tree enum].
    ///
    /// [syntax tree enum]: enum.Expr.html#syntax-tree-enums
    //
    // TODO: change syntax-tree-enum link to an intra rustdoc link, currently
    // blocked on https://github.com/rust-lang/rust/issues/62833
    pub enum Lit #manual_extra_traits {
        /// A UTF-8 string literal: `"foo"`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Str(LitStr #manual_extra_traits {
            token: Literal,
        }),

        /// A byte string literal: `b"foo"`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub ByteStr(LitByteStr #manual_extra_traits {
            token: Literal,
        }),

        /// A byte literal: `b'f'`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Byte(LitByte #manual_extra_traits {
            token: Literal,
        }),

        /// A character literal: `'a'`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Char(LitChar #manual_extra_traits {
            token: Literal,
        }),

        /// An integer literal: `1` or `1u16`.
        ///
        /// Holds up to 64 bits of data. Use `Lit::Verbatim` for any larger
        /// integer literal.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Int(LitInt #manual_extra_traits {
            token: Literal,
            digits: String,
            suffix: String,
        }),

        /// A floating point literal: `1f64` or `1.0e10f64`.
        ///
        /// Must be finite. May not be infinte or NaN.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Float(LitFloat #manual_extra_traits {
            token: Literal,
            digits: String,
            suffix: String,
        }),

        /// A boolean literal: `true` or `false`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Bool(LitBool #manual_extra_traits {
            pub value: bool,
            pub span: Span,
        }),

        /// A raw token literal not interpreted by Syn, possibly because it
        /// represents an integer larger than 64 bits.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub Verbatim(Literal),
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for Lit {}

#[cfg(feature = "extra-traits")]
impl PartialEq for Lit {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Lit::Str(this), Lit::Str(other)) => this == other,
            (Lit::ByteStr(this), Lit::ByteStr(other)) => this == other,
            (Lit::Byte(this), Lit::Byte(other)) => this == other,
            (Lit::Char(this), Lit::Char(other)) => this == other,
            (Lit::Int(this), Lit::Int(other)) => this == other,
            (Lit::Float(this), Lit::Float(other)) => this == other,
            (Lit::Bool(this), Lit::Bool(other)) => this == other,
            (Lit::Verbatim(this), Lit::Verbatim(other)) => {
                this.to_string() == other.to_string()
            }
            _ => false,
        }
    }
}

#[cfg(feature = "extra-traits")]
impl Hash for Lit {
    fn hash<H>(&self, hash: &mut H)
    where
        H: Hasher,
    {
        match self {
            Lit::Str(lit) => {
                hash.write_u8(0);
                lit.hash(hash);
            }
            Lit::ByteStr(lit) => {
                hash.write_u8(1);
                lit.hash(hash);
            }
            Lit::Byte(lit) => {
                hash.write_u8(2);
                lit.hash(hash);
            }
            Lit::Char(lit) => {
                hash.write_u8(3);
                lit.hash(hash);
            }
            Lit::Int(lit) => {
                hash.write_u8(4);
                lit.hash(hash);
            }
            Lit::Float(lit) => {
                hash.write_u8(5);
                lit.hash(hash);
            }
            Lit::Bool(lit) => {
                hash.write_u8(6);
                lit.hash(hash);
            }
            Lit::Verbatim(lit) => {
                hash.write_u8(7);
                lit.to_string().hash(hash);
            }
        }
    }
}

impl LitStr {
    pub fn new(value: &str, span: Span) -> Self {
        let mut lit = Literal::string(value);
        lit.set_span(span);
        LitStr { token: lit }
    }

    pub fn value(&self) -> String {
        value::parse_lit_str(&self.token.to_string())
    }

    /// Parse a syntax tree node from the content of this string literal.
    ///
    /// All spans in the syntax tree will point to the span of this `LitStr`.
    ///
    /// # Example
    ///
    /// ```edition2018
    /// use proc_macro2::Span;
    /// use syn::{Attribute, Error, Ident, Lit, Meta, MetaNameValue, Path, Result};
    ///
    /// // Parses the path from an attribute that looks like:
    /// //
    /// //     #[path = "a::b::c"]
    /// //
    /// // or returns `None` if the input is some other attribute.
    /// fn get_path(attr: &Attribute) -> Result<Option<Path>> {
    ///     if !attr.path.is_ident("path") {
    ///         return Ok(None);
    ///     }
    ///
    ///     match attr.parse_meta()? {
    ///         Meta::NameValue(MetaNameValue { lit: Lit::Str(lit_str), .. }) => {
    ///             lit_str.parse().map(Some)
    ///         }
    ///         _ => {
    ///             let message = "expected #[path = \"...\"]";
    ///             Err(Error::new_spanned(attr, message))
    ///         }
    ///     }
    /// }
    /// ```
    #[cfg(feature = "parsing")]
    pub fn parse<T: Parse>(&self) -> Result<T> {
        self.parse_with(T::parse)
    }

    /// Invoke parser on the content of this string literal.
    ///
    /// All spans in the syntax tree will point to the span of this `LitStr`.
    ///
    /// # Example
    ///
    /// ```edition2018
    /// # use proc_macro2::Span;
    /// # use syn::{LitStr, Result};
    /// #
    /// # fn main() -> Result<()> {
    /// #     let lit_str = LitStr::new("a::b::c", Span::call_site());
    /// #
    /// #     const IGNORE: &str = stringify! {
    /// let lit_str: LitStr = /* ... */;
    /// #     };
    ///
    /// // Parse a string literal like "a::b::c" into a Path, not allowing
    /// // generic arguments on any of the path segments.
    /// let basic_path = lit_str.parse_with(syn::Path::parse_mod_style)?;
    /// #
    /// #     Ok(())
    /// # }
    /// ```
    #[cfg(feature = "parsing")]
    pub fn parse_with<F: Parser>(&self, parser: F) -> Result<F::Output> {
        use proc_macro2::Group;

        // Token stream with every span replaced by the given one.
        fn respan_token_stream(stream: TokenStream, span: Span) -> TokenStream {
            stream
                .into_iter()
                .map(|token| respan_token_tree(token, span))
                .collect()
        }

        // Token tree with every span replaced by the given one.
        fn respan_token_tree(mut token: TokenTree, span: Span) -> TokenTree {
            match token {
                TokenTree::Group(ref mut g) => {
                    let stream = respan_token_stream(g.stream().clone(), span);
                    *g = Group::new(g.delimiter(), stream);
                    g.set_span(span);
                }
                ref mut other => other.set_span(span),
            }
            token
        }

        // Parse string literal into a token stream with every span equal to the
        // original literal's span.
        let mut tokens = crate::parse_str(&self.value())?;
        tokens = respan_token_stream(tokens, self.span());

        parser.parse2(tokens)
    }

    pub fn span(&self) -> Span {
        self.token.span()
    }

    pub fn set_span(&mut self, span: Span) {
        self.token.set_span(span)
    }
}

impl LitByteStr {
    pub fn new(value: &[u8], span: Span) -> Self {
        let mut token = Literal::byte_string(value);
        token.set_span(span);
        LitByteStr { token: token }
    }

    pub fn value(&self) -> Vec<u8> {
        value::parse_lit_byte_str(&self.token.to_string())
    }

    pub fn span(&self) -> Span {
        self.token.span()
    }

    pub fn set_span(&mut self, span: Span) {
        self.token.set_span(span)
    }
}

impl LitByte {
    pub fn new(value: u8, span: Span) -> Self {
        let mut token = Literal::u8_suffixed(value);
        token.set_span(span);
        LitByte { token: token }
    }

    pub fn value(&self) -> u8 {
        value::parse_lit_byte(&self.token.to_string())
    }

    pub fn span(&self) -> Span {
        self.token.span()
    }

    pub fn set_span(&mut self, span: Span) {
        self.token.set_span(span)
    }
}

impl LitChar {
    pub fn new(value: char, span: Span) -> Self {
        let mut token = Literal::character(value);
        token.set_span(span);
        LitChar { token: token }
    }

    pub fn value(&self) -> char {
        value::parse_lit_char(&self.token.to_string())
    }

    pub fn span(&self) -> Span {
        self.token.span()
    }

    pub fn set_span(&mut self, span: Span) {
        self.token.set_span(span)
    }
}

impl LitInt {
    pub fn new(repr: &str, span: Span) -> Self {
        if let Some((digits, suffix)) = value::parse_lit_int(repr) {
            let mut token = value::to_literal(repr);
            token.set_span(span);
            LitInt { token, digits, suffix }
        } else {
            panic!("Not an integer literal: `{}`", repr);
        }
    }

    pub fn base10_digits(&self) -> &str {
        &self.digits
    }

    pub fn suffix(&self) -> &str {
        &self.suffix
    }

    pub fn span(&self) -> Span {
        self.token.span()
    }

    pub fn set_span(&mut self, span: Span) {
        self.token.set_span(span)
    }
}

impl From<Literal> for LitInt {
    fn from(token: Literal) -> Self {
        let repr = token.to_string();
        if let Some((digits, suffix)) = value::parse_lit_int(&repr) {
            LitInt { token, digits, suffix }
        } else {
            panic!("Not an integer literal: `{}`", repr);
        }
    }
}

impl Display for LitInt {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.token.fmt(formatter)
    }
}

impl LitFloat {
    pub fn new(repr: &str, span: Span) -> Self {
        if let Some((digits, suffix)) = value::parse_lit_float(repr) {
            let mut token = value::to_literal(repr);
            token.set_span(span);
            LitFloat { token, digits, suffix }
        } else {
            panic!("Not a float literal: `{}`", repr);
        }
    }

    pub fn base10_digits(&self) -> &str {
        &self.digits
    }

    pub fn suffix(&self) -> &str {
        &self.suffix
    }

    pub fn span(&self) -> Span {
        self.token.span()
    }

    pub fn set_span(&mut self, span: Span) {
        self.token.set_span(span)
    }
}

impl From<Literal> for LitFloat {
    fn from(token: Literal) -> Self {
        let repr = token.to_string();
        if let Some((digits, suffix)) = value::parse_lit_float(&repr) {
            LitFloat { token, digits, suffix }
        } else {
            panic!("Not a float literal: `{}`", repr);
        }
    }
}

impl Display for LitFloat {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.token.fmt(formatter)
    }
}

macro_rules! lit_extra_traits {
    ($ty:ident, $field:ident) => {
        #[cfg(feature = "extra-traits")]
        impl Eq for $ty {}

        #[cfg(feature = "extra-traits")]
        impl PartialEq for $ty {
            fn eq(&self, other: &Self) -> bool {
                self.$field.to_string() == other.$field.to_string()
            }
        }

        #[cfg(feature = "extra-traits")]
        impl Hash for $ty {
            fn hash<H>(&self, state: &mut H)
            where
                H: Hasher,
            {
                self.$field.to_string().hash(state);
            }
        }

        #[cfg(feature = "parsing")]
        #[doc(hidden)]
        #[allow(non_snake_case)]
        pub fn $ty(marker: lookahead::TokenMarker) -> $ty {
            match marker {}
        }
    };
}

lit_extra_traits!(LitStr, token);
lit_extra_traits!(LitByteStr, token);
lit_extra_traits!(LitByte, token);
lit_extra_traits!(LitChar, token);
lit_extra_traits!(LitInt, token);
lit_extra_traits!(LitFloat, token);
lit_extra_traits!(LitBool, value);

ast_enum! {
    /// The style of a string literal, either plain quoted or a raw string like
    /// `r##"data"##`.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub enum StrStyle #no_visit {
        /// An ordinary string like `"data"`.
        Cooked,
        /// A raw string like `r##"data"##`.
        ///
        /// The unsigned integer is the number of `#` symbols used.
        Raw(usize),
    }
}

#[cfg(feature = "parsing")]
#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Lit(marker: lookahead::TokenMarker) -> Lit {
    match marker {}
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use crate::parse::{Parse, ParseStream, Result};

    impl Parse for Lit {
        fn parse(input: ParseStream) -> Result<Self> {
            input.step(|cursor| {
                if let Some((lit, rest)) = cursor.literal() {
                    return Ok((Lit::new(lit), rest));
                }
                while let Some((ident, rest)) = cursor.ident() {
                    let value = if ident == "true" {
                        true
                    } else if ident == "false" {
                        false
                    } else {
                        break;
                    };
                    let lit_bool = LitBool {
                        value: value,
                        span: ident.span(),
                    };
                    return Ok((Lit::Bool(lit_bool), rest));
                }
                Err(cursor.error("expected literal"))
            })
        }
    }

    impl Parse for LitStr {
        fn parse(input: ParseStream) -> Result<Self> {
            let head = input.fork();
            match input.parse()? {
                Lit::Str(lit) => Ok(lit),
                _ => Err(head.error("expected string literal")),
            }
        }
    }

    impl Parse for LitByteStr {
        fn parse(input: ParseStream) -> Result<Self> {
            let head = input.fork();
            match input.parse()? {
                Lit::ByteStr(lit) => Ok(lit),
                _ => Err(head.error("expected byte string literal")),
            }
        }
    }

    impl Parse for LitByte {
        fn parse(input: ParseStream) -> Result<Self> {
            let head = input.fork();
            match input.parse()? {
                Lit::Byte(lit) => Ok(lit),
                _ => Err(head.error("expected byte literal")),
            }
        }
    }

    impl Parse for LitChar {
        fn parse(input: ParseStream) -> Result<Self> {
            let head = input.fork();
            match input.parse()? {
                Lit::Char(lit) => Ok(lit),
                _ => Err(head.error("expected character literal")),
            }
        }
    }

    impl Parse for LitInt {
        fn parse(input: ParseStream) -> Result<Self> {
            let head = input.fork();
            match input.parse()? {
                Lit::Int(lit) => Ok(lit),
                _ => Err(head.error("expected integer literal")),
            }
        }
    }

    impl Parse for LitFloat {
        fn parse(input: ParseStream) -> Result<Self> {
            let head = input.fork();
            match input.parse()? {
                Lit::Float(lit) => Ok(lit),
                _ => Err(head.error("expected floating point literal")),
            }
        }
    }

    impl Parse for LitBool {
        fn parse(input: ParseStream) -> Result<Self> {
            let head = input.fork();
            match input.parse()? {
                Lit::Bool(lit) => Ok(lit),
                _ => Err(head.error("expected boolean literal")),
            }
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use proc_macro2::TokenStream;
    use quote::{ToTokens, TokenStreamExt};

    impl ToTokens for LitStr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token.to_tokens(tokens);
        }
    }

    impl ToTokens for LitByteStr {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token.to_tokens(tokens);
        }
    }

    impl ToTokens for LitByte {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token.to_tokens(tokens);
        }
    }

    impl ToTokens for LitChar {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token.to_tokens(tokens);
        }
    }

    impl ToTokens for LitInt {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token.to_tokens(tokens);
        }
    }

    impl ToTokens for LitFloat {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.token.to_tokens(tokens);
        }
    }

    impl ToTokens for LitBool {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let s = if self.value { "true" } else { "false" };
            tokens.append(Ident::new(s, self.span));
        }
    }
}

mod value {
    use super::*;
    use crate::bigint::BigInt;
    use proc_macro2::TokenStream;
    use std::char;
    use std::ops::{Index, RangeFrom};

    impl Lit {
        /// Interpret a Syn literal from a proc-macro2 literal.
        pub fn new(token: Literal) -> Self {
            let repr = token.to_string();

            match byte(&repr, 0) {
                b'"' | b'r' => return Lit::Str(LitStr { token: token }),
                b'b' => match byte(&repr, 1) {
                    b'"' | b'r' => return Lit::ByteStr(LitByteStr { token: token }),
                    b'\'' => return Lit::Byte(LitByte { token: token }),
                    _ => {}
                },
                b'\'' => return Lit::Char(LitChar { token: token }),
                b'0'...b'9' | b'-' => {
                    if !(repr.ends_with("f32") || repr.ends_with("f64")) {
                        if let Some((digits, suffix)) = parse_lit_int(&repr) {
                            return Lit::Int(LitInt { token, digits, suffix });
                        }
                    }
                    if let Some((digits, suffix)) = parse_lit_float(&repr) {
                        return Lit::Float(LitFloat { token, digits, suffix });
                    }
                }
                b't' | b'f' => {
                    if repr == "true" || repr == "false" {
                        return Lit::Bool(LitBool {
                            value: repr == "true",
                            span: token.span(),
                        });
                    }
                }
                _ => {}
            }

            panic!("Unrecognized literal: `{}`", repr);
        }
    }

    /// Get the byte at offset idx, or a default of `b'\0'` if we're looking
    /// past the end of the input buffer.
    pub fn byte<S: AsRef<[u8]> + ?Sized>(s: &S, idx: usize) -> u8 {
        let s = s.as_ref();
        if idx < s.len() {
            s[idx]
        } else {
            0
        }
    }

    fn next_chr(s: &str) -> char {
        s.chars().next().unwrap_or('\0')
    }

    pub fn parse_lit_str(s: &str) -> String {
        match byte(s, 0) {
            b'"' => parse_lit_str_cooked(s),
            b'r' => parse_lit_str_raw(s),
            _ => unreachable!(),
        }
    }

    // Clippy false positive
    // https://github.com/rust-lang-nursery/rust-clippy/issues/2329
    #[cfg_attr(feature = "cargo-clippy", allow(needless_continue))]
    fn parse_lit_str_cooked(mut s: &str) -> String {
        assert_eq!(byte(s, 0), b'"');
        s = &s[1..];

        let mut out = String::new();
        'outer: loop {
            let ch = match byte(s, 0) {
                b'"' => break,
                b'\\' => {
                    let b = byte(s, 1);
                    s = &s[2..];
                    match b {
                        b'x' => {
                            let (byte, rest) = backslash_x(s);
                            s = rest;
                            assert!(byte <= 0x80, "Invalid \\x byte in string literal");
                            char::from_u32(u32::from(byte)).unwrap()
                        }
                        b'u' => {
                            let (chr, rest) = backslash_u(s);
                            s = rest;
                            chr
                        }
                        b'n' => '\n',
                        b'r' => '\r',
                        b't' => '\t',
                        b'\\' => '\\',
                        b'0' => '\0',
                        b'\'' => '\'',
                        b'"' => '"',
                        b'\r' | b'\n' => loop {
                            let ch = next_chr(s);
                            if ch.is_whitespace() {
                                s = &s[ch.len_utf8()..];
                            } else {
                                continue 'outer;
                            }
                        },
                        b => panic!("unexpected byte {:?} after \\ character in byte literal", b),
                    }
                }
                b'\r' => {
                    assert_eq!(byte(s, 1), b'\n', "Bare CR not allowed in string");
                    s = &s[2..];
                    '\n'
                }
                _ => {
                    let ch = next_chr(s);
                    s = &s[ch.len_utf8()..];
                    ch
                }
            };
            out.push(ch);
        }

        assert_eq!(s, "\"");
        out
    }

    fn parse_lit_str_raw(mut s: &str) -> String {
        assert_eq!(byte(s, 0), b'r');
        s = &s[1..];

        let mut pounds = 0;
        while byte(s, pounds) == b'#' {
            pounds += 1;
        }
        assert_eq!(byte(s, pounds), b'"');
        assert_eq!(byte(s, s.len() - pounds - 1), b'"');
        for end in s[s.len() - pounds..].bytes() {
            assert_eq!(end, b'#');
        }

        s[pounds + 1..s.len() - pounds - 1].to_owned()
    }

    pub fn parse_lit_byte_str(s: &str) -> Vec<u8> {
        assert_eq!(byte(s, 0), b'b');
        match byte(s, 1) {
            b'"' => parse_lit_byte_str_cooked(s),
            b'r' => parse_lit_byte_str_raw(s),
            _ => unreachable!(),
        }
    }

    // Clippy false positive
    // https://github.com/rust-lang-nursery/rust-clippy/issues/2329
    #[cfg_attr(feature = "cargo-clippy", allow(needless_continue))]
    fn parse_lit_byte_str_cooked(mut s: &str) -> Vec<u8> {
        assert_eq!(byte(s, 0), b'b');
        assert_eq!(byte(s, 1), b'"');
        s = &s[2..];

        // We're going to want to have slices which don't respect codepoint boundaries.
        let mut s = s.as_bytes();

        let mut out = Vec::new();
        'outer: loop {
            let byte = match byte(s, 0) {
                b'"' => break,
                b'\\' => {
                    let b = byte(s, 1);
                    s = &s[2..];
                    match b {
                        b'x' => {
                            let (b, rest) = backslash_x(s);
                            s = rest;
                            b
                        }
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'\\' => b'\\',
                        b'0' => b'\0',
                        b'\'' => b'\'',
                        b'"' => b'"',
                        b'\r' | b'\n' => loop {
                            let byte = byte(s, 0);
                            let ch = char::from_u32(u32::from(byte)).unwrap();
                            if ch.is_whitespace() {
                                s = &s[1..];
                            } else {
                                continue 'outer;
                            }
                        },
                        b => panic!("unexpected byte {:?} after \\ character in byte literal", b),
                    }
                }
                b'\r' => {
                    assert_eq!(byte(s, 1), b'\n', "Bare CR not allowed in string");
                    s = &s[2..];
                    b'\n'
                }
                b => {
                    s = &s[1..];
                    b
                }
            };
            out.push(byte);
        }

        assert_eq!(s, b"\"");
        out
    }

    fn parse_lit_byte_str_raw(s: &str) -> Vec<u8> {
        assert_eq!(byte(s, 0), b'b');
        parse_lit_str_raw(&s[1..]).into_bytes()
    }

    pub fn parse_lit_byte(s: &str) -> u8 {
        assert_eq!(byte(s, 0), b'b');
        assert_eq!(byte(s, 1), b'\'');

        // We're going to want to have slices which don't respect codepoint boundaries.
        let mut s = s[2..].as_bytes();

        let b = match byte(s, 0) {
            b'\\' => {
                let b = byte(s, 1);
                s = &s[2..];
                match b {
                    b'x' => {
                        let (b, rest) = backslash_x(s);
                        s = rest;
                        b
                    }
                    b'n' => b'\n',
                    b'r' => b'\r',
                    b't' => b'\t',
                    b'\\' => b'\\',
                    b'0' => b'\0',
                    b'\'' => b'\'',
                    b'"' => b'"',
                    b => panic!("unexpected byte {:?} after \\ character in byte literal", b),
                }
            }
            b => {
                s = &s[1..];
                b
            }
        };

        assert_eq!(byte(s, 0), b'\'');
        b
    }

    pub fn parse_lit_char(mut s: &str) -> char {
        assert_eq!(byte(s, 0), b'\'');
        s = &s[1..];

        let ch = match byte(s, 0) {
            b'\\' => {
                let b = byte(s, 1);
                s = &s[2..];
                match b {
                    b'x' => {
                        let (byte, rest) = backslash_x(s);
                        s = rest;
                        assert!(byte <= 0x80, "Invalid \\x byte in string literal");
                        char::from_u32(u32::from(byte)).unwrap()
                    }
                    b'u' => {
                        let (chr, rest) = backslash_u(s);
                        s = rest;
                        chr
                    }
                    b'n' => '\n',
                    b'r' => '\r',
                    b't' => '\t',
                    b'\\' => '\\',
                    b'0' => '\0',
                    b'\'' => '\'',
                    b'"' => '"',
                    b => panic!("unexpected byte {:?} after \\ character in byte literal", b),
                }
            }
            _ => {
                let ch = next_chr(s);
                s = &s[ch.len_utf8()..];
                ch
            }
        };
        assert_eq!(s, "\'", "Expected end of char literal");
        ch
    }

    fn backslash_x<S>(s: &S) -> (u8, &S)
    where
        S: Index<RangeFrom<usize>, Output = S> + AsRef<[u8]> + ?Sized,
    {
        let mut ch = 0;
        let b0 = byte(s, 0);
        let b1 = byte(s, 1);
        ch += 0x10
            * match b0 {
                b'0'...b'9' => b0 - b'0',
                b'a'...b'f' => 10 + (b0 - b'a'),
                b'A'...b'F' => 10 + (b0 - b'A'),
                _ => panic!("unexpected non-hex character after \\x"),
            };
        ch += match b1 {
            b'0'...b'9' => b1 - b'0',
            b'a'...b'f' => 10 + (b1 - b'a'),
            b'A'...b'F' => 10 + (b1 - b'A'),
            _ => panic!("unexpected non-hex character after \\x"),
        };
        (ch, &s[2..])
    }

    fn backslash_u(mut s: &str) -> (char, &str) {
        if byte(s, 0) != b'{' {
            panic!("expected {{ after \\u");
        }
        s = &s[1..];

        let mut ch = 0;
        for _ in 0..6 {
            let b = byte(s, 0);
            match b {
                b'0'...b'9' => {
                    ch *= 0x10;
                    ch += u32::from(b - b'0');
                    s = &s[1..];
                }
                b'a'...b'f' => {
                    ch *= 0x10;
                    ch += u32::from(10 + b - b'a');
                    s = &s[1..];
                }
                b'A'...b'F' => {
                    ch *= 0x10;
                    ch += u32::from(10 + b - b'A');
                    s = &s[1..];
                }
                b'}' => break,
                _ => panic!("unexpected non-hex character after \\u"),
            }
        }
        assert!(byte(s, 0) == b'}');
        s = &s[1..];

        if let Some(ch) = char::from_u32(ch) {
            (ch, s)
        } else {
            panic!("character code {:x} is not a valid unicode character", ch);
        }
    }


    // Returns base 10 digits and suffix.
    pub fn parse_lit_int(mut s: &str) -> Option<(String, String)> {
        let negative = byte(s, 0) == b'-';
        if negative {
            s = &s[1..];
        }

        let base = match (byte(s, 0), byte(s, 1)) {
            (b'0', b'x') => {
                s = &s[2..];
                16
            }
            (b'0', b'o') => {
                s = &s[2..];
                8
            }
            (b'0', b'b') => {
                s = &s[2..];
                2
            }
            (b'0'...b'9', _) => 10,
            _ => return None,
        };

        let mut value = BigInt::new();
        loop {
            let b = byte(s, 0);
            let digit = match b {
                b'0'...b'9' => b - b'0',
                b'a'...b'f' if base > 10 => b - b'a' + 10,
                b'A'...b'F' if base > 10 => b - b'A' + 10,
                b'_' => {
                    s = &s[1..];
                    continue;
                }
                // NOTE: Looking at a floating point literal, we don't want to
                // consider these integers.
                b'.' if base == 10 => return None,
                b'e' | b'E' if base == 10 => return None,
                _ => break,
            };

            if digit >= base {
                return None;
            }

            value *= base;
            value += digit;
            s = &s[1..];
        }

        let suffix = s;
        if suffix.is_empty() || crate::ident::xid_ok(&suffix) {
            let mut repr = value.to_string();
            if negative {
                repr.insert(0, '-');
            }
            Some((repr, suffix.to_owned()))
        } else {
            None
        }
    }

    // Returns base 10 digits and suffix.
    pub fn parse_lit_float(input: &str) -> Option<(String, String)> {
        // Rust's floating point literals are very similar to the ones parsed by
        // the standard library, except that rust's literals can contain
        // ignorable underscores. Let's remove those underscores.

        let mut bytes = input.to_owned().into_bytes();

        let start = (*bytes.get(0)? == b'-') as usize;
        match bytes.get(start)? {
            b'0'..=b'9' => {}
            _ => return None,
        }

        let mut read = start;
        let mut write = start;
        let mut has_dot = false;
        let mut has_e = false;
        let mut has_neg = false;
        let mut has_exponent = false;
        while read < bytes.len() {
            match bytes[read] {
                b'_' => {
                    // Don't increase write
                    read += 1;
                    continue;
                }
                b @ b'0'..=b'9' => {
                    if has_e {
                        has_exponent = true;
                    }
                    bytes[write] = b;
                }
                b'.' => {
                    if has_e || has_dot {
                        return None;
                    }
                    has_dot = true;
                    bytes[write] = b'.';
                }
                b'e' | b'E' => {
                    if has_e {
                        return None;
                    }
                    has_e = true;
                    bytes[write] = b'e';
                }
                b'-' => {
                    if has_neg || has_exponent || !has_e {
                        return None;
                    }
                    has_neg = true;
                    bytes[write] = b'-';
                }
                _ => break,
            }
            read += 1;
            write += 1;
        }

        if has_e && !has_exponent {
            return None;
        }

        let mut digits = String::from_utf8(bytes).unwrap();
        let suffix = digits.split_off(read);
        digits.truncate(write);
        if suffix.is_empty() || crate::ident::xid_ok(&suffix) {
            Some((digits, suffix))
        } else {
            None
        }
    }

    pub fn to_literal(s: &str) -> Literal {
        let stream = s.parse::<TokenStream>().unwrap();
        match stream.into_iter().next().unwrap() {
            TokenTree::Literal(l) => l,
            _ => unreachable!(),
        }
    }
}
