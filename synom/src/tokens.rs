//! Discrete tokens that can be parsed out by synom.
//!
//! This module contains a number of useful tokens like `+=` and `/` along with
//! keywords like `crate` and such. These structures are used to track the spans
//! of these tokens and all implment the `ToTokens` and `Synom` traits when the
//! corresponding feature is activated.

use span::Span;

macro_rules! tokens {
    (
        ops: {
            $(($($op:tt)*),)*
        }
        delim: {
            $(($($delim:tt)*),)*
        }
        syms: {
            $(($($sym:tt)*),)*
        }
    ) => (
        $(op! { $($op)* })*
        $(delim! { $($delim)* })*
        $(sym! { $($sym)* })*
    )
}

macro_rules! op {
    (pub struct $name:ident($($contents:tt)*) => $s:expr) => {
        #[cfg_attr(feature = "clone-impls", derive(Copy, Clone))]
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[derive(Default)]
        pub struct $name(pub $($contents)*);

        #[cfg(feature = "printing")]
        impl ::quote::ToTokens for $name {
            fn to_tokens(&self, tokens: &mut ::quote::Tokens) {
                printing::op($s, &self.0, tokens);
            }
        }

        #[cfg(feature = "parsing")]
        impl ::Synom for $name {
            fn parse(tokens: $crate::Cursor) -> $crate::PResult<$name> {
                parsing::op($s, tokens, $name)
            }
        }
    }
}

macro_rules! sym {
    (pub struct $name:ident => $s:expr) => {
        #[cfg_attr(feature = "clone-impls", derive(Copy, Clone))]
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[derive(Default)]
        pub struct $name(pub Span);

        #[cfg(feature = "printing")]
        impl ::quote::ToTokens for $name {
            fn to_tokens(&self, tokens: &mut ::quote::Tokens) {
                printing::sym($s, &self.0, tokens);
            }
        }

        #[cfg(feature = "parsing")]
        impl ::Synom for $name {
            fn parse(tokens: $crate::Cursor) -> $crate::PResult<$name> {
                parsing::sym($s, tokens, $name)
            }
        }
    }
}

macro_rules! delim {
    (pub struct $name:ident => $s:expr) => {
        #[cfg_attr(feature = "clone-impls", derive(Copy, Clone))]
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[derive(Default)]
        pub struct $name(pub Span);

        impl $name {
            #[cfg(feature = "printing")]
            pub fn surround<F>(&self,
                               tokens: &mut ::quote::Tokens,
                               f: F)
                where F: FnOnce(&mut ::quote::Tokens)
            {
                printing::delim($s, &self.0, tokens, f);
            }

            #[cfg(feature = "parsing")]
            pub fn parse<F, R>(tokens: $crate::Cursor, f: F) -> $crate::PResult<(R, $name)>
                where F: FnOnce($crate::Cursor) -> $crate::PResult<R>
            {
                parsing::delim($s, tokens, $name, f)
            }
        }
    }
}

tokens! {
    ops: {
        (pub struct Add([Span; 1])          => "+"),
        (pub struct AddEq([Span; 2])        => "+="),
        (pub struct And([Span; 1])          => "&"),
        (pub struct AndAnd([Span; 2])       => "&&"),
        (pub struct AndEq([Span; 2])        => "&="),
        (pub struct At([Span; 1])           => "@"),
        (pub struct Bang([Span; 1])         => "!"),
        (pub struct Caret([Span; 1])        => "^"),
        (pub struct CaretEq([Span; 2])      => "^="),
        (pub struct Colon([Span; 1])        => ":"),
        (pub struct Colon2([Span; 2])       => "::"),
        (pub struct Comma([Span; 1])        => ","),
        (pub struct Div([Span; 1])          => "/"),
        (pub struct DivEq([Span; 2])        => "/="),
        (pub struct Dot([Span; 1])          => "."),
        (pub struct Dot2([Span; 2])         => ".."),
        (pub struct Dot3([Span; 3])         => "..."),
        (pub struct Eq([Span; 1])           => "="),
        (pub struct EqEq([Span; 2])         => "=="),
        (pub struct Ge([Span; 2])           => ">="),
        (pub struct Gt([Span; 1])           => ">"),
        (pub struct Le([Span; 2])           => "<="),
        (pub struct Lt([Span; 1])           => "<"),
        (pub struct MulEq([Span; 2])        => "*="),
        (pub struct Ne([Span; 2])           => "!="),
        (pub struct Or([Span; 1])           => "|"),
        (pub struct OrEq([Span; 2])         => "|="),
        (pub struct OrOr([Span; 2])         => "||"),
        (pub struct Pound([Span; 1])        => "#"),
        (pub struct Question([Span; 1])     => "?"),
        (pub struct RArrow([Span; 2])       => "->"),
        (pub struct Rem([Span; 1])          => "%"),
        (pub struct RemEq([Span; 2])        => "%="),
        (pub struct Rocket([Span; 2])       => "=>"),
        (pub struct Semi([Span; 1])         => ";"),
        (pub struct Shl([Span; 2])          => "<<"),
        (pub struct ShlEq([Span; 3])        => "<<="),
        (pub struct Shr([Span; 2])          => ">>"),
        (pub struct ShrEq([Span; 3])        => ">>="),
        (pub struct Star([Span; 1])         => "*"),
        (pub struct Sub([Span; 1])          => "-"),
        (pub struct SubEq([Span; 2])        => "-="),
        (pub struct Underscore([Span; 1])   => "_"),
    }
    delim: {
        (pub struct Brace                   => "{"),
        (pub struct Bracket                 => "["),
        (pub struct Paren                   => "("),
    }
    syms: {
        (pub struct As                      => "as"),
        (pub struct Box_                    => "box"),
        (pub struct Break                   => "break"),
        (pub struct CapSelf                 => "Self"),
        (pub struct Catch                   => "catch"),
        (pub struct Const                   => "const"),
        (pub struct Continue                => "continue"),
        (pub struct Crate                   => "crate"),
        (pub struct Default_                => "default"),
        (pub struct Do                      => "do"),
        (pub struct Else                    => "else"),
        (pub struct Enum                    => "enum"),
        (pub struct Extern                  => "extern"),
        (pub struct Fn_                     => "fn"),
        (pub struct For                     => "for"),
        (pub struct If                      => "if"),
        (pub struct Impl                    => "impl"),
        (pub struct In                      => "in"),
        (pub struct Let                     => "let"),
        (pub struct Loop                    => "loop"),
        (pub struct Match                   => "match"),
        (pub struct Mod                     => "mod"),
        (pub struct Move                    => "move"),
        (pub struct Mut                     => "mut"),
        (pub struct Pub                     => "pub"),
        (pub struct Ref                     => "ref"),
        (pub struct Return                  => "return"),
        (pub struct Self_                   => "self"),
        (pub struct Static                  => "static"),
        (pub struct Struct                  => "struct"),
        (pub struct Super                   => "super"),
        (pub struct Trait                   => "trait"),
        (pub struct Type                    => "type"),
        (pub struct Union                   => "union"),
        (pub struct Unsafe                  => "unsafe"),
        (pub struct Use                     => "use"),
        (pub struct Where                   => "where"),
        (pub struct While                   => "while"),
    }
}

#[cfg(feature = "parsing")]
mod parsing {
    use proc_macro2::{TokenTree, TokenKind, Delimiter, OpKind};

    use {PResult, Cursor, parse_error};
    use span::Span;

    pub trait FromSpans: Sized {
        fn from_spans(spans: &[Span]) -> Self;
    }

    impl FromSpans for [Span; 1] {
        fn from_spans(spans: &[Span]) -> Self {
            [spans[0]]
        }
    }

    impl FromSpans for [Span; 2] {
        fn from_spans(spans: &[Span]) -> Self {
            [spans[0], spans[1]]
        }
    }

    impl FromSpans for [Span; 3] {
        fn from_spans(spans: &[Span]) -> Self {
            [spans[0], spans[1], spans[2]]
        }
    }

    pub fn op<'a, T, R>(s: &str,
                        tokens: Cursor<'a>,
                        new: fn(T) -> R)
            -> PResult<'a, R>
        where T: FromSpans,
    {
        let mut spans = [Span::default(); 3];
        assert!(s.len() <= spans.len());
        let chars = s.chars();
        let mut it = tokens.iter();

        for (i, (ch, slot)) in chars.zip(&mut spans).enumerate() {
            let tok = match it.next() {
                Some(tok) => tok,
                _ => return parse_error(),
            };
            let kind = match tok.kind {
                TokenKind::Op(c, kind) if c == ch => kind,
                _ => return parse_error(),
            };
            if i != s.len() - 1 {
                match kind {
                    OpKind::Joint => {}
                    OpKind::Alone => return parse_error(),
                }
            }
            *slot = Span(tok.span);
        }
        Ok((it.as_slice(), new(T::from_spans(&spans))))
    }

    pub fn sym<'a, T>(sym: &str,
                      tokens: Cursor<'a>,
                      new: fn(Span) -> T)
        -> PResult<'a, T>
    {
        let mut tokens = tokens.iter();
        let (span, s) = match tokens.next() {
            Some(&TokenTree { span, kind: TokenKind::Word(sym) }) => (span, sym),
            _ => return parse_error(),
        };
        if s.as_str() == sym {
            Ok((tokens.as_slice(), new(Span(span))))
        } else {
            parse_error()
        }
    }

    pub fn delim<'a, F, R, T>(delim: &str,
                              tokens: Cursor<'a>,
                              new: fn(Span) -> T,
                              f: F)
        -> PResult<'a, (R, T)>
        where F: FnOnce(Cursor) -> PResult<R>
    {
        let delim = match delim {
            "(" => Delimiter::Parenthesis,
            "{" => Delimiter::Brace,
            "[" => Delimiter::Bracket,
            _ => panic!("unknown delimiter: {}", delim),
        };
        let mut tokens = tokens.iter();
        let (span, d, others) = match tokens.next() {
            Some(&TokenTree { span, kind: TokenKind::Sequence(d, ref rest) }) => {
                (span, d, rest)
            }
            _ => return parse_error(),
        };
        match (delim, d) {
            (Delimiter::Parenthesis, Delimiter::Parenthesis) |
            (Delimiter::Brace, Delimiter::Brace) |
            (Delimiter::Bracket, Delimiter::Bracket) => {}
            _ => return parse_error(),
        }

        // TODO: Need a custom type to avoid this allocation every time we try
        // this branch. (issue dtolnay/syn#148)
        let rest = others.clone().into_iter().collect::<Vec<_>>();
        match f(&rest) {
            Ok((remaining, ret)) => {
                if remaining.is_empty() {
                    Ok((tokens.as_slice(), (ret, new(Span(span)))))
                } else {
                    parse_error()
                }
            }
            Err(err) => Err(err),
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use proc_macro2::{TokenTree, TokenKind, OpKind};
    use quote::Tokens;

    use span::Span;

    pub fn op(s: &str, spans: &[Span], tokens: &mut Tokens) {
        assert_eq!(s.len(), spans.len());

        let mut chars = s.chars();
        let mut spans = spans.iter();
        let ch = chars.next_back().unwrap();
        let span = spans.next_back().unwrap();
        for (ch, span) in chars.zip(spans) {
            tokens.append(TokenTree {
                span: span.0,
                kind: TokenKind::Op(ch, OpKind::Joint),
            });
        }

        tokens.append(TokenTree {
            span: span.0,
            kind: TokenKind::Op(ch, OpKind::Alone),
        });
    }

    pub fn sym(s: &str, span: &Span, tokens: &mut Tokens) {
        tokens.append(TokenTree {
            span: span.0,
            kind: TokenKind::Word(s.into()),
        });
    }

    pub fn delim<F>(s: &str, span: &Span, tokens: &mut Tokens, f: F)
        where F: FnOnce(&mut Tokens)
    {
        tokens.append_delimited(s, span.0, f)
    }
}
