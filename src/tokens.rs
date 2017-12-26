//! Discrete tokens that can be parsed out by synom.
//!
//! This module contains a number of useful tokens like `+=` and `/` along with
//! keywords like `crate` and such. These structures are used to track the spans
//! of these tokens and all implment the `ToTokens` and `Synom` traits when the
//! corresponding feature is activated.

use Span;

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
        #[cfg_attr(feature = "extra-traits", derive(Eq, PartialEq, Hash))]
        #[derive(Default)]
        pub struct $name(pub $($contents)*);

        #[cfg(feature = "extra-traits")]
        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                let mut t = f.debug_tuple(stringify!($name));
                for span in &self.0 {
                    t.field(span);
                }
                t.finish()
            }
        }

        #[cfg(feature = "printing")]
        impl ::quote::ToTokens for $name {
            fn to_tokens(&self, tokens: &mut ::quote::Tokens) {
                printing::op($s, &self.0, tokens);
            }
        }

        #[cfg(feature = "parsing")]
        impl ::Synom for $name {
            fn parse(tokens: $crate::synom::Cursor) -> $crate::PResult<$name> {
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
            fn parse(tokens: $crate::synom::Cursor) -> $crate::PResult<$name> {
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
            pub fn parse<F, R>(tokens: $crate::synom::Cursor, f: F) -> $crate::PResult<(R, $name)>
                where F: FnOnce($crate::synom::Cursor) -> $crate::PResult<R>
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
        (pub struct DotDotEq([Span; 3])     => "..="),
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
        (pub struct LArrow([Span; 2])       => "<-"),
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
        (pub struct Group                   => " "),
    }
    syms: {
        (pub struct As                      => "as"),
        (pub struct Auto                    => "auto"),
        (pub struct Box                     => "box"),
        (pub struct Break                   => "break"),
        (pub struct CapSelf                 => "Self"),
        (pub struct Catch                   => "catch"),
        (pub struct Const                   => "const"),
        (pub struct Continue                => "continue"),
        (pub struct Crate                   => "crate"),
        (pub struct Default                 => "default"),
        (pub struct Do                      => "do"),
        (pub struct Dyn                     => "dyn"),
        (pub struct Else                    => "else"),
        (pub struct Enum                    => "enum"),
        (pub struct Extern                  => "extern"),
        (pub struct Fn                      => "fn"),
        (pub struct For                     => "for"),
        (pub struct If                      => "if"),
        (pub struct Impl                    => "impl"),
        (pub struct In                      => "in"),
        (pub struct Let                     => "let"),
        (pub struct Loop                    => "loop"),
        (pub struct Macro                   => "macro"),
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
        (pub struct Yield                   => "yield"),
    }
}

// Unfortunate duplication due to a rustdoc bug.
// https://github.com/rust-lang/rust/issues/45939
#[macro_export]
macro_rules! Token {
    (+)        => { $crate::tokens::Add };
    (+=)       => { $crate::tokens::AddEq };
    (&)        => { $crate::tokens::And };
    (&&)       => { $crate::tokens::AndAnd };
    (&=)       => { $crate::tokens::AndEq };
    (@)        => { $crate::tokens::At };
    (!)        => { $crate::tokens::Bang };
    (^)        => { $crate::tokens::Caret };
    (^=)       => { $crate::tokens::CaretEq };
    (:)        => { $crate::tokens::Colon };
    (::)       => { $crate::tokens::Colon2 };
    (,)        => { $crate::tokens::Comma };
    (/)        => { $crate::tokens::Div };
    (/=)       => { $crate::tokens::DivEq };
    (.)        => { $crate::tokens::Dot };
    (..)       => { $crate::tokens::Dot2 };
    (...)      => { $crate::tokens::Dot3 };
    (..=)      => { $crate::tokens::DotDotEq };
    (=)        => { $crate::tokens::Eq };
    (==)       => { $crate::tokens::EqEq };
    (>=)       => { $crate::tokens::Ge };
    (>)        => { $crate::tokens::Gt };
    (<=)       => { $crate::tokens::Le };
    (<)        => { $crate::tokens::Lt };
    (*=)       => { $crate::tokens::MulEq };
    (!=)       => { $crate::tokens::Ne };
    (|)        => { $crate::tokens::Or };
    (|=)       => { $crate::tokens::OrEq };
    (||)       => { $crate::tokens::OrOr };
    (#)        => { $crate::tokens::Pound };
    (?)        => { $crate::tokens::Question };
    (->)       => { $crate::tokens::RArrow };
    (<-)       => { $crate::tokens::LArrow };
    (%)        => { $crate::tokens::Rem };
    (%=)       => { $crate::tokens::RemEq };
    (=>)       => { $crate::tokens::Rocket };
    (;)        => { $crate::tokens::Semi };
    (<<)       => { $crate::tokens::Shl };
    (<<=)      => { $crate::tokens::ShlEq };
    (>>)       => { $crate::tokens::Shr };
    (>>=)      => { $crate::tokens::ShrEq };
    (*)        => { $crate::tokens::Star };
    (-)        => { $crate::tokens::Sub };
    (-=)       => { $crate::tokens::SubEq };
    (_)        => { $crate::tokens::Underscore };
    (as)       => { $crate::tokens::As };
    (auto)     => { $crate::tokens::Auto };
    (box)      => { $crate::tokens::Box };
    (break)    => { $crate::tokens::Break };
    (Self)     => { $crate::tokens::CapSelf };
    (catch)    => { $crate::tokens::Catch };
    (const)    => { $crate::tokens::Const };
    (continue) => { $crate::tokens::Continue };
    (crate)    => { $crate::tokens::Crate };
    (default)  => { $crate::tokens::Default };
    (do)       => { $crate::tokens::Do };
    (dyn)      => { $crate::tokens::Dyn };
    (else)     => { $crate::tokens::Else };
    (enum)     => { $crate::tokens::Enum };
    (extern)   => { $crate::tokens::Extern };
    (fn)       => { $crate::tokens::Fn };
    (for)      => { $crate::tokens::For };
    (if)       => { $crate::tokens::If };
    (impl)     => { $crate::tokens::Impl };
    (in)       => { $crate::tokens::In };
    (let)      => { $crate::tokens::Let };
    (loop)     => { $crate::tokens::Loop };
    (macro)    => { $crate::tokens::Macro };
    (match)    => { $crate::tokens::Match };
    (mod)      => { $crate::tokens::Mod };
    (move)     => { $crate::tokens::Move };
    (mut)      => { $crate::tokens::Mut };
    (pub)      => { $crate::tokens::Pub };
    (ref)      => { $crate::tokens::Ref };
    (return)   => { $crate::tokens::Return };
    (self)     => { $crate::tokens::Self_ };
    (static)   => { $crate::tokens::Static };
    (struct)   => { $crate::tokens::Struct };
    (super)    => { $crate::tokens::Super };
    (trait)    => { $crate::tokens::Trait };
    (type)     => { $crate::tokens::Type };
    (union)    => { $crate::tokens::Union };
    (unsafe)   => { $crate::tokens::Unsafe };
    (use)      => { $crate::tokens::Use };
    (where)    => { $crate::tokens::Where };
    (while)    => { $crate::tokens::While };
    (yield)    => { $crate::tokens::Yield };
}

#[macro_export]
macro_rules! punct {
    ($i:expr, +)   => { call!($i, <$crate::tokens::Add as $crate::synom::Synom>::parse) };
    ($i:expr, +=)  => { call!($i, <$crate::tokens::AddEq as $crate::synom::Synom>::parse) };
    ($i:expr, &)   => { call!($i, <$crate::tokens::And as $crate::synom::Synom>::parse) };
    ($i:expr, &&)  => { call!($i, <$crate::tokens::AndAnd as $crate::synom::Synom>::parse) };
    ($i:expr, &=)  => { call!($i, <$crate::tokens::AndEq as $crate::synom::Synom>::parse) };
    ($i:expr, @)   => { call!($i, <$crate::tokens::At as $crate::synom::Synom>::parse) };
    ($i:expr, !)   => { call!($i, <$crate::tokens::Bang as $crate::synom::Synom>::parse) };
    ($i:expr, ^)   => { call!($i, <$crate::tokens::Caret as $crate::synom::Synom>::parse) };
    ($i:expr, ^=)  => { call!($i, <$crate::tokens::CaretEq as $crate::synom::Synom>::parse) };
    ($i:expr, :)   => { call!($i, <$crate::tokens::Colon as $crate::synom::Synom>::parse) };
    ($i:expr, ::)  => { call!($i, <$crate::tokens::Colon2 as $crate::synom::Synom>::parse) };
    ($i:expr, ,)   => { call!($i, <$crate::tokens::Comma as $crate::synom::Synom>::parse) };
    ($i:expr, /)   => { call!($i, <$crate::tokens::Div as $crate::synom::Synom>::parse) };
    ($i:expr, /=)  => { call!($i, <$crate::tokens::DivEq as $crate::synom::Synom>::parse) };
    ($i:expr, .)   => { call!($i, <$crate::tokens::Dot as $crate::synom::Synom>::parse) };
    ($i:expr, ..)  => { call!($i, <$crate::tokens::Dot2 as $crate::synom::Synom>::parse) };
    ($i:expr, ...) => { call!($i, <$crate::tokens::Dot3 as $crate::synom::Synom>::parse) };
    ($i:expr, ..=) => { call!($i, <$crate::tokens::DotDotEq as $crate::synom::Synom>::parse) };
    ($i:expr, =)   => { call!($i, <$crate::tokens::Eq as $crate::synom::Synom>::parse) };
    ($i:expr, ==)  => { call!($i, <$crate::tokens::EqEq as $crate::synom::Synom>::parse) };
    ($i:expr, >=)  => { call!($i, <$crate::tokens::Ge as $crate::synom::Synom>::parse) };
    ($i:expr, >)   => { call!($i, <$crate::tokens::Gt as $crate::synom::Synom>::parse) };
    ($i:expr, <=)  => { call!($i, <$crate::tokens::Le as $crate::synom::Synom>::parse) };
    ($i:expr, <)   => { call!($i, <$crate::tokens::Lt as $crate::synom::Synom>::parse) };
    ($i:expr, *=)  => { call!($i, <$crate::tokens::MulEq as $crate::synom::Synom>::parse) };
    ($i:expr, !=)  => { call!($i, <$crate::tokens::Ne as $crate::synom::Synom>::parse) };
    ($i:expr, |)   => { call!($i, <$crate::tokens::Or as $crate::synom::Synom>::parse) };
    ($i:expr, |=)  => { call!($i, <$crate::tokens::OrEq as $crate::synom::Synom>::parse) };
    ($i:expr, ||)  => { call!($i, <$crate::tokens::OrOr as $crate::synom::Synom>::parse) };
    ($i:expr, #)   => { call!($i, <$crate::tokens::Pound as $crate::synom::Synom>::parse) };
    ($i:expr, ?)   => { call!($i, <$crate::tokens::Question as $crate::synom::Synom>::parse) };
    ($i:expr, ->)  => { call!($i, <$crate::tokens::RArrow as $crate::synom::Synom>::parse) };
    ($i:expr, <-)  => { call!($i, <$crate::tokens::LArrow as $crate::synom::Synom>::parse) };
    ($i:expr, %)   => { call!($i, <$crate::tokens::Rem as $crate::synom::Synom>::parse) };
    ($i:expr, %=)  => { call!($i, <$crate::tokens::RemEq as $crate::synom::Synom>::parse) };
    ($i:expr, =>)  => { call!($i, <$crate::tokens::Rocket as $crate::synom::Synom>::parse) };
    ($i:expr, ;)   => { call!($i, <$crate::tokens::Semi as $crate::synom::Synom>::parse) };
    ($i:expr, <<)  => { call!($i, <$crate::tokens::Shl as $crate::synom::Synom>::parse) };
    ($i:expr, <<=) => { call!($i, <$crate::tokens::ShlEq as $crate::synom::Synom>::parse) };
    ($i:expr, >>)  => { call!($i, <$crate::tokens::Shr as $crate::synom::Synom>::parse) };
    ($i:expr, >>=) => { call!($i, <$crate::tokens::ShrEq as $crate::synom::Synom>::parse) };
    ($i:expr, *)   => { call!($i, <$crate::tokens::Star as $crate::synom::Synom>::parse) };
    ($i:expr, -)   => { call!($i, <$crate::tokens::Sub as $crate::synom::Synom>::parse) };
    ($i:expr, -=)  => { call!($i, <$crate::tokens::SubEq as $crate::synom::Synom>::parse) };
    ($i:expr, _)   => { call!($i, <$crate::tokens::Underscore as $crate::synom::Synom>::parse) };
}

#[macro_export]
macro_rules! keyword {
    ($i:expr, as)       => { call!($i, <$crate::tokens::As as $crate::synom::Synom>::parse) };
    ($i:expr, auto)     => { call!($i, <$crate::tokens::Auto as $crate::synom::Synom>::parse) };
    ($i:expr, box)      => { call!($i, <$crate::tokens::Box as $crate::synom::Synom>::parse) };
    ($i:expr, break)    => { call!($i, <$crate::tokens::Break as $crate::synom::Synom>::parse) };
    ($i:expr, Self)     => { call!($i, <$crate::tokens::CapSelf as $crate::synom::Synom>::parse) };
    ($i:expr, catch)    => { call!($i, <$crate::tokens::Catch as $crate::synom::Synom>::parse) };
    ($i:expr, const)    => { call!($i, <$crate::tokens::Const as $crate::synom::Synom>::parse) };
    ($i:expr, continue) => { call!($i, <$crate::tokens::Continue as $crate::synom::Synom>::parse) };
    ($i:expr, crate)    => { call!($i, <$crate::tokens::Crate as $crate::synom::Synom>::parse) };
    ($i:expr, default)  => { call!($i, <$crate::tokens::Default as $crate::synom::Synom>::parse) };
    ($i:expr, do)       => { call!($i, <$crate::tokens::Do as $crate::synom::Synom>::parse) };
    ($i:expr, dyn)      => { call!($i, <$crate::tokens::Dyn as $crate::synom::Synom>::parse) };
    ($i:expr, else)     => { call!($i, <$crate::tokens::Else as $crate::synom::Synom>::parse) };
    ($i:expr, enum)     => { call!($i, <$crate::tokens::Enum as $crate::synom::Synom>::parse) };
    ($i:expr, extern)   => { call!($i, <$crate::tokens::Extern as $crate::synom::Synom>::parse) };
    ($i:expr, fn)       => { call!($i, <$crate::tokens::Fn as $crate::synom::Synom>::parse) };
    ($i:expr, for)      => { call!($i, <$crate::tokens::For as $crate::synom::Synom>::parse) };
    ($i:expr, if)       => { call!($i, <$crate::tokens::If as $crate::synom::Synom>::parse) };
    ($i:expr, impl)     => { call!($i, <$crate::tokens::Impl as $crate::synom::Synom>::parse) };
    ($i:expr, in)       => { call!($i, <$crate::tokens::In as $crate::synom::Synom>::parse) };
    ($i:expr, let)      => { call!($i, <$crate::tokens::Let as $crate::synom::Synom>::parse) };
    ($i:expr, loop)     => { call!($i, <$crate::tokens::Loop as $crate::synom::Synom>::parse) };
    ($i:expr, macro)    => { call!($i, <$crate::tokens::Macro as $crate::synom::Synom>::parse) };
    ($i:expr, match)    => { call!($i, <$crate::tokens::Match as $crate::synom::Synom>::parse) };
    ($i:expr, mod)      => { call!($i, <$crate::tokens::Mod as $crate::synom::Synom>::parse) };
    ($i:expr, move)     => { call!($i, <$crate::tokens::Move as $crate::synom::Synom>::parse) };
    ($i:expr, mut)      => { call!($i, <$crate::tokens::Mut as $crate::synom::Synom>::parse) };
    ($i:expr, pub)      => { call!($i, <$crate::tokens::Pub as $crate::synom::Synom>::parse) };
    ($i:expr, ref)      => { call!($i, <$crate::tokens::Ref as $crate::synom::Synom>::parse) };
    ($i:expr, return)   => { call!($i, <$crate::tokens::Return as $crate::synom::Synom>::parse) };
    ($i:expr, self)     => { call!($i, <$crate::tokens::Self_ as $crate::synom::Synom>::parse) };
    ($i:expr, static)   => { call!($i, <$crate::tokens::Static as $crate::synom::Synom>::parse) };
    ($i:expr, struct)   => { call!($i, <$crate::tokens::Struct as $crate::synom::Synom>::parse) };
    ($i:expr, super)    => { call!($i, <$crate::tokens::Super as $crate::synom::Synom>::parse) };
    ($i:expr, trait)    => { call!($i, <$crate::tokens::Trait as $crate::synom::Synom>::parse) };
    ($i:expr, type)     => { call!($i, <$crate::tokens::Type as $crate::synom::Synom>::parse) };
    ($i:expr, union)    => { call!($i, <$crate::tokens::Union as $crate::synom::Synom>::parse) };
    ($i:expr, unsafe)   => { call!($i, <$crate::tokens::Unsafe as $crate::synom::Synom>::parse) };
    ($i:expr, use)      => { call!($i, <$crate::tokens::Use as $crate::synom::Synom>::parse) };
    ($i:expr, where)    => { call!($i, <$crate::tokens::Where as $crate::synom::Synom>::parse) };
    ($i:expr, while)    => { call!($i, <$crate::tokens::While as $crate::synom::Synom>::parse) };
    ($i:expr, yield)    => { call!($i, <$crate::tokens::Yield as $crate::synom::Synom>::parse) };
}

#[cfg(feature = "parsing")]
mod parsing {
    use proc_macro2::{Delimiter, Spacing};

    use cursor::Cursor;
    use {PResult, Span, parse_error};

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
                        mut tokens: Cursor<'a>,
                        new: fn(T) -> R)
            -> PResult<'a, R>
        where T: FromSpans,
    {
        let mut spans = [Span::default(); 3];
        assert!(s.len() <= spans.len());
        let chars = s.chars();

        for (i, (ch, slot)) in chars.zip(&mut spans).enumerate() {
            match tokens.op() {
                Some((rest, span, c, kind)) if c == ch => {
                    if i != s.len() - 1 {
                        match kind {
                            Spacing::Joint => {}
                            _ => return parse_error(),
                        }
                    }
                    *slot = Span(span);
                    tokens = rest;
                }
                _ => return parse_error()
            }
        }
        Ok((tokens, new(T::from_spans(&spans))))
    }

    pub fn sym<'a, T>(sym: &str,
                      tokens: Cursor<'a>,
                      new: fn(Span) -> T)
        -> PResult<'a, T>
    {
        if let Some((rest, span, s)) = tokens.word() {
            if s.as_str() == sym {
                return Ok((rest, new(Span(span))));
            }
        }
        parse_error()
    }

    pub fn delim<'a, F, R, T>(delim: &str,
                              tokens: Cursor<'a>,
                              new: fn(Span) -> T,
                              f: F)
        -> PResult<'a, (R, T)>
        where F: FnOnce(Cursor) -> PResult<R>
    {
        // NOTE: We should support none-delimited sequences here.
        let delim = match delim {
            "(" => Delimiter::Parenthesis,
            "{" => Delimiter::Brace,
            "[" => Delimiter::Bracket,
            " " => Delimiter::None,
            _ => panic!("unknown delimiter: {}", delim),
        };

        if let Some(seqinfo) = tokens.seq(delim) {
            match f(seqinfo.inside) {
                Ok((remaining, ret)) => {
                    if remaining.eof() {
                        return Ok((seqinfo.outside, (ret, new(Span(seqinfo.span)))));
                    }
                }
                Err(err) => return Err(err),
            }
        }
        parse_error()
    }
}

#[cfg(feature = "printing")]
mod printing {
    use proc_macro2::{TokenTree, TokenNode, Spacing, Term};
    use quote::Tokens;

    use Span;

    pub fn op(s: &str, spans: &[Span], tokens: &mut Tokens) {
        assert_eq!(s.len(), spans.len());

        let mut chars = s.chars();
        let mut spans = spans.iter();
        let ch = chars.next_back().unwrap();
        let span = spans.next_back().unwrap();
        for (ch, span) in chars.zip(spans) {
            tokens.append(TokenTree {
                span: span.0,
                kind: TokenNode::Op(ch, Spacing::Joint),
            });
        }

        tokens.append(TokenTree {
            span: span.0,
            kind: TokenNode::Op(ch, Spacing::Alone),
        });
    }

    pub fn sym(s: &str, span: &Span, tokens: &mut Tokens) {
        tokens.append(TokenTree {
            span: span.0,
            kind: TokenNode::Term(Term::intern(s)),
        });
    }

    pub fn delim<F>(s: &str, span: &Span, tokens: &mut Tokens, f: F)
        where F: FnOnce(&mut Tokens)
    {
        tokens.append_delimited(s, span.0, f)
    }
}
