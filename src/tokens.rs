//! Discrete tokens that can be parsed out by synom.
//!
//! This module contains a number of useful tokens like `+=` and `/` along with
//! keywords like `crate` and such. These structures are used to track the spans
//! of these tokens and all implment the `ToTokens` and `Synom` traits when the
//! corresponding feature is activated.

use proc_macro2::Span;

macro_rules! tokens {
    (
        ops: {
            $($op:tt pub struct $op_name:ident/$len:tt #[$op_doc:meta])*
        }
        delim: {
            $($delim:tt pub struct $delim_name:ident #[$delim_doc:meta])*
        }
        syms: {
            $($sym:tt pub struct $sym_name:ident #[$sym_doc:meta])*
        }
    ) => (
        $(op! { #[$op_doc] $op pub struct $op_name/$len })*
        $(delim! { #[$delim_doc] $delim pub struct $delim_name })*
        $(sym! { #[$sym_doc] $sym pub struct $sym_name })*
    )
}

macro_rules! op {
    (#[$doc:meta] $s:tt pub struct $name:ident/$len:tt) => {
        #[cfg_attr(feature = "clone-impls", derive(Copy, Clone))]
        #[derive(Default)]
        #[$doc]
        pub struct $name(pub [Span; $len]);

        #[cfg(feature = "extra-traits")]
        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                f.write_str(stringify!($name))
            }
        }

        #[cfg(feature = "extra-traits")]
        impl ::std::cmp::Eq for $name {}

        #[cfg(feature = "extra-traits")]
        impl ::std::cmp::PartialEq for $name {
            fn eq(&self, _other: &$name) -> bool {
                true
            }
        }

        #[cfg(feature = "extra-traits")]
        impl ::std::hash::Hash for $name {
            fn hash<H>(&self, _state: &mut H)
                where H: ::std::hash::Hasher
            {}
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
    (#[$doc:meta] $s:tt pub struct $name:ident) => {
        #[cfg_attr(feature = "clone-impls", derive(Copy, Clone))]
        #[derive(Default)]
        #[$doc]
        pub struct $name(pub Span);

        #[cfg(feature = "extra-traits")]
        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                f.write_str(stringify!($name))
            }
        }

        #[cfg(feature = "extra-traits")]
        impl ::std::cmp::Eq for $name {}

        #[cfg(feature = "extra-traits")]
        impl ::std::cmp::PartialEq for $name {
            fn eq(&self, _other: &$name) -> bool {
                true
            }
        }

        #[cfg(feature = "extra-traits")]
        impl ::std::hash::Hash for $name {
            fn hash<H>(&self, _state: &mut H)
                where H: ::std::hash::Hasher
            {}
        }

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
    (#[$doc:meta] $s:tt pub struct $name:ident) => {
        #[cfg_attr(feature = "clone-impls", derive(Copy, Clone))]
        #[derive(Default)]
        #[$doc]
        pub struct $name(pub Span);

        #[cfg(feature = "extra-traits")]
        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                f.write_str(stringify!($name))
            }
        }

        #[cfg(feature = "extra-traits")]
        impl ::std::cmp::Eq for $name {}

        #[cfg(feature = "extra-traits")]
        impl ::std::cmp::PartialEq for $name {
            fn eq(&self, _other: &$name) -> bool {
                true
            }
        }

        #[cfg(feature = "extra-traits")]
        impl ::std::hash::Hash for $name {
            fn hash<H>(&self, _state: &mut H)
                where H: ::std::hash::Hasher
            {}
        }

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
        "+"        pub struct Add/1        /// `+`
        "+="       pub struct AddEq/2      /// `+=`
        "&"        pub struct And/1        /// `&`
        "&&"       pub struct AndAnd/2     /// `&&`
        "&="       pub struct AndEq/2      /// `&=`
        "@"        pub struct At/1         /// `@`
        "!"        pub struct Bang/1       /// `!`
        "^"        pub struct Caret/1      /// `^`
        "^-"       pub struct CaretEq/2    /// `^=`
        ":"        pub struct Colon/1      /// `:`
        "::"       pub struct Colon2/2     /// `::`
        ","        pub struct Comma/1      /// `,`
        "/"        pub struct Div/1        /// `/`
        "/="       pub struct DivEq/2      /// `/=`
        "."        pub struct Dot/1        /// `.`
        ".."       pub struct Dot2/2       /// `..`
        "..."      pub struct Dot3/3       /// `...`
        "..="      pub struct DotDotEq/3   /// `..=`
        "="        pub struct Eq/1         /// `=`
        "=="       pub struct EqEq/2       /// `==`
        ">="       pub struct Ge/2         /// `>=`
        ">"        pub struct Gt/1         /// `>`
        "<="       pub struct Le/2         /// `<=`
        "<"        pub struct Lt/1         /// `<`
        "*="       pub struct MulEq/2      /// `*=`
        "!="       pub struct Ne/2         /// `!=`
        "|"        pub struct Or/1         /// `|`
        "|="       pub struct OrEq/2       /// `|=`
        "||"       pub struct OrOr/2       /// `||`
        "#"        pub struct Pound/1      /// `#`
        "?"        pub struct Question/1   /// `?`
        "->"       pub struct RArrow/2     /// `->`
        "<-"       pub struct LArrow/2     /// `<-`
        "%"        pub struct Rem/1        /// `%`
        "%="       pub struct RemEq/2      /// `%=`
        "=>"       pub struct Rocket/2     /// `=>`
        ";"        pub struct Semi/1       /// `;`
        "<<"       pub struct Shl/2        /// `<<`
        "<<="      pub struct ShlEq/3      /// `<<=`
        ">>"       pub struct Shr/2        /// `>>`
        ">>="      pub struct ShrEq/3      /// `>>=`
        "*"        pub struct Star/1       /// `*`
        "-"        pub struct Sub/1        /// `-`
        "-="       pub struct SubEq/2      /// `-=`
        "_"        pub struct Underscore/1 /// `_`
    }
    delim: {
        "{"        pub struct Brace        /// `{...}`
        "["        pub struct Bracket      /// `[...]`
        "("        pub struct Paren        /// `(...)`
        " "        pub struct Group        /// None-delimited group
    }
    syms: {
        "as"       pub struct As           /// `as`
        "auto"     pub struct Auto         /// `auto`
        "box"      pub struct Box          /// `box`
        "break"    pub struct Break        /// `break`
        "Self"     pub struct CapSelf      /// `Self`
        "catch"    pub struct Catch        /// `catch`
        "const"    pub struct Const        /// `const`
        "continue" pub struct Continue     /// `continue`
        "crate"    pub struct Crate        /// `crate`
        "default"  pub struct Default      /// `default`
        "do"       pub struct Do           /// `do`
        "dyn"      pub struct Dyn          /// `dyn`
        "else"     pub struct Else         /// `else`
        "enum"     pub struct Enum         /// `enum`
        "extern"   pub struct Extern       /// `extern`
        "fn"       pub struct Fn           /// `fn`
        "for"      pub struct For          /// `for`
        "if"       pub struct If           /// `if`
        "impl"     pub struct Impl         /// `impl`
        "in"       pub struct In           /// `in`
        "let"      pub struct Let          /// `let`
        "loop"     pub struct Loop         /// `loop`
        "macro"    pub struct Macro        /// `macro`
        "match"    pub struct Match        /// `match`
        "mod"      pub struct Mod          /// `mod`
        "move"     pub struct Move         /// `move`
        "mut"      pub struct Mut          /// `mut`
        "pub"      pub struct Pub          /// `pub`
        "ref"      pub struct Ref          /// `ref`
        "return"   pub struct Return       /// `return`
        "self"     pub struct Self_        /// `self`
        "static"   pub struct Static       /// `static`
        "struct"   pub struct Struct       /// `struct`
        "super"    pub struct Super        /// `super`
        "trait"    pub struct Trait        /// `trait`
        "type"     pub struct Type         /// `type`
        "union"    pub struct Union        /// `union`
        "unsafe"   pub struct Unsafe       /// `unsafe`
        "use"      pub struct Use          /// `use`
        "where"    pub struct Where        /// `where`
        "while"    pub struct While        /// `while`
        "yield"    pub struct Yield        /// `yield`
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

#[cfg(feature = "parsing")]
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

#[cfg(feature = "parsing")]
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
    use proc_macro2::{Span, Delimiter, Spacing};

    use cursor::Cursor;
    use {PResult, parse_error};

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
                    *slot = span;
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
                return Ok((rest, new(span)));
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
                        return Ok((seqinfo.outside, (ret, new(seqinfo.span))));
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
    use proc_macro2::{Span, TokenTree, TokenNode, Spacing, Term};
    use quote::Tokens;

    pub fn op(s: &str, spans: &[Span], tokens: &mut Tokens) {
        assert_eq!(s.len(), spans.len());

        let mut chars = s.chars();
        let mut spans = spans.iter();
        let ch = chars.next_back().unwrap();
        let span = spans.next_back().unwrap();
        for (ch, span) in chars.zip(spans) {
            tokens.append(TokenTree {
                span: *span,
                kind: TokenNode::Op(ch, Spacing::Joint),
            });
        }

        tokens.append(TokenTree {
            span: *span,
            kind: TokenNode::Op(ch, Spacing::Alone),
        });
    }

    pub fn sym(s: &str, span: &Span, tokens: &mut Tokens) {
        tokens.append(TokenTree {
            span: *span,
            kind: TokenNode::Term(Term::intern(s)),
        });
    }

    pub fn delim<F>(s: &str, span: &Span, tokens: &mut Tokens, f: F)
        where F: FnOnce(&mut Tokens)
    {
        tokens.append_delimited(s, *span, f)
    }
}
