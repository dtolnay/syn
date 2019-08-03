macro_rules! ast_struct {
    (
        @@ [$($attrs_pub:tt)*]
        struct $name:ident #full $($rest:tt)*
    ) => {
        #[cfg(feature = "full")]
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $($attrs_pub)* struct $name $($rest)*

        #[cfg(not(feature = "full"))]
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $($attrs_pub)* struct $name {
            _noconstruct: (),
        }
    };

    (
        @@ [$($attrs_pub:tt)*]
        struct $name:ident #manual_extra_traits $($rest:tt)*
    ) => {
        #[cfg_attr(feature = "extra-traits", derive(Debug))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $($attrs_pub)* struct $name $($rest)*
    };

    (
        @@ [$($attrs_pub:tt)*]
        struct $name:ident $($rest:tt)*
    ) => {
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $($attrs_pub)* struct $name $($rest)*
    };

    ($($t:tt)*) => {
        strip_attrs_pub!(ast_struct!($($t)*));
    };
}

macro_rules! ast_enum {
    // Drop the `#no_visit` attribute, if present.
    (
        @@ [$($attrs_pub:tt)*]
        enum $name:ident #no_visit $($rest:tt)*
    ) => (
        ast_enum!(@@ [$($attrs_pub)*] enum $name $($rest)*);
    );

    (
        @@ [$($attrs_pub:tt)*]
        enum $name:ident #manual_extra_traits $($rest:tt)*
    ) => (
        #[cfg_attr(feature = "extra-traits", derive(Debug))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $($attrs_pub)* enum $name $($rest)*
    );

    (
        @@ [$($attrs_pub:tt)*]
        enum $name:ident $($rest:tt)*
    ) => (
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $($attrs_pub)* enum $name $($rest)*
    );

    ($($t:tt)*) => {
        strip_attrs_pub!(ast_enum!($($t)*));
    };
}

// Unfortunately, at this time, we can't make the generated enum here have the
// correct span. The way that the span for the overall enum decl is calculated
// is using `lo.to(prev_span)` [1].
//
// If the beginning and ending spans of the enum item don't have the same macro
// context, fallback code is run [2]. This code chooses the span within the
// macro context to avoid firing spurious diagnostics [3].
//
// This means `[src]` links in rustdoc will point to the macro, instead of the
// actual declaration, if either the first or last token of our declaration has
// a span from the macro. With `macro_rules!` there is no way to preserve the
// span of a `{}` block while changing its contents, so we're forced to have the
// span of our final token point to our macro.
//
// [1]: https://github.com/rust-lang/rust/blob/9a90d03ad171856dc016c2dcc19292ec49a8a26f/src/libsyntax/parse/parser.rs#L7377
// [2]: https://github.com/rust-lang/rust/blob/9a90d03ad171856dc016c2dcc19292ec49a8a26f/src/libsyntax_pos/lib.rs#L467-L478
// [3]: https://github.com/rust-lang/rust/pull/47942
macro_rules! ast_enum_of_structs {
    (
        $(#[$enum_attr:meta])*
        $pub:ident $enum:ident $name:ident $(# $tags:ident)* {
            $(
                $(#[$variant_attr:meta])*
                $variant:ident $( ($member:ident $($rest:tt)*) )*,
            )*
        }

        $($remaining:tt)*
    ) => (
        check_keyword_matches!(pub $pub);
        check_keyword_matches!(enum $enum);

        ast_enum! {
            $(#[$enum_attr])*
            $pub $enum $name $(# $tags)* {
                $(
                    $(#[$variant_attr])*
                    $variant $( ($member) )*,
                )*
            }
        }

        $(
            maybe_ast_struct! {
                $(#[$variant_attr])*
                $(
                    pub struct $member $($rest)*
                )*
            }

            $(
                impl From<$member> for $name {
                    fn from(e: $member) -> $name {
                        $name::$variant(e)
                    }
                }
            )*
        )*

        #[cfg(feature = "printing")]
        generate_to_tokens! {
            $($remaining)*
            ()
            tokens
            $name { $($variant $( [$($rest)*] )*,)* }
        }
    )
}

#[cfg(feature = "printing")]
macro_rules! generate_to_tokens {
    (do_not_generate_to_tokens $($foo:tt)*) => ();

    (($($arms:tt)*) $tokens:ident $name:ident { $variant:ident, $($next:tt)*}) => {
        generate_to_tokens!(
            ($($arms)* $name::$variant => {})
            $tokens $name { $($next)* }
        );
    };

    (($($arms:tt)*) $tokens:ident $name:ident { $variant:ident [$($rest:tt)*], $($next:tt)*}) => {
        generate_to_tokens!(
            ($($arms)* $name::$variant(ref _e) => to_tokens_call!(_e, $tokens, $($rest)*),)
            $tokens $name { $($next)* }
        );
    };

    (($($arms:tt)*) $tokens:ident $name:ident {}) => {
        impl ::quote::ToTokens for $name {
            fn to_tokens(&self, $tokens: &mut ::proc_macro2::TokenStream) {
                match *self {
                    $($arms)*
                }
            }
        }
    };
}

#[cfg(all(feature = "printing", feature = "full"))]
macro_rules! to_tokens_call {
    ($e:ident, $tokens:ident, $($rest:tt)*) => {
        $e.to_tokens($tokens)
    };
}

#[cfg(all(feature = "printing", feature = "derive", not(feature = "full")))]
macro_rules! to_tokens_call {
    // If the variant is marked as #full, don't auto-generate to-tokens for it.
    ($e:ident, $tokens:ident, #full $($rest:tt)*) => {
        unreachable!()
    };
    ($e:ident, $tokens:ident, $($rest:tt)*) => {
        $e.to_tokens($tokens)
    };
}

macro_rules! maybe_ast_struct {
    (
        $(#[$attr:meta])*
        $(
            pub struct $name:ident
        )*
    ) => ();

    ($($rest:tt)*) => (ast_struct! { $($rest)* });
}

macro_rules! strip_attrs_pub {
    ($mac:ident!($(#[$m:meta])* $pub:ident $($t:tt)*)) => {
        check_keyword_matches!(pub $pub);

        $mac!(@@ [$(#[$m])* $pub] $($t)*);
    };
}

macro_rules! check_keyword_matches {
    (struct struct) => {};
    (enum enum) => {};
    (pub pub) => {};
}
