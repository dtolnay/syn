#[cfg(not(syn_can_match_ident_after_attrs))]
macro_rules! ast_struct {
    (
        $(#[$attr:meta])*
        pub struct $name:ident #full $($rest:tt)*
    ) => {
        #[cfg(feature = "full")]
        $(#[$attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        pub struct $name $($rest)*

        #[cfg(not(feature = "full"))]
        $(#[$attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        pub struct $name {
            _noconstruct: (),
        }
    };

    (
        $(#[$attr:meta])*
        pub struct $name:ident #manual_extra_traits $($rest:tt)*
    ) => {
        $(#[$attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        pub struct $name $($rest)*
    };

    (
        $(#[$attr:meta])*
        pub struct $name:ident $($rest:tt)*
    ) => {
        $(#[$attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        pub struct $name $($rest)*
    };
}

#[cfg(syn_can_match_ident_after_attrs)]
macro_rules! ast_struct {
    (
        $(#[$attr:meta])*
        $pub:ident $struct:ident $name:ident #full $($rest:tt)*
    ) => {
        check_keyword_matches!(pub $pub);
        check_keyword_matches!(struct $struct);

        #[cfg(feature = "full")]
        $(#[$attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $pub $struct $name $($rest)*

        #[cfg(not(feature = "full"))]
        $(#[$attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $pub $struct $name {
            _noconstruct: (),
        }
    };

    (
        $(#[$attr:meta])*
        $pub:ident $struct:ident $name:ident #manual_extra_traits $($rest:tt)*
    ) => {
        check_keyword_matches!(pub $pub);
        check_keyword_matches!(struct $struct);

        $(#[$attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $pub $struct $name $($rest)*
    };

    (
        $(#[$attr:meta])*
        $pub:ident $struct:ident $name:ident $($rest:tt)*
    ) => {
        check_keyword_matches!(pub $pub);
        check_keyword_matches!(struct $struct);

        $(#[$attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $pub $struct $name $($rest)*
    };
}


#[cfg(not(syn_can_match_ident_after_attrs))]
macro_rules! ast_enum {
    // Drop the `#no_visit` attribute, if present.
    (
        $(#[$enum_attr:meta])*
        pub enum $name:ident #no_visit $($rest:tt)*
    ) => (
        ast_enum! { $(#[$enum_attr])* pub enum $name $($rest)* }
    );

    (
        $(#[$enum_attr:meta])*
        pub enum $name:ident $($rest:tt)*
    ) => (
        $(#[$enum_attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        pub enum $name $($rest)*
    );
}

#[cfg(syn_can_match_ident_after_attrs)]
macro_rules! ast_enum {
    // Drop the `#no_visit` attribute, if present.
    (
        $(#[$enum_attr:meta])*
        $pub:ident $enum:ident $name:ident #no_visit $($rest:tt)*
    ) => (
        ast_enum! { $(#[$enum_attr])* $pub $enum $name $($rest)* }
    );

    (
        $(#[$enum_attr:meta])*
        $pub:ident $enum:ident $name:ident $($rest:tt)*
    ) => (
        check_keyword_matches!(pub $pub);
        check_keyword_matches!(enum $enum);

        $(#[$enum_attr])*
        #[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
        #[cfg_attr(feature = "clone-impls", derive(Clone))]
        $pub $enum $name $($rest)*
    );
}

#[cfg(not(syn_can_match_ident_after_attrs))]
macro_rules! ast_enum_of_structs {
    (
        $(#[$enum_attr:meta])*
        pub enum $name:ident {
            $(
                $(#[$variant_attr:meta])*
                pub $variant:ident $( ($member:ident $($rest:tt)*) )*,
            )*
        }

        $($remaining:tt)*
    ) => (
        ast_enum! {
            $(#[$enum_attr])*
            pub enum $name {
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

#[cfg(syn_can_match_ident_after_attrs)]
macro_rules! ast_enum_of_structs {
    (
        $(#[$enum_attr:meta])*
        $pub:ident $enum:ident $name:ident {
            $(
                $(#[$variant_attr:meta])*
                $vpub:ident $variant:ident $( ($member:ident $($rest:tt)*) )*,
            )*
        }

        $($remaining:tt)*
    ) => (
        check_keyword_matches!(pub $pub);
        check_keyword_matches!(enum $enum);

        ast_enum! {
            $(#[$enum_attr])*
            $pub $enum $name {
                $(
                    $(#[$variant_attr])*
                    $variant $( ($member) )*,
                )*
            }
        }

        $(
            check_keyword_matches!(pub $vpub);
            maybe_ast_struct! {
                $(#[$variant_attr])*
                $(
                    $vpub struct $member $($rest)*
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

#[cfg(syn_can_match_ident_after_attrs)]
macro_rules! check_keyword_matches {
    (struct struct) => {};
    (enum enum) => {};
    (pub pub) => {};
}
