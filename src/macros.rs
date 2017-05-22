macro_rules! ast_struct {
    (
        $(#[$attr:meta])*
        pub struct $name:ident {
            $(
                $(#[$field_attr:meta])*
                pub $field:ident: $ty:ty,
            )*
        }
    ) => {
        $(#[$attr])*
        #[derive(Debug, Clone, Eq, PartialEq, Hash)]
        pub struct $name {
            $(
                $(#[$field_attr])*
                pub $field: $ty,
            )*
        }
    }
}

macro_rules! ast_enum {
    (
        $(#[$enum_attr:meta])*
        pub enum $name:ident { $($variants:tt)* }
    ) => (
        $(#[$enum_attr])*
        #[derive(Debug, Clone, Eq, PartialEq, Hash)]
        pub enum $name {
            $($variants)*
        }
    )
}

macro_rules! ast_enum_of_structs {
    (
        $(#[$enum_attr:meta])*
        pub enum $name:ident {
            $(
                $(#[$variant_attr:meta])*
                pub $variant:ident($member:ident $($rest:tt)*),
            )*
        }

        $($remaining:tt)*
    ) => (
        ast_enum! {
            $(#[$enum_attr])*
            pub enum $name {
                $(
                    $(#[$variant_attr])*
                    $variant($member),
                )*
            }
        }

        $(
            maybe_ast_struct! {
                $(#[$variant_attr])*
                pub struct $member $($rest)*
            }

            impl From<$member> for $name {
                fn from(e: $member) -> $name {
                    $name::$variant(e)
                }
            }
        )*

        generate_to_tokens! {
            $($remaining)*
            enum $name { $($variant,)* }
        }
    )
}

macro_rules! generate_to_tokens {
    (do_not_generate_to_tokens $($foo:tt)*) => ();

    (enum $name:ident { $($variant:ident,)* }) => (
        #[cfg(feature = "printing")]
        impl ::quote::ToTokens for $name {
            fn to_tokens(&self, tokens: &mut ::quote::Tokens) {
                match *self {
                    $(
                        $name::$variant(ref e) => e.to_tokens(tokens),
                    )*
                }
            }
        }
    )
}

macro_rules! maybe_ast_struct {
    (
        $(#[$attr:meta])*
        pub struct $name:ident
    ) => ();

    ($($rest:tt)*) => (ast_struct! { $($rest)* });
}
