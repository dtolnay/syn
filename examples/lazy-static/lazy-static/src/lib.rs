#![feature(proc_macro_diagnostic)]

use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::{Expr, Ident, Token, Type, Visibility, parse_macro_input};

/// Parses the following syntax, which aligns with the input of the real
/// `lazy_static` crate.
///
///     lazy_static! {
///         $VISIBILITY static ref $NAME: $TYPE = $EXPR;
///     }
///
/// For example:
///
///     lazy_static! {
///         static ref USERNAME: Regex = Regex::new("^[a-z0-9_-]{3,16}$").unwrap();
///     }
struct LazyStatic {
    visibility: Visibility,
    name: Ident,
    ty: Type,
    init: Expr,
}

impl Parse for LazyStatic {
    fn parse(input: ParseStream) -> Result<Self> {
        let visibility: Visibility = input.parse()?;
        input.parse::<Token![static]>()?;
        input.parse::<Token![ref]>()?;
        let name: Ident = input.parse()?;
        input.parse::<Token![:]>()?;
        let ty: Type = input.parse()?;
        input.parse::<Token![=]>()?;
        let init: Expr = input.parse()?;
        input.parse::<Token![;]>()?;
        Ok(LazyStatic {
            visibility,
            name,
            ty,
            init,
        })
    }
}

#[proc_macro]
pub fn lazy_static(input: TokenStream) -> TokenStream {
    let LazyStatic {
        visibility,
        name,
        ty,
        init,
    } = parse_macro_input!(input as LazyStatic);

    // The warning looks like this.
    //
    //     warning: come on, pick a more creative name
    //       --> src/main.rs:10:16
    //        |
    //     10 |     static ref FOO: String = "lazy_static".to_owned();
    //        |                ^^^
    if name == "FOO" {
        name.span()
            .unwrap()
            .warning("come on, pick a more creative name")
            .emit();
    }

    // The error looks like this.
    //
    //     error: I can't think of a legitimate use for lazily initializing the value `()`
    //       --> src/main.rs:10:27
    //        |
    //     10 |     static ref UNIT: () = ();
    //        |                           ^^
    if let Expr::Tuple(init) = &init
        && init.elems.is_empty()
    {
        init.span()
            .unwrap()
            .error("I can't think of a legitimate use for lazily initializing the value `()`")
            .emit();
    }

    // Assert that the static type implements Sync. If not, user sees an error
    // message like the following. We span this assertion with the field type's
    // line/column so that the error message appears in the correct place.
    //
    //     error[E0277]: `*const ()` cannot be shared between threads safely
    //       --> src/main.rs:10:21
    //        |
    //     10 |     static ref PTR: *const () = &();
    //        |                     ^^^^^^^^^ lazy_static requires a type that implements Sync
    //
    // The trait MustBeSync is defined below using `on_unimplemented` to
    // customize the error message.
    let assert_sync = quote_spanned! {ty.span()=>
        <#ty as MustBeSync>::CHECK
    };

    let expanded = quote! {
        #visibility struct #name;

        const _: () = {
            #[diagnostic::on_unimplemented(
                message = "`{Self}` cannot be shared between threads safely",
                label = "lazy_static requires a type that implements Sync",
            )]
            trait MustBeSync {
                const CHECK: () = ();
            }
            #[diagnostic::do_not_recommend]
            impl<T: ?::std::marker::Sized + ::std::marker::Sync> MustBeSync for T {}
            #assert_sync
        };

        impl ::std::ops::Deref for #name {
            type Target = #ty;

            fn deref(&self) -> &Self::Target {
                static ONCE: ::std::sync::Once = ::std::sync::Once::new();
                static mut VALUE: *mut #ty = 0 as *mut #ty;

                let do_init = || {
                    let value: #ty = #init;
                    unsafe { VALUE = ::std::boxed::Box::into_raw(::std::boxed::Box::new(value)) }
                };

                unsafe {
                    ONCE.call_once(do_init);
                    &*VALUE
                }
            }
        }
    };

    TokenStream::from(expanded)
}
