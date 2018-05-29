#![recursion_limit = "128"]
#![feature(proc_macro)]

#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;
extern crate proc_macro;

use syn::{Visibility, Ident, Type, Expr};
use syn::synom::Synom;
use syn::spanned::Spanned;
use proc_macro::TokenStream;

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

impl Synom for LazyStatic {
    named!(parse -> Self, do_parse!(
        visibility: syn!(Visibility) >>
        keyword!(static) >>
        keyword!(ref) >>
        name: syn!(Ident) >>
        punct!(:) >>
        ty: syn!(Type) >>
        punct!(=) >>
        init: syn!(Expr) >>
        punct!(;) >>
        (LazyStatic { visibility, name, ty, init })
    ));
}

#[proc_macro]
pub fn lazy_static(input: TokenStream) -> TokenStream {
    let LazyStatic { visibility, name, ty, init } = syn::parse(input).unwrap();

    // The warning looks like this.
    //
    //     warning: come on, pick a more creative name
    //       --> src/main.rs:10:16
    //        |
    //     10 |     static ref FOO: String = "lazy_static".to_owned();
    //        |                ^^^
    if name == "FOO" {
        name.span().unstable()
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
    if let Expr::Tuple(ref init) = init {
        if init.elems.is_empty() {
            init.span().unstable()
                .error("I can't think of a legitimate use for lazily initializing the value `()`")
                .emit();
            return TokenStream::new();
        }
    }

    // Assert that the static type implements Sync. If not, user sees an error
    // message like the following. We span this assertion with the field type's
    // line/column so that the error message appears in the correct place.
    //
    //     error[E0277]: the trait bound `*const (): std::marker::Sync` is not satisfied
    //       --> src/main.rs:10:21
    //        |
    //     10 |     static ref PTR: *const () = &();
    //        |                     ^^^^^^^^^ `*const ()` cannot be shared between threads safely
    let assert_sync = quote_spanned! {ty.span()=>
        struct _AssertSync where #ty: ::std::marker::Sync;
    };

    // Check for Sized. Not vital to check here, but the error message is less
    // confusing this way than if they get a Sized error in one of our
    // implementation details where it assumes Sized.
    //
    //     error[E0277]: the trait bound `str: std::marker::Sized` is not satisfied
    //       --> src/main.rs:10:19
    //        |
    //     10 |     static ref A: str = "";
    //        |                   ^^^ `str` does not have a constant size known at compile-time
    let assert_sized = quote_spanned! {ty.span()=>
        struct _AssertSized where #ty: ::std::marker::Sized;
    };

    let init_ptr = quote_spanned! {init.span()=>
        Box::into_raw(Box::new(#init))
    };

    let expanded = quote! {
        #visibility struct #name;

        impl ::std::ops::Deref for #name {
            type Target = #ty;

            fn deref(&self) -> &#ty {
                #assert_sync
                #assert_sized

                static ONCE: ::std::sync::Once = ::std::sync::ONCE_INIT;
                static mut VALUE: *mut #ty = 0 as *mut #ty;

                unsafe {
                    ONCE.call_once(|| VALUE = #init_ptr);
                    &*VALUE
                }
            }
        }
    };

    expanded.into()
}
