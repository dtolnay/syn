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

    if name == "FOO" {
        name.span().unstable()
            .warning("come on, pick a more creative name")
            .emit();
    }

    if let Type::Tuple(ref ty) = ty {
        if ty.elems.is_empty() {
            ty.span().unstable()
                .error("I can't think of a legitimate use for lazily initializing `()`")
                .emit();
            return TokenStream::empty();
        }
    }

    // FIXME: ty.span().resolved_at(def_site)
    let ty_span = ty.span();
    let assert_sync = quote_spanned! {ty_span,
        fn assert_sync<T: Sync>() {}
        let _ = assert_sync::<#ty>;
    };

    // FIXME: init.span().resolved_at(def_site)
    let init_span = init.span();
    let init_ptr = quote_spanned! {init_span,
        Box::into_raw(Box::new(#init))
    };

    let expanded = quote! {
        extern crate std;

        #visibility struct #name;

        impl std::ops::Deref for #name {
            type Target = #ty;

            fn deref(&self) -> &#ty {
                #assert_sync

                static ONCE: std::sync::Once = std::sync::ONCE_INIT;
                static mut VALUE: *const #ty = 0 as *const #ty;

                unsafe {
                    ONCE.call_once(|| VALUE = #init_ptr);
                    &*VALUE
                }
            }
        }
    };
    
    expanded.into()
}
