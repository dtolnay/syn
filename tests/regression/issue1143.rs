#[test]
fn issue1143() {
    // Can *tolerate* a braced `use` item which has `::`-lead paths inside it
    let _: syn::File = syn::parse_quote! {
        #[cfg(foo)]
        pub(crate) use {
            ::fs::FsPool,
            ::std::fs,
        };

        #[cfg(foo)]
        fn bar() -> Result<()> {
            let pool = FsPool::default();
            let file =
                fs::OpenOptions::new()
                    .write(true)
                    .create_new(true)?
            ;
            let pooled_file = pool.write(file);
            // …
        }
    };
    // without affecting the classic parsing logic.
    let _: syn::ItemUse = syn::parse_quote! {
        #[cfg(codegen)]
        use {
            proc_macro2::{TokenStream as TokenStream2},
            quote::quote,
            syn::*,
        };
    };
    let _: syn::ItemUse = syn::parse_quote! {
        use ::fs::FsPool;
    };
}
