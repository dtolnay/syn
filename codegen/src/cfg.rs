use proc_macro2::TokenStream;
use quote::quote;
use syn_codegen::Features;

pub fn features<'a>(
    features: &Features,
    overriding_cfg: impl Into<Option<&'a str>>,
) -> TokenStream {
    let features = &features.any;
    let cfg = match features.len() {
        0 => None,
        1 => Some(quote! { cfg(feature = #(#features)*) }),
        _ => Some(quote! { cfg(any(#(feature = #features),*)) }),
    };
    match (cfg, overriding_cfg.into()) {
        (Some(cfg), Some(overriding_cfg)) => quote! {
            #[#cfg]
            #[cfg_attr(doc_cfg, doc(cfg(feature = #overriding_cfg)))]
        },
        (Some(cfg), None) => quote! {
            #[#cfg]
            #[cfg_attr(doc_cfg, doc(#cfg))]
        },
        (None, Some(overriding_cfg)) => quote! {
            #[cfg_attr(doc_cfg, doc(cfg(feature = #overriding_cfg)))]
        },
        (None, None) => TokenStream::new(),
    }
}
