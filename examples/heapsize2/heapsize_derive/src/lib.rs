extern crate proc_macro;
use proc_macro::TokenStream;

extern crate proc_macro2;
use proc_macro2::Span;

extern crate syn;
use syn::DeriveInput;

#[macro_use]
extern crate quote;

extern crate heapsize;
use heapsize::HeapSize;

#[proc_macro_derive(HeapSize)]
pub fn derive_heap_size(input: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse(input).unwrap();
    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let expanded = quote_spanned! {
        Span::def_site(),
        impl #impl_generics HeapSize for #name #ty_generics #where_clause {
            fn heap_size_of_children(&self) -> usize {
                0
            }
        }
    };

    expanded.into()
}
