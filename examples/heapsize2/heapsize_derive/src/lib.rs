extern crate proc_macro;
extern crate proc_macro2;

#[macro_use]
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro2::{Span, TokenStream};
use syn::{DeriveInput, Data, Fields, Generics, GenericParam, Index};
use syn::spanned::Spanned;

#[proc_macro_derive(HeapSize)]
pub fn derive_heap_size(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the input tokens into a syntax tree.
    let input: DeriveInput = syn::parse(input).unwrap();

    // Used in the quasi-quotation below as `#name`.
    let name = input.ident;

    // Add a bound `T: HeapSize` to every type parameter T.
    let generics = add_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // Generate an expression to sum up the heap size of each field.
    let var = quote!(self);
    let sum = heap_size_sum(&input.data, &var);

    let expanded = quote! {
        // The generated impl.
        impl #impl_generics ::heapsize::HeapSize for #name #ty_generics #where_clause {
            fn heap_size_of_children(&#var) -> usize {
                #sum
            }
        }
    };

    // Hand the output tokens back to the compiler.
    expanded.into()
}

// Add a bound `T: HeapSize` to every type parameter T.
fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(::heapsize::HeapSize));
        }
    }
    generics
}

// Generate an expression to sum up the heap size of each field.
fn heap_size_sum(data: &Data, var: &TokenStream) -> TokenStream {
    let call_site = Span::call_site();

    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    // Expands to an expression like
                    //
                    //     0 + HeapSize::heap_size(&self.x) + HeapSize::heap_size(&self.y)
                    //
                    // We take some care to use the span of each `syn::Field` as
                    // the span of the corresponding `heap_size_of_children`
                    // call. This way if one of the field types does not
                    // implement `HeapSize` then the compiler's error message
                    // underlines which field it is. An example is shown in the
                    // readme of the parent directory.
                    let recurse = fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let access = quote_spanned!(call_site=> #var.#name);
                        quote_spanned! {f.span()=>
                            ::heapsize::HeapSize::heap_size_of_children(&#access)
                        }
                    });
                    quote! {
                        0 #(+ #recurse)*
                    }
                }
                Fields::Unnamed(ref fields) => {
                    // This expands to an expression like
                    //
                    //     0 + HeapSize::heap_size(&self.0) + HeapSize::heap_size(&self.1)
                    let recurse = fields.unnamed.iter().enumerate().map(|(i, f)| {
                        let index = Index { index: i as u32, span: call_site };
                        let access = quote_spanned!(call_site=> #var.#index);
                        quote_spanned! {f.span()=>
                            ::heapsize::HeapSize::heap_size_of_children(&#access)
                        }
                    });
                    quote! {
                        0 #(+ #recurse)*
                    }
                }
                Fields::Unit => {
                    // Unit structs cannot own more than 0 bytes of heap memory.
                    quote!(0)
                }
            }
        }
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}
