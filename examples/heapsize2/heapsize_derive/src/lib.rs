extern crate proc_macro;

#[macro_use]
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use syn::{DeriveInput, Data, Fields, Generics, GenericParam, Ident};
use syn::spanned::Spanned;
use quote::Tokens;

#[proc_macro_derive(HeapSize)]
pub fn derive_heap_size(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree.
    let input: DeriveInput = syn::parse(input).unwrap();

    // Used in the quasi-quotation below as `#name`.
    let name = input.ident;

    // Add a bound `T: HeapSize` to every type parameter T.
    let generics = add_trait_bounds(input.generics);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // Generate an expression to sum up the heap size of each field.
    let sum = heap_size_sum(&input.data);

    let expanded = quote! {
        mod scope {
            extern crate heapsize;
            use self::heapsize::HeapSize;

            // The generated impl. Okay to use `HeapSize` unqualified here, it
            // is guaranteed to resolve to the import on the previous line. This
            // works even in edge cases like the user's struct having the name
            // `HeapSize` as demonstrated in main.rs, in which case the
            // generated code looks like `impl HeapSize for HeapSize`.
            impl #impl_generics HeapSize for #name #ty_generics #where_clause {
                fn heap_size_of_children(&self) -> usize {
                    #sum
                }
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
            type_param.bounds.push(parse_quote!(HeapSize));
        }
    }
    generics
}

// Generate an expression to sum up the heap size of each field.
fn heap_size_sum(data: &Data) -> Tokens {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    // Expands to an expression like
                    //
                    //     0 + self.x.heap_size() + self.y.heap_size() + self.z.heap_size()
                    //
                    // Does not need to use fully qualified function call syntax
                    // like `::heapsize::HeapSize::heap_size_of_children(&...)`.
                    // Our procedural macro places the `HeapSize` trait in scope
                    // within the generated code so this is guaranteed to
                    // resolve to the right trait method, even if one of these
                    // fields has an inherent method with a conflicting name as
                    // demonstrated in main.rs.
                    //
                    // We take some care to use the span of each `syn::Field` as
                    // the span of the corresponding `heap_size_of_children`
                    // call. This way if one of the field types does not
                    // implement `HeapSize` then the compiler's error message
                    // underlines which field it is. An example is shown in the
                    // readme of the parent directory.
                    let children = fields.named.iter().map(|f| {
                        let name = f.ident;
                        // FIXME: this needs to be f.span().resolved_at(def_site).
                        // https://github.com/rust-lang/rust/pull/47149
                        let method = Ident::new("heap_size_of_children", f.span());
                        quote! {
                            self.#name.#method()
                        }
                    });
                    quote! {
                        0 #(+ #children)*
                    }
                }
                Fields::Unnamed(ref fields) => {
                    // We can also expand in the straightforward way without
                    // worrying about spans. This works but the error message
                    // may not be as good in the case that one of the field
                    // types does not implement HeapSize.
                    //
                    // This expands to an expression like
                    //
                    //     0 + self.0.heap_size() + self.1.heap_size() + self.2.heap_size()
                    let indices = 0..fields.unnamed.len();
                    quote! {
                        0 #(+ self.#indices.heap_size_of_children())*
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
