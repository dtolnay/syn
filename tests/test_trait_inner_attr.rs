#[macro_use]
mod macros;

use quote::quote;
use syn::ItemTrait;

#[test]
fn test_trait_inner_attr() {
    let input = quote! {
        trait Foo {
            #![allow(bar)]
        }
    };

    snapshot!(input as ItemTrait, @r###"
    ItemTrait {
        attrs: [
            Attribute {
                style: Inner,
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "allow",
                            arguments: None,
                        },
                    ],
                },
                tokens: `( bar )`,
            },
        ],
        vis: Inherited,
        ident: "Foo",
        generics: Generics,
    }
    "###);
}
