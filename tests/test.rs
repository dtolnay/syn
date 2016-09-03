extern crate item;
use item::*;

fn simple_ty(ident: &str) -> Ty {
    Ty::Path(None, Path {
        global: false,
        segments: vec![PathSegment::ident(ident.to_string())],
    })
}
#[test]
fn test_struct() {
    let raw = "
        #[derive(Debug, Clone)]
        pub struct Item {
            pub ident: Ident,
            pub attrs: Vec<Attribute>,
        }
    ";

    let expected = Item {
        ident: "Item".to_string(),
        vis: Visibility::Public,
        attrs: vec![
            Attribute {
                value: MetaItem::List("derive".to_string(), vec![
                    MetaItem::Word("Debug".to_string()),
                    MetaItem::Word("Clone".to_string()),
                ]),
                is_sugared_doc: false,
            },
        ],
        generics: Generics::default(),
        body: Body::Struct(Style::Struct, vec![
            Field {
                ident: Some("ident".to_string()),
                vis: Visibility::Public,
                attrs: Vec::new(),
                ty: simple_ty("Ident"),
            },
            Field {
                ident: Some("attrs".to_string()),
                vis: Visibility::Public,
                attrs: Vec::new(),
                ty: Ty::Path(None, Path {
                    global: false,
                    segments: vec![
                        PathSegment {
                            ident: "Vec".to_string(),
                            parameters: PathParameters::AngleBracketed(
                                AngleBracketedParameterData {
                                    lifetimes: Vec::new(),
                                    types: vec![simple_ty("Attribute")],
                                    bindings: Vec::new(),
                                },
                            ),
                        }
                    ],
                }),
            },
        ]),
    };

    assert_eq!(expected, parse(raw).unwrap());
}
