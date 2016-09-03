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

    assert_eq!(expected, parse(raw));
}

#[test]
fn test_enum() {
    let raw = "
        pub enum Result<T, E> {
            Ok(T),
            #[unlikely]
            Err(E),
        }
    ";

    let expected = Item {
        ident: "Result".to_string(),
        vis: Visibility::Public,
        attrs: Vec::new(),
        generics: Generics {
            lifetimes: Vec::new(),
            ty_params: vec![
                TyParam {
                    ident: "T".to_string(),
                    bounds: Vec::new(),
                    default: None,
                },
                TyParam {
                    ident: "E".to_string(),
                    bounds: Vec::new(),
                    default: None,
                },
            ],
            where_clause: Vec::new(),
        },
        body: Body::Enum(vec![
            Variant {
                ident: "Ok".to_string(),
                attrs: Vec::new(),
                style: Style::Tuple,
                fields: vec![
                    Field {
                        ident: None,
                        vis: Visibility::Inherited,
                        attrs: Vec::new(),
                        ty: simple_ty("T"),
                    },
                ],
            },
            Variant {
                ident: "Err".to_string(),
                attrs: vec![
                    Attribute {
                        value: MetaItem::Word("unlikely".to_string()),
                        is_sugared_doc: false,
                    },
                ],
                style: Style::Tuple,
                fields: vec![
                    Field {
                        ident: None,
                        vis: Visibility::Inherited,
                        attrs: Vec::new(),
                        ty: simple_ty("E"),
                    },
                ],
            },
        ]),
    };

    assert_eq!(expected, parse(raw));
}
