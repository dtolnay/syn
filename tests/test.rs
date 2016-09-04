extern crate item;
use item::*;

fn simple_ty(ident: &str) -> Ty {
    Ty::Path(None, Path {
        global: false,
        segments: vec![PathSegment::ident(ident.into())],
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
        ident: "Item".into(),
        vis: Visibility::Public,
        attrs: vec![
            Attribute {
                value: MetaItem::List("derive".into(), vec![
                    MetaItem::Word("Debug".into()),
                    MetaItem::Word("Clone".into()),
                ]),
                is_sugared_doc: false,
            },
        ],
        generics: Generics::default(),
        body: Body::Struct(Style::Struct, vec![
            Field {
                ident: Some("ident".into()),
                vis: Visibility::Public,
                attrs: Vec::new(),
                ty: simple_ty("Ident"),
            },
            Field {
                ident: Some("attrs".into()),
                vis: Visibility::Public,
                attrs: Vec::new(),
                ty: Ty::Path(None, Path {
                    global: false,
                    segments: vec![
                        PathSegment {
                            ident: "Vec".into(),
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
        /// See the std::result module documentation for details.
        #[must_use]
        pub enum Result<T, E> {
            Ok(T),
            Err(E),
        }
    ";

    let expected = Item {
        ident: "Result".into(),
        vis: Visibility::Public,
        attrs: vec![
            Attribute {
                value: MetaItem::NameValue(
                    "doc".into(),
                    "/// See the std::result module documentation for details.".into(),
                ),
                is_sugared_doc: true,
            },
            Attribute {
                value: MetaItem::Word("must_use".into()),
                is_sugared_doc: false,
            },
        ],
        generics: Generics {
            lifetimes: Vec::new(),
            ty_params: vec![
                TyParam {
                    ident: "T".into(),
                    bounds: Vec::new(),
                    default: None,
                },
                TyParam {
                    ident: "E".into(),
                    bounds: Vec::new(),
                    default: None,
                },
            ],
            where_clause: Vec::new(),
        },
        body: Body::Enum(vec![
            Variant {
                ident: "Ok".into(),
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
                ident: "Err".into(),
                attrs: Vec::new(),
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
