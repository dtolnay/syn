extern crate syn;
use syn::*;

#[test]
fn test_unit() {
    let raw = "struct Unit;";

    let expected = MacroInput {
        ident: "Unit".into(),
        vis: Visibility::Inherited,
        attrs: Vec::new(),
        generics: Generics::default(),
        body: Body::Struct(VariantData::Unit),
    };

    assert_eq!(expected, parse_macro_input(raw).unwrap());
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

    let expected = MacroInput {
        ident: "Item".into(),
        vis: Visibility::Public,
        attrs: vec![
            Attribute {
                style: AttrStyle::Outer,
                value: MetaItem::List("derive".into(), vec![
                    MetaItem::Word("Debug".into()),
                    MetaItem::Word("Clone".into()),
                ]),
                is_sugared_doc: false,
            },
        ],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Struct(vec![
            Field {
                ident: Some("ident".into()),
                vis: Visibility::Public,
                attrs: Vec::new(),
                ty: Ty::Path(None, "Ident".into()),
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
                                    types: vec![Ty::Path(None, "Attribute".into())],
                                    bindings: Vec::new(),
                                },
                            ),
                        }
                    ],
                }),
            },
        ])),
    };

    assert_eq!(expected, parse_macro_input(raw).unwrap());
}

#[test]
fn test_enum() {
    let raw = "
        /// See the std::result module documentation for details.
        #[must_use]
        pub enum Result<T, E> {
            Ok(T),
            Err(E),
            Surprise = 0isize,
        }
    ";

    let expected = MacroInput {
        ident: "Result".into(),
        vis: Visibility::Public,
        attrs: vec![
            Attribute {
                style: AttrStyle::Outer,
                value: MetaItem::NameValue(
                    "doc".into(),
                    Lit::Str(
                        "/// See the std::result module documentation for details.".into(),
                        StrStyle::Cooked,
                    ),
                ),
                is_sugared_doc: true,
            },
            Attribute {
                style: AttrStyle::Outer,
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
            where_clause: WhereClause { predicates: Vec::new() },
        },
        body: Body::Enum(vec![
            Variant {
                ident: "Ok".into(),
                attrs: Vec::new(),
                data: VariantData::Tuple(vec![
                    Field {
                        ident: None,
                        vis: Visibility::Inherited,
                        attrs: Vec::new(),
                        ty: Ty::Path(None, "T".into()),
                    },
                ]),
                discriminant: None,
            },
            Variant {
                ident: "Err".into(),
                attrs: Vec::new(),
                data: VariantData::Tuple(vec![
                    Field {
                        ident: None,
                        vis: Visibility::Inherited,
                        attrs: Vec::new(),
                        ty: Ty::Path(None, "E".into()),
                    },
                ]),
                discriminant: None,
            },
            Variant {
                ident: "Surprise".into(),
                attrs: Vec::new(),
                data: VariantData::Unit,
                discriminant: Some(Discriminant {
                    value: 0,
                    ty: IntTy::Isize,
                }),
            },
        ]),
    };

    assert_eq!(expected, parse_macro_input(raw).unwrap());
}
