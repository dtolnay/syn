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
        attrs: vec![Attribute {
            style: AttrStyle::Outer,
            path: "derive".into(),
            tts: vec![
                TokenTree::Delimited(Delimited { delim: DelimToken::Paren, tts: vec![
                    TokenTree::Token(Token::Ident(Ident::from("Debug"))),
                    TokenTree::Token(Token::Comma),
                    TokenTree::Token(Token::Ident(Ident::from("Clone"))),
                ]})
            ],
            is_sugared_doc: false,
        }],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Struct(vec![Field {
                                                        ident: Some("ident".into()),
                                                        vis: Visibility::Public,
                                                        attrs: Vec::new(),
                                                        ty: Ty::Path(None, "Ident".into()),
                                                    },
                                                    Field {
                                                        ident: Some("attrs".into()),
                                                        vis: Visibility::Public,
                                                        attrs: Vec::new(),
                                                        ty: Ty::Path(None,
                                                                     Path {
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
                                                    }])),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);

    let expected_meta_item = MetaItem::List(vec![
        NestedMetaItem::MetaItem(Ident::from("Debug"), MetaItem::Word),
        NestedMetaItem::MetaItem(Ident::from("Clone"), MetaItem::Word),
    ]);

    assert_eq!(expected_meta_item, actual.attrs[0].meta_item().unwrap());
}

#[test]
#[cfg(feature = "full")]
fn test_enum() {
    let raw = r#"
        /// See the std::result module documentation for details.
        #[must_use]
        pub enum Result<T, E> {
            Ok(T),
            Err(E),
            Surprise = 0isize,

            // Smuggling data into a proc_macro_derive,
            // in the style of https://github.com/dtolnay/proc-macro-hack
            ProcMacroHack = (0, "data").0,
        }
    "#;

    let expected = MacroInput {
        ident: "Result".into(),
        vis: Visibility::Public,
        attrs: vec![
            Attribute {
                style: AttrStyle::Outer,
                path: "doc".into(),
                tts: vec![
                    TokenTree::Token(Token::Eq),
                    TokenTree::Token(Token::Literal(Lit::Str(
                        "/// See the std::result module documentation for details.".into(),
                        StrStyle::Cooked,
                    ))),
                ],
                is_sugared_doc: true,
            },
            Attribute {
                style: AttrStyle::Outer,
                path: "must_use".into(),
                tts: vec![],
                is_sugared_doc: false,
            },
        ],
        generics: Generics {
            lifetimes: Vec::new(),
            ty_params: vec![TyParam {
                                attrs: Vec::new(),
                                ident: "T".into(),
                                bounds: Vec::new(),
                                default: None,
                            },
                            TyParam {
                                attrs: Vec::new(),
                                ident: "E".into(),
                                bounds: Vec::new(),
                                default: None,
                            }],
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
                discriminant: Some(ConstExpr::Lit(Lit::Int(0, IntTy::Isize))),
            },
            Variant {
                ident: "ProcMacroHack".into(),
                attrs: Vec::new(),
                data: VariantData::Unit,
                discriminant: Some(ConstExpr::Other(Expr {
                    node: ExprKind::TupField(
                        Box::new(Expr {
                            node: ExprKind::Tup(vec![
                                Expr {
                                    node: ExprKind::Lit(Lit::Int(0, IntTy::Unsuffixed)),
                                    attrs: Vec::new(),
                                },
                                Expr {
                                    node: ExprKind::Lit(Lit::Str("data".into(), StrStyle::Cooked)),
                                    attrs: Vec::new(),
                                },
                            ]),
                            attrs: Vec::new(),
                        }),
                        0
                    ),
                    attrs: Vec::new(),
                })),
            },
        ]),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);

    let expected_meta_items = vec![
        MetaItem::NameValue(Lit::Str(
            "/// See the std::result module documentation for details.".into(),
            StrStyle::Cooked,
        )),
        MetaItem::Word,
    ];

    let actual_meta_items: Vec<_> = actual.attrs.into_iter().map(|attr| attr.meta_item().unwrap()).collect();

    assert_eq!(expected_meta_items, actual_meta_items);
}
