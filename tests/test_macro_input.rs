//! Test the now-deprecated `parse_macro_input` function.
//!
//! Deprecation warnings are suppressed to keep the output clean.
#![allow(deprecated)]

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
                    TokenTree::Token(Token::Ident("Debug".into())),
                    TokenTree::Token(Token::Comma),
                    TokenTree::Token(Token::Ident("Clone".into())),
                ]})
            ],
            is_sugared_doc: false,
        }],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Struct(vec![
            Field {
                ident: Some("ident".into()),
                vis: Visibility::Public,
                attrs: Vec::new(),
                ty: TyPath {
                    qself: None,
                    path: "Ident".into(),
                }.into(),
            },
            Field {
                ident: Some("attrs".into()),
                vis: Visibility::Public,
                attrs: Vec::new(),
                ty: TyPath {
                    qself: None,
                    path: Path {
                        global: false,
                        segments: vec![
                            PathSegment {
                                ident: "Vec".into(),
                                parameters: PathParameters::AngleBracketed(
                                    AngleBracketedParameterData {
                                        lifetimes: Vec::new(),
                                        types: vec![TyPath {
                                            qself: None,
                                            path: "Attribute".into(),
                                        }.into()],
                                        bindings: Vec::new(),
                                    },
                                ),
                            }
                        ],
                    },
                }.into(),
            },
        ]))
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);

    let expected_meta_item: MetaItem = MetaItemList {
        ident: "derive".into(),
        nested: vec![
            NestedMetaItem::MetaItem(MetaItem::Word("Debug".into())),
            NestedMetaItem::MetaItem(MetaItem::Word("Clone".into())),
        ],
    }.into();

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
                        ty: TyPath { qself: None, path: "T".into() }.into(),
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
                        ty: TyPath { qself: None, path: "E".into() }.into(),
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
                    node: ExprTupField {
                        expr: Box::new(Expr {
                            node: ExprTup {
                                args: vec![
                                    Expr {
                                        node: ExprKind::Lit(Lit::Int(0, IntTy::Unsuffixed)),
                                        attrs: Vec::new(),
                                    },
                                    Expr {
                                        node: ExprKind::Lit(Lit::Str("data".into(), StrStyle::Cooked)),
                                        attrs: Vec::new(),
                                    },
                                ],
                            }.into(),
                            attrs: Vec::new(),
                        }),
                        field: 0
                    }.into(),
                    attrs: Vec::new(),
                })),
            },
        ]),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);

    let expected_meta_items = vec![
        MetaNameValue {
            ident: "doc".into(),
            lit: Lit::Str(
                "/// See the std::result module documentation for details.".into(),
                StrStyle::Cooked,
            ),
        }.into(),
        MetaItem::Word("must_use".into()),
    ];

    let actual_meta_items: Vec<_> = actual.attrs.into_iter().map(|attr| attr.meta_item().unwrap()).collect();

    assert_eq!(expected_meta_items, actual_meta_items);
}

#[test]
fn test_attr_with_path() {
    let raw =r#"
        #[::attr_args::identity
            fn main() { assert_eq!(foo(), "Hello, world!"); }]
        struct Dummy;
    "#;

    let expected = MacroInput {
        ident: "Dummy".into(),
        vis: Visibility::Inherited,
        attrs: vec![Attribute {
            style: AttrStyle::Outer,
            path: Path { global: true, segments: vec!["attr_args".into(), "identity".into()] },
            tts: vec![
                TokenTree::Token(Token::Ident("fn".into())),
                TokenTree::Token(Token::Ident("main".into())),
                TokenTree::Delimited(Delimited { delim: DelimToken::Paren, tts: vec![] }),
                TokenTree::Delimited(Delimited { delim: DelimToken::Brace, tts: vec![
                    TokenTree::Token(Token::Ident("assert_eq".into())),
                    TokenTree::Token(Token::Not),
                    TokenTree::Delimited(Delimited { delim: DelimToken::Paren, tts: vec![
                        TokenTree::Token(Token::Ident("foo".into())),
                        TokenTree::Delimited(Delimited { delim: DelimToken::Paren, tts: vec![] }),
                        TokenTree::Token(Token::Comma),
                        TokenTree::Token(Token::Literal(Lit::Str("Hello, world!".into(), StrStyle::Cooked))),
                    ]}),
                    TokenTree::Token(Token::Semi),
                ]})
            ],
            is_sugared_doc: false,
        }],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Unit),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);

    assert!(actual.attrs[0].meta_item().is_none());
}

#[test]
fn test_attr_with_non_mod_style_path() {
    let raw =r#"
        #[inert <T>]
        struct S;
    "#;

    let expected = MacroInput {
        ident: "S".into(),
        vis: Visibility::Inherited,
        attrs: vec![Attribute {
            style: AttrStyle::Outer,
            path: Path { global: false, segments: vec!["inert".into()] },
            tts: vec![
                TokenTree::Token(Token::Lt),
                TokenTree::Token(Token::Ident("T".into())),
                TokenTree::Token(Token::Gt),
            ],
            is_sugared_doc: false,
        }],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Unit),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);

    assert!(actual.attrs[0].meta_item().is_none());
}

#[test]
fn test_attr_with_mod_style_path_with_self() {
    let raw =r#"
        #[foo::self]
        struct S;
    "#;

    let expected = MacroInput {
        ident: "S".into(),
        vis: Visibility::Inherited,
        attrs: vec![Attribute {
            style: AttrStyle::Outer,
            path: Path { global: false, segments: vec!["foo".into(), "self".into()] },
            tts: vec![],
            is_sugared_doc: false,
        }],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Unit),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);

    assert!(actual.attrs[0].meta_item().is_none());
}

#[test]
fn test_pub_restricted() {
    // Taken from tests/rust/src/test/ui/resolve/auxiliary/privacy-struct-ctor.rs
    let raw =r#"
        pub(in m) struct Z(pub(in m::n) u8);
    "#;

    let expected = MacroInput {
        ident: "Z".into(),
        vis: Visibility::Restricted(Box::new("m".into())),
        attrs: vec![],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Tuple(vec![Field {
            ident: None,
            vis: Visibility::Restricted(Box::new(Path { global: false, segments: vec!["m".into(), "n".into()] })),
            attrs: vec![],
            ty: TyPath { qself: None, path: "u8".into() }.into(),
        }])),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_pub_restricted_crate() {
    let raw =r#"
        pub(crate) struct S;
    "#;

    let expected = MacroInput {
        ident: "S".into(),
        vis: Visibility::Crate,
        attrs: vec![],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Unit),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_pub_restricted_super() {
    let raw =r#"
        pub(super) struct S;
    "#;

    let expected = MacroInput {
        ident: "S".into(),
        vis: Visibility::Restricted(Box::new("super".into())),
        attrs: vec![],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Unit),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_pub_restricted_in_super() {
    let raw =r#"
        pub(in super) struct S;
    "#;

    let expected = MacroInput {
        ident: "S".into(),
        vis: Visibility::Restricted(Box::new("super".into())),
        attrs: vec![],
        generics: Generics::default(),
        body: Body::Struct(VariantData::Unit),
    };

    let actual = parse_macro_input(raw).unwrap();

    assert_eq!(expected, actual);
}
