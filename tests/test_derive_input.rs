#![cfg(feature = "extra-traits")]

extern crate syn;
extern crate proc_macro2;

use syn::*;
use proc_macro2::{TokenNode, Spacing, Delimiter, Literal, Term};
use proc_macro2::Delimiter::{Parenthesis, Brace};

fn op(c: char) -> TokenTree {
    TokenTree(proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenNode::Op(c, Spacing::Alone),
    })
}

fn lit<T: Into<Literal>>(t: T) -> TokenTree {
    TokenTree(proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenNode::Literal(t.into()),
    })
}

fn word(sym: &str) -> TokenTree {
    TokenTree(proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenNode::Term(Term::intern(sym)),
    })
}

fn delimited(delim: Delimiter, tokens: Vec<TokenTree>) -> TokenTree {
    TokenTree(proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenNode::Group(delim, tokens.into_iter().map(|t| t.0).collect()),
    })
}

#[test]
fn test_unit() {
    let raw = "struct Unit;";

    let expected = DeriveInput {
        ident: "Unit".into(),
        vis: Visibility::Inherited(VisInherited {}),
        attrs: Vec::new(),
        generics: Generics::default(),
        body: Body::Struct(BodyStruct {
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
            data: VariantData::Unit,
        }),
    };

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}

#[test]
fn test_struct() {
    let raw = "
        #[derive(Debug, Clone)]
        pub struct Item {
            pub ident: Ident,
            pub attrs: Vec<Attribute>
        }
    ";

    let expected = DeriveInput {
        ident: "Item".into(),
        vis: Visibility::Public(VisPublic {
            pub_token: Default::default(),
        }),
        attrs: vec![Attribute {
            bracket_token: Default::default(),
            pound_token: Default::default(),
            style: AttrStyle::Outer,
            path: "derive".into(),
            tts: vec![
                delimited(Parenthesis, vec![
                    word("Debug"),
                    op(','),
                    word("Clone"),
                ]),
            ],
            is_sugared_doc: false,
        }],
        generics: Generics::default(),
        body: Body::Struct(BodyStruct {
            semi_token: None,
            struct_token: Default::default(),
            data: VariantData::Struct(vec![
                Field {
                    ident: Some("ident".into()),
                    colon_token: Some(Default::default()),
                    vis: Visibility::Public(VisPublic {
                        pub_token: Default::default(),
                    }),
                    attrs: Vec::new(),
                    ty: TyPath {
                        qself: None,
                        path: "Ident".into(),
                    }.into(),
                },
                Field {
                    ident: Some("attrs".into()),
                    colon_token: Some(Default::default()),
                    vis: Visibility::Public(VisPublic {
                        pub_token: Default::default(),
                    }),
                    attrs: Vec::new(),
                    ty: TyPath {
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: vec![
                                PathSegment {
                                    ident: "Vec".into(),
                                    parameters: PathParameters::AngleBracketed(
                                        AngleBracketedParameterData {
                                            turbofish: None,
                                            lt_token: Default::default(),
                                            lifetimes: Default::default(),
                                            types: vec![Ty::from(TyPath {
                                                qself: None,
                                                path: "Attribute".into(),
                                            })].into(),
                                            bindings: Default::default(),
                                            gt_token: Default::default(),
                                        },
                                    ),
                                }
                            ].into(),
                        },
                    }.into(),
                },
            ].into(), Default::default()),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    let expected_meta_item: MetaItem = MetaItemList {
        ident: "derive".into(),
        paren_token: Default::default(),
        nested: vec![
            NestedMetaItem::MetaItem(MetaItem::Term("Debug".into())),
            NestedMetaItem::MetaItem(MetaItem::Term("Clone".into())),
        ].into(),
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
            ProcMacroHack = (0, "data").0
        }
    "#;

    let expected = DeriveInput {
        ident: "Result".into(),
        vis: Visibility::Public(VisPublic {
            pub_token: Default::default(),
        }),
        attrs: vec![
            Attribute {
                bracket_token: Default::default(),
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                path: "doc".into(),
                tts: vec![
                    op('='),
                    lit(Literal::doccomment("/// See the std::result module documentation for details.")),
                ],
                is_sugared_doc: true,
            },
            Attribute {
                bracket_token: Default::default(),
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                path: "must_use".into(),
                tts: vec![],
                is_sugared_doc: false,
            },
        ],
        generics: Generics {
            lifetimes: Default::default(),
            lt_token: Some(Default::default()),
            gt_token: Some(Default::default()),
            ty_params: vec![
                TyParam {
                    attrs: Vec::new(),
                    ident: "T".into(),
                    bounds: Default::default(),
                    default: None,
                    colon_token: None,
                    eq_token: None,
                },
                TyParam {
                    attrs: Vec::new(),
                    ident: "E".into(),
                    bounds: Default::default(),
                    colon_token: None,
                    eq_token: None,
                    default: None,
                },
            ].into(),
            where_clause: WhereClause::default(),
        },
        body: Body::Enum(BodyEnum {
            variants: vec![
                Variant {
                    ident: "Ok".into(),
                    attrs: Vec::new(),
                    eq_token: None,
                    data: VariantData::Tuple(vec![
                        Field {
                            colon_token: None,
                            ident: None,
                            vis: Visibility::Inherited(VisInherited {}),
                            attrs: Vec::new(),
                            ty: TyPath { qself: None, path: "T".into() }.into(),
                        },
                    ].into(), Default::default()),
                    discriminant: None,
                },
                Variant {
                    ident: "Err".into(),
                    attrs: Vec::new(),
                    eq_token: None,
                    data: VariantData::Tuple(vec![
                        Field {
                            ident: None,
                            colon_token: None,
                            vis: Visibility::Inherited(VisInherited {}),
                            attrs: Vec::new(),
                            ty: TyPath { qself: None, path: "E".into() }.into(),
                        },
                    ].into(), Default::default()),
                    discriminant: None,
                },
                Variant {
                    ident: "Surprise".into(),
                    attrs: Vec::new(),
                    data: VariantData::Unit,
                    eq_token: Some(Default::default()),
                    discriminant: Some(Expr{
                        node: Lit {
                            value: LitKind::Other(Literal::isize(0)),
                            span: Default::default(),
                        }.into(),
                        attrs: Vec::new(),
                    }),
                },
                Variant {
                    ident: "ProcMacroHack".into(),
                    attrs: Vec::new(),
                    data: VariantData::Unit,
                    eq_token: Some(Default::default()),
                    discriminant: Some(Expr {
                        node: ExprTupField {
                            expr: Box::new(Expr {
                                node: ExprTup {
                                    lone_comma: None,
                                    paren_token: Default::default(),
                                    args: vec![
                                        Expr {
                                            node: ExprKind::Lit(Lit {
                                                value: LitKind::Other(Literal::integer(0)),
                                                span: Default::default(),
                                            }),
                                            attrs: Vec::new(),
                                        },
                                        Expr {
                                            node: ExprKind::Lit(Lit {
                                                value: LitKind::Other(Literal::string("data")),
                                                span: Default::default(),
                                            }),
                                            attrs: Vec::new(),
                                        },
                                    ].into(),
                                }.into(),
                                attrs: Vec::new(),
                            }),
                            dot_token: Default::default(),
                            field: Lit {
                                value: LitKind::Other(Literal::integer(0)),
                                span: Default::default(),
                            },
                        }.into(),
                        attrs: Vec::new(),
                    }),
                },
            ].into(),
            brace_token: Default::default(),
            enum_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    let expected_meta_items = vec![
        MetaNameValue {
            ident: "doc".into(),
            eq_token: Default::default(),
            lit: Lit {
                value: LitKind::Other(Literal::doccomment("/// See the std::result module documentation for details.")),
                span: Default::default(),
            },
        }.into(),
        MetaItem::Term("must_use".into()),
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

    let expected = DeriveInput {
        ident: "Dummy".into(),
        vis: Visibility::Inherited(VisInherited {}),
        attrs: vec![Attribute {
            bracket_token: Default::default(),
            pound_token: Default::default(),
            style: AttrStyle::Outer,
            path: Path {
                leading_colon: Some(Default::default()),
                segments: vec![
                    PathSegment::from("attr_args"),
                    PathSegment::from("identity"),
                ].into(),
            },
            tts: vec![
                word("fn"),
                word("main"),
                delimited(Parenthesis, vec![]),
                delimited(Brace, vec![
                    word("assert_eq"),
                    op('!'),
                    delimited(Parenthesis, vec![
                        word("foo"),
                        delimited(Parenthesis, vec![]),
                        op(','),
                        lit(Literal::string("Hello, world!")),
                    ]),
                    op(';'),
                ]),
            ],
            is_sugared_doc: false,
        }],
        generics: Generics::default(),
        body: Body::Struct(BodyStruct {
            data: VariantData::Unit,
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    assert!(actual.attrs[0].meta_item().is_none());
}

#[test]
fn test_attr_with_non_mod_style_path() {
    let raw =r#"
        #[inert <T>]
        struct S;
    "#;

    let expected = DeriveInput {
        ident: "S".into(),
        vis: Visibility::Inherited(VisInherited {}),
        attrs: vec![Attribute {
            bracket_token: Default::default(),
            pound_token: Default::default(),
            style: AttrStyle::Outer,
            path: Path {
                leading_colon: None,
                segments: vec![
                    PathSegment::from("inert"),
                ].into(),
            },
            tts: vec![
                op('<'),
                word("T"),
                op('>'),
            ],
            is_sugared_doc: false,
        }],
        generics: Generics::default(),
        body: Body::Struct(BodyStruct {
            data: VariantData::Unit,
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    assert!(actual.attrs[0].meta_item().is_none());
}

#[test]
fn test_attr_with_mod_style_path_with_self() {
    let raw =r#"
        #[foo::self]
        struct S;
    "#;

    let expected = DeriveInput {
        ident: "S".into(),
        vis: Visibility::Inherited(VisInherited {}),
        attrs: vec![Attribute {
            bracket_token: Default::default(),
            pound_token: Default::default(),
            style: AttrStyle::Outer,
            path: Path {
                leading_colon: None,
                segments: vec![
                    PathSegment::from("foo"),
                    PathSegment::from("self"),
                ].into(),
            },
            tts: vec![],
            is_sugared_doc: false,
        }],
        generics: Generics::default(),
        body: Body::Struct(BodyStruct {
            data: VariantData::Unit,
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    assert!(actual.attrs[0].meta_item().is_none());
}

#[test]
fn test_pub_restricted() {
    // Taken from tests/rust/src/test/ui/resolve/auxiliary/privacy-struct-ctor.rs
    let raw =r#"
        pub(in m) struct Z(pub(in m::n) u8);
    "#;

    let expected = DeriveInput {
        ident: "Z".into(),
        vis: Visibility::Restricted(VisRestricted {
            path: Box::new("m".into()),
            in_token: Some(Default::default()),
            paren_token: Default::default(),
            pub_token: Default::default(),
        }),
        attrs: vec![],
        generics: Generics::default(),
        body: Body::Struct(BodyStruct {
            data: VariantData::Tuple(vec![Field {
                ident: None,
                vis: Visibility::Restricted(VisRestricted {
                    path: Box::new(Path {
                        leading_colon: None,
                        segments: vec![
                            PathSegment::from("m"),
                            PathSegment::from("n"),
                        ].into(),
                    }),
                    in_token: Some(Default::default()),
                    paren_token: Default::default(),
                    pub_token: Default::default(),
                }),
                colon_token: None,
                attrs: vec![],
                ty: TyPath {
                    qself: None,
                    path: "u8".into(),
                }.into(),
            }].into(), Default::default()),
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_pub_restricted_crate() {
    let raw =r#"
        pub(crate) struct S;
    "#;

    let expected = DeriveInput {
        ident: "S".into(),
        vis: Visibility::Crate(VisCrate {
            pub_token: Default::default(),
            crate_token: Default::default(),
            paren_token: Default::default(),
        }),
        attrs: vec![],
        generics: Generics::default(),
        body: Body::Struct(BodyStruct {
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
            data: VariantData::Unit,
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_pub_restricted_super() {
    let raw =r#"
        pub(super) struct S;
    "#;

    let expected = DeriveInput {
        ident: "S".into(),
        vis: Visibility::Restricted(VisRestricted {
            path: Box::new("super".into()),
            in_token: None,
            paren_token: Default::default(),
            pub_token: Default::default(),
        }),
        attrs: vec![],
        generics: Generics::default(),
        body: Body::Struct(BodyStruct {
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
            data: VariantData::Unit,
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_pub_restricted_in_super() {
    let raw =r#"
        pub(in super) struct S;
    "#;

    let expected = DeriveInput {
        ident: "S".into(),
        vis: Visibility::Restricted(VisRestricted {
            path: Box::new("super".into()),
            in_token: Some(Default::default()),
            paren_token: Default::default(),
            pub_token: Default::default(),
        }),
        attrs: vec![],
        generics: Generics::default(),
        body: Body::Struct(BodyStruct {
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
            data: VariantData::Unit,
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}
