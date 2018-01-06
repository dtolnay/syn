#![cfg(feature = "extra-traits")]

extern crate proc_macro2;
extern crate syn;

use syn::*;
use proc_macro2::{Delimiter, Literal, Spacing, Term, TokenNode, TokenStream, TokenTree};
use proc_macro2::Delimiter::{Brace, Parenthesis};

use std::iter::FromIterator;

#[macro_use]
mod macros;

fn op(c: char) -> TokenTree {
    proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenNode::Op(c, Spacing::Alone),
    }
}

fn lit<T: Into<Literal>>(t: T) -> TokenTree {
    proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenNode::Literal(t.into()),
    }
}

fn word(sym: &str) -> TokenTree {
    proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenNode::Term(Term::intern(sym)),
    }
}

fn delimited(delim: Delimiter, tokens: Vec<TokenTree>) -> TokenTree {
    proc_macro2::TokenTree {
        span: Default::default(),
        kind: TokenNode::Group(delim, tokens.into_iter().collect()),
    }
}

#[test]
fn test_unit() {
    let raw = "struct Unit;";

    let expected = DeriveInput {
        ident: "Unit".into(),
        vis: Visibility::Inherited,
        attrs: Vec::new(),
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
            fields: Fields::Unit,
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
        attrs: vec![
            Attribute {
                bracket_token: Default::default(),
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                path: "derive".into(),
                tts: TokenStream::from_iter(vec![
                    delimited(Parenthesis, vec![word("Debug"), op(','), word("Clone")]),
                ]),
                is_sugared_doc: false,
            },
        ],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            semi_token: None,
            struct_token: Default::default(),
            fields: Fields::Named(FieldsNamed {
                brace_token: Default::default(),
                named: punctuated![
                    Field {
                        ident: Some("ident".into()),
                        colon_token: Some(Default::default()),
                        vis: Visibility::Public(VisPublic {
                            pub_token: Default::default(),
                        }),
                        attrs: Vec::new(),
                        ty: TypePath {
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
                        ty: TypePath {
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: punctuated![
                                    PathSegment {
                                        ident: "Vec".into(),
                                        arguments: PathArguments::AngleBracketed(
                                            AngleBracketedGenericArguments {
                                                colon2_token: None,
                                                lt_token: Default::default(),
                                                args: punctuated![
                                                    GenericArgument::Type(Type::from(TypePath {
                                                        qself: None,
                                                        path: "Attribute".into(),
                                                    })),
                                                ],
                                                gt_token: Default::default(),
                                            },
                                        ),
                                    }
                                ],
                            },
                        }.into(),
                    },
                ],
            }),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    let expected_meta_item: MetaItem = MetaItemList {
        ident: "derive".into(),
        paren_token: Default::default(),
        nested: punctuated![
            NestedMetaItem::MetaItem(MetaItem::Term("Debug".into())),
            NestedMetaItem::MetaItem(MetaItem::Term("Clone".into())),
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
                tts: TokenStream::from_iter(vec![
                    op('='),
                    lit(Literal::string(
                        "/// See the std::result module documentation for details.",
                    )),
                ]),
                is_sugared_doc: true,
            },
            Attribute {
                bracket_token: Default::default(),
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                path: "must_use".into(),
                tts: TokenStream::empty(),
                is_sugared_doc: false,
            },
        ],
        generics: Generics {
            lt_token: Some(Default::default()),
            params: punctuated![
                GenericParam::Type(TypeParam {
                    attrs: Vec::new(),
                    ident: "T".into(),
                    bounds: Default::default(),
                    default: None,
                    colon_token: None,
                    eq_token: None,
                }),
                GenericParam::Type(TypeParam {
                    attrs: Vec::new(),
                    ident: "E".into(),
                    bounds: Default::default(),
                    colon_token: None,
                    eq_token: None,
                    default: None,
                }),
            ],
            gt_token: Some(Default::default()),
            where_clause: None,
        },
        data: Data::Enum(DataEnum {
            variants: punctuated![
                Variant {
                    ident: "Ok".into(),
                    attrs: Vec::new(),
                    fields: Fields::Unnamed(FieldsUnnamed {
                        paren_token: Default::default(),
                        unnamed: punctuated![
                            Field {
                                colon_token: None,
                                ident: None,
                                vis: Visibility::Inherited,
                                attrs: Vec::new(),
                                ty: TypePath {
                                    qself: None,
                                    path: "T".into(),
                                }.into(),
                            },
                        ],
                    }),
                    discriminant: None,
                },
                Variant {
                    ident: "Err".into(),
                    attrs: Vec::new(),
                    fields: Fields::Unnamed(FieldsUnnamed {
                        paren_token: Default::default(),
                        unnamed: punctuated![
                            Field {
                                ident: None,
                                colon_token: None,
                                vis: Visibility::Inherited,
                                attrs: Vec::new(),
                                ty: TypePath {
                                    qself: None,
                                    path: "E".into(),
                                }.into(),
                            },
                        ],
                    }),
                    discriminant: None,
                },
                Variant {
                    ident: "Surprise".into(),
                    attrs: Vec::new(),
                    fields: Fields::Unit,
                    discriminant: Some((
                        Default::default(),
                        Expr::Lit(ExprLit {
                            attrs: Vec::new(),
                            lit: Lit::Int(LitInt::new(0, IntSuffix::Isize, Default::default())),
                        }),
                    )),
                },
                Variant {
                    ident: "ProcMacroHack".into(),
                    attrs: Vec::new(),
                    fields: Fields::Unit,
                    discriminant: Some((
                        Default::default(),
                        Expr::Field(ExprField {
                            attrs: Vec::new(),
                            base: Box::new(Expr::Tuple(ExprTuple {
                                attrs: Vec::new(),
                                paren_token: Default::default(),
                                elems: punctuated![
                                    Expr::Lit(ExprLit {
                                        attrs: Vec::new(),
                                        lit: Lit::Int(LitInt::new(
                                            0,
                                            IntSuffix::None,
                                            Default::default()
                                        )),
                                    }),
                                    Expr::Lit(ExprLit {
                                        attrs: Vec::new(),
                                        lit: Lit::Str(LitStr::new("data", Default::default())),
                                    }),
                                ],
                            })),
                            dot_token: Default::default(),
                            member: Member::Unnamed(Index {
                                index: 0,
                                span: Default::default(),
                            }),
                        }),
                    )),
                },
            ],
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
            lit: Lit::Str(LitStr::new(
                "/// See the std::result module documentation for details.",
                Default::default(),
            )),
        }.into(),
        MetaItem::Term("must_use".into()),
    ];

    let actual_meta_items: Vec<_> = actual
        .attrs
        .into_iter()
        .map(|attr| attr.meta_item().unwrap())
        .collect();

    assert_eq!(expected_meta_items, actual_meta_items);
}

#[test]
fn test_attr_with_path() {
    let raw = r#"
        #[::attr_args::identity
            fn main() { assert_eq!(foo(), "Hello, world!"); }]
        struct Dummy;
    "#;

    let expected = DeriveInput {
        ident: "Dummy".into(),
        vis: Visibility::Inherited,
        attrs: vec![
            Attribute {
                bracket_token: Default::default(),
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                path: Path {
                    leading_colon: Some(Default::default()),
                    segments: punctuated![
                        PathSegment::from("attr_args"),
                        PathSegment::from("identity"),
                    ],
                },
                tts: TokenStream::from_iter(vec![
                    word("fn"),
                    word("main"),
                    delimited(Parenthesis, vec![]),
                    delimited(
                        Brace,
                        vec![
                            word("assert_eq"),
                            op('!'),
                            delimited(
                                Parenthesis,
                                vec![
                                    word("foo"),
                                    delimited(Parenthesis, vec![]),
                                    op(','),
                                    lit(Literal::string("Hello, world!")),
                                ],
                            ),
                            op(';'),
                        ],
                    ),
                ]),
                is_sugared_doc: false,
            },
        ],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            fields: Fields::Unit,
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
    let raw = r#"
        #[inert <T>]
        struct S;
    "#;

    let expected = DeriveInput {
        ident: "S".into(),
        vis: Visibility::Inherited,
        attrs: vec![
            Attribute {
                bracket_token: Default::default(),
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                path: Path {
                    leading_colon: None,
                    segments: punctuated![PathSegment::from("inert")],
                },
                tts: TokenStream::from_iter(vec![op('<'), word("T"), op('>')]),
                is_sugared_doc: false,
            },
        ],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            fields: Fields::Unit,
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
    let raw = r#"
        #[foo::self]
        struct S;
    "#;

    let expected = DeriveInput {
        ident: "S".into(),
        vis: Visibility::Inherited,
        attrs: vec![
            Attribute {
                bracket_token: Default::default(),
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                path: Path {
                    leading_colon: None,
                    segments: punctuated![PathSegment::from("foo"), PathSegment::from("self")],
                },
                tts: TokenStream::empty(),
                is_sugared_doc: false,
            },
        ],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            fields: Fields::Unit,
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
    let raw = r#"
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
        data: Data::Struct(DataStruct {
            fields: Fields::Unnamed(FieldsUnnamed {
                paren_token: Default::default(),
                unnamed: punctuated![
                    Field {
                        ident: None,
                        vis: Visibility::Restricted(VisRestricted {
                            path: Box::new(Path {
                                leading_colon: None,
                                segments: punctuated![
                                    PathSegment::from("m"),
                                    PathSegment::from("n")
                                ],
                            }),
                            in_token: Some(Default::default()),
                            paren_token: Default::default(),
                            pub_token: Default::default(),
                        }),
                        colon_token: None,
                        attrs: vec![],
                        ty: TypePath {
                            qself: None,
                            path: "u8".into(),
                        }.into(),
                    },
                ],
            }),
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_pub_restricted_crate() {
    let raw = r#"
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
        data: Data::Struct(DataStruct {
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
            fields: Fields::Unit,
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_pub_restricted_super() {
    let raw = r#"
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
        data: Data::Struct(DataStruct {
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
            fields: Fields::Unit,
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_pub_restricted_in_super() {
    let raw = r#"
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
        data: Data::Struct(DataStruct {
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
            fields: Fields::Unit,
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}
