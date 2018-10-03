// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg(feature = "extra-traits")]

extern crate proc_macro2;
extern crate syn;

use proc_macro2::Delimiter::{Brace, Parenthesis};
use proc_macro2::*;
use syn::punctuated::Punctuated;
use syn::*;

use std::iter::FromIterator;

#[macro_use]
mod macros;

fn op(c: char) -> TokenTree {
    Punct::new(c, Spacing::Alone).into()
}

fn lit<T: Into<Literal>>(t: T) -> TokenTree {
    t.into().into()
}

fn ident(sym: &str) -> Ident {
    Ident::new(sym, Span::call_site())
}

fn word(sym: &str) -> TokenTree {
    ident(sym).into()
}

fn delimited(delim: Delimiter, tokens: Vec<TokenTree>) -> TokenTree {
    Group::new(delim, tokens.into_iter().collect()).into()
}

#[test]
fn test_unit() {
    let raw = "struct Unit;";

    let expected = DeriveInput {
        ident: ident("Unit"),
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
        ident: ident("Item"),
        vis: Visibility::Public(VisPublic {
            pub_token: Default::default(),
        }),
        attrs: vec![Attribute {
            bracket_token: Default::default(),
            pound_token: Default::default(),
            style: AttrStyle::Outer,
            path: ident("derive").into(),
            tts: TokenStream::from_iter(vec![delimited(
                Parenthesis,
                vec![word("Debug"), op(','), word("Clone")],
            )]),
        }],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            semi_token: None,
            struct_token: Default::default(),
            fields: Fields::Named(FieldsNamed {
                brace_token: Default::default(),
                named: punctuated![
                    Field {
                        ident: Some(ident("ident")),
                        colon_token: Some(Default::default()),
                        vis: Visibility::Public(VisPublic {
                            pub_token: Default::default(),
                        }),
                        attrs: Vec::new(),
                        ty: TypePath {
                            qself: None,
                            path: ident("Ident").into(),
                        }
                        .into(),
                    },
                    Field {
                        ident: Some(ident("attrs")),
                        colon_token: Some(Default::default()),
                        vis: Visibility::Public(VisPublic {
                            pub_token: Default::default(),
                        }),
                        attrs: Vec::new(),
                        ty: TypePath {
                            qself: None,
                            path: Path {
                                leading_colon: None,
                                segments: punctuated![PathSegment {
                                    ident: ident("Vec"),
                                    arguments: PathArguments::AngleBracketed(
                                        AngleBracketedGenericArguments {
                                            colon2_token: None,
                                            lt_token: Default::default(),
                                            args: punctuated![GenericArgument::Type(Type::from(
                                                TypePath {
                                                    qself: None,
                                                    path: ident("Attribute").into(),
                                                }
                                            )),],
                                            gt_token: Default::default(),
                                        },
                                    ),
                                },],
                            },
                        }
                        .into(),
                    },
                ],
            }),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    let expected_meta_item: Meta = MetaList {
        ident: ident("derive"),
        paren_token: Default::default(),
        nested: punctuated![
            NestedMeta::Meta(Meta::Word(ident("Debug"))),
            NestedMeta::Meta(Meta::Word(ident("Clone"))),
        ],
    }
    .into();

    assert_eq!(
        expected_meta_item,
        actual.attrs[0].interpret_meta().unwrap()
    );
}

#[test]
fn test_union() {
    let raw = "
        union MaybeUninit<T> {
            uninit: (),
            value: T
        }
    ";

    let expected = DeriveInput {
        ident: ident("MaybeUninit"),
        vis: Visibility::Inherited,
        attrs: Vec::new(),
        generics: Generics {
            lt_token: Some(Default::default()),
            params: punctuated![GenericParam::Type(TypeParam {
                attrs: Vec::new(),
                ident: ident("T"),
                bounds: Default::default(),
                default: None,
                colon_token: None,
                eq_token: None,
            }),],
            gt_token: Some(Default::default()),
            where_clause: None,
        },
        data: Data::Union(DataUnion {
            union_token: Default::default(),
            fields: FieldsNamed {
                brace_token: Default::default(),
                named: punctuated![
                    Field {
                        ident: Some(ident("uninit")),
                        colon_token: Some(Default::default()),
                        vis: Visibility::Inherited,
                        attrs: Vec::new(),
                        ty: TypeTuple {
                            paren_token: Default::default(),
                            elems: Punctuated::new(),
                        }
                        .into(),
                    },
                    Field {
                        ident: Some(ident("value")),
                        colon_token: Some(Default::default()),
                        vis: Visibility::Inherited,
                        attrs: Vec::new(),
                        ty: TypePath {
                            qself: None,
                            path: ident("T").into(),
                        }
                        .into(),
                    },
                ],
            },
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
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
        ident: ident("Result"),
        vis: Visibility::Public(VisPublic {
            pub_token: Default::default(),
        }),
        attrs: vec![
            Attribute {
                bracket_token: Default::default(),
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                path: ident("doc").into(),
                tts: TokenStream::from_iter(vec![
                    op('='),
                    lit(Literal::string(
                        " See the std::result module documentation for details.",
                    )),
                ]),
            },
            Attribute {
                bracket_token: Default::default(),
                pound_token: Default::default(),
                style: AttrStyle::Outer,
                path: ident("must_use").into(),
                tts: TokenStream::new(),
            },
        ],
        generics: Generics {
            lt_token: Some(Default::default()),
            params: punctuated![
                GenericParam::Type(TypeParam {
                    attrs: Vec::new(),
                    ident: ident("T"),
                    bounds: Default::default(),
                    default: None,
                    colon_token: None,
                    eq_token: None,
                }),
                GenericParam::Type(TypeParam {
                    attrs: Vec::new(),
                    ident: ident("E"),
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
                    ident: ident("Ok"),
                    attrs: Vec::new(),
                    fields: Fields::Unnamed(FieldsUnnamed {
                        paren_token: Default::default(),
                        unnamed: punctuated![Field {
                            colon_token: None,
                            ident: None,
                            vis: Visibility::Inherited,
                            attrs: Vec::new(),
                            ty: TypePath {
                                qself: None,
                                path: ident("T").into(),
                            }
                            .into(),
                        },],
                    }),
                    discriminant: None,
                },
                Variant {
                    ident: ident("Err"),
                    attrs: Vec::new(),
                    fields: Fields::Unnamed(FieldsUnnamed {
                        paren_token: Default::default(),
                        unnamed: punctuated![Field {
                            ident: None,
                            colon_token: None,
                            vis: Visibility::Inherited,
                            attrs: Vec::new(),
                            ty: TypePath {
                                qself: None,
                                path: ident("E").into(),
                            }
                            .into(),
                        },],
                    }),
                    discriminant: None,
                },
                Variant {
                    ident: ident("Surprise"),
                    attrs: Vec::new(),
                    fields: Fields::Unit,
                    discriminant: Some((
                        Default::default(),
                        Expr::Lit(ExprLit {
                            attrs: Vec::new(),
                            lit: Lit::Int(LitInt::new(0, IntSuffix::Isize, Span::call_site())),
                        }),
                    )),
                },
                Variant {
                    ident: ident("ProcMacroHack"),
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
                                            Span::call_site()
                                        )),
                                    }),
                                    Expr::Lit(ExprLit {
                                        attrs: Vec::new(),
                                        lit: Lit::Str(LitStr::new("data", Span::call_site())),
                                    }),
                                ],
                            })),
                            dot_token: Default::default(),
                            member: Member::Unnamed(Index {
                                index: 0,
                                span: Span::call_site(),
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
            ident: ident("doc"),
            eq_token: Default::default(),
            lit: Lit::Str(LitStr::new(
                " See the std::result module documentation for details.",
                Span::call_site(),
            )),
        }
        .into(),
        Meta::Word(ident("must_use")),
    ];

    let actual_meta_items: Vec<_> = actual
        .attrs
        .into_iter()
        .map(|attr| attr.interpret_meta().unwrap())
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
        ident: ident("Dummy"),
        vis: Visibility::Inherited,
        attrs: vec![Attribute {
            bracket_token: Default::default(),
            pound_token: Default::default(),
            style: AttrStyle::Outer,
            path: Path {
                leading_colon: Some(Default::default()),
                segments: punctuated![
                    PathSegment::from(ident("attr_args")),
                    PathSegment::from(ident("identity")),
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
        }],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            fields: Fields::Unit,
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    assert!(actual.attrs[0].interpret_meta().is_none());
}

#[test]
fn test_attr_with_non_mod_style_path() {
    let raw = r#"
        #[inert <T>]
        struct S;
    "#;

    let expected = DeriveInput {
        ident: ident("S"),
        vis: Visibility::Inherited,
        attrs: vec![Attribute {
            bracket_token: Default::default(),
            pound_token: Default::default(),
            style: AttrStyle::Outer,
            path: Path {
                leading_colon: None,
                segments: punctuated![PathSegment::from(ident("inert"))],
            },
            tts: TokenStream::from_iter(vec![op('<'), word("T"), op('>')]),
        }],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            fields: Fields::Unit,
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    assert!(actual.attrs[0].interpret_meta().is_none());
}

#[test]
fn test_attr_with_mod_style_path_with_self() {
    let raw = r#"
        #[foo::self]
        struct S;
    "#;

    let expected = DeriveInput {
        ident: ident("S"),
        vis: Visibility::Inherited,
        attrs: vec![Attribute {
            bracket_token: Default::default(),
            pound_token: Default::default(),
            style: AttrStyle::Outer,
            path: Path {
                leading_colon: None,
                segments: punctuated![
                    PathSegment::from(ident("foo")),
                    PathSegment::from(ident("self")),
                ],
            },
            tts: TokenStream::new(),
        }],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            fields: Fields::Unit,
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);

    assert!(actual.attrs[0].interpret_meta().is_none());
}

#[test]
fn test_pub_restricted() {
    // Taken from tests/rust/src/test/ui/resolve/auxiliary/privacy-struct-ctor.rs
    let raw = r#"
        pub(in m) struct Z(pub(in m::n) u8);
    "#;

    let expected = DeriveInput {
        ident: ident("Z"),
        vis: Visibility::Restricted(VisRestricted {
            path: Box::new(ident("m").into()),
            in_token: Some(Default::default()),
            paren_token: Default::default(),
            pub_token: Default::default(),
        }),
        attrs: vec![],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            fields: Fields::Unnamed(FieldsUnnamed {
                paren_token: Default::default(),
                unnamed: punctuated![Field {
                    ident: None,
                    vis: Visibility::Restricted(VisRestricted {
                        path: Box::new(Path {
                            leading_colon: None,
                            segments: punctuated![
                                PathSegment::from(ident("m")),
                                PathSegment::from(ident("n")),
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
                        path: ident("u8").into(),
                    }
                    .into(),
                },],
            }),
            semi_token: Some(Default::default()),
            struct_token: Default::default(),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}

#[test]
fn test_vis_crate() {
    let raw = r#"
        crate struct S;
    "#;

    let expected = DeriveInput {
        ident: ident("S"),
        vis: Visibility::Crate(VisCrate {
            crate_token: Default::default(),
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
fn test_pub_restricted_crate() {
    let raw = r#"
        pub(crate) struct S;
    "#;

    let expected = DeriveInput {
        ident: ident("S"),
        vis: Visibility::Restricted(VisRestricted {
            pub_token: Default::default(),
            paren_token: Default::default(),
            in_token: None,
            path: Box::new(ident("crate").into()),
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
        ident: ident("S"),
        vis: Visibility::Restricted(VisRestricted {
            path: Box::new(ident("super").into()),
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
        ident: ident("S"),
        vis: Visibility::Restricted(VisRestricted {
            path: Box::new(ident("super").into()),
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

#[test]
fn test_fields_on_unit_struct() {
    let raw = "struct S;";
    let struct_body = match syn::parse_str::<DeriveInput>(raw).unwrap().data {
        Data::Struct(body) => body,
        _ => panic!("expected a struct"),
    };

    assert_eq!(0, struct_body.fields.iter().count());
}

#[test]
fn test_fields_on_named_struct() {
    let raw = "struct S {
        foo: i32,
        pub bar: String,
    }";
    let struct_body = match syn::parse_str::<DeriveInput>(raw).unwrap().data {
        Data::Struct(body) => body,
        _ => panic!("expected a struct"),
    };

    let expected = vec![
        Field {
            attrs: vec![],
            vis: Visibility::Inherited,
            ident: Some(ident("foo")),
            colon_token: Some(Default::default()),
            ty: syn::parse_str("i32").unwrap(),
        },
        Field {
            attrs: vec![],
            vis: Visibility::Public(VisPublic {
                pub_token: Default::default(),
            }),
            ident: Some(ident("bar")),
            colon_token: Some(Default::default()),
            ty: syn::parse_str("String").unwrap(),
        },
    ];
    let expected = expected.iter().collect::<Vec<_>>();

    assert_eq!(expected, struct_body.fields.iter().collect::<Vec<_>>());
}

#[test]
fn test_fields_on_tuple_struct() {
    let raw = "struct S(i32, pub String);";
    let struct_body = match syn::parse_str::<DeriveInput>(raw).unwrap().data {
        Data::Struct(body) => body,
        _ => panic!("expected a struct"),
    };

    let expected = vec![
        Field {
            attrs: vec![],
            vis: Visibility::Inherited,
            ident: None,
            colon_token: None,
            ty: syn::parse_str("i32").unwrap(),
        },
        Field {
            attrs: vec![],
            vis: Visibility::Public(VisPublic {
                pub_token: Default::default(),
            }),
            ident: None,
            colon_token: None,
            ty: syn::parse_str("String").unwrap(),
        },
    ];
    let expected = expected.iter().collect::<Vec<_>>();

    assert_eq!(expected, struct_body.fields.iter().collect::<Vec<_>>());
}

#[test]
fn test_ambiguous_crate() {
    // The field type is `(crate::X)` not `crate (::X)`.
    let raw = "struct S(crate::X);";

    let expected = DeriveInput {
        ident: ident("S"),
        vis: Visibility::Inherited,
        attrs: vec![],
        generics: Generics::default(),
        data: Data::Struct(DataStruct {
            struct_token: Default::default(),
            fields: Fields::Unnamed(FieldsUnnamed {
                paren_token: Default::default(),
                unnamed: punctuated![Field {
                    attrs: Vec::new(),
                    vis: Visibility::Inherited,
                    ident: None,
                    colon_token: None,
                    ty: Type::Path(TypePath {
                        qself: None,
                        path: Path {
                            leading_colon: None,
                            segments: punctuated![ident("crate").into(), ident("X").into(),],
                        },
                    }),
                }],
            }),
            semi_token: Some(Default::default()),
        }),
    };

    let actual = syn::parse_str(raw).unwrap();

    assert_eq!(expected, actual);
}
