#![cfg(not(miri))]
#![allow(
    clippy::needless_lifetimes,
    clippy::single_element_loop,
    clippy::too_many_lines,
    clippy::uninlined_format_args
)]

#[macro_use]
mod macros;

use proc_macro2::{Delimiter, Group, Span};
use quote::{quote, ToTokens as _};
use std::mem;
use std::process::ExitCode;
use syn::punctuated::Punctuated;
use syn::visit_mut::{self, VisitMut};
use syn::{
    parse_quote, token, Arm, BinOp, Block, Expr, ExprAssign, ExprAwait, ExprBinary, ExprBlock,
    ExprBreak, ExprCall, ExprCast, ExprClosure, ExprField, ExprForLoop, ExprIf, ExprIndex,
    ExprMatch, ExprMethodCall, ExprRange, ExprRawAddr, ExprReference, ExprReturn, ExprTry,
    ExprTuple, ExprUnary, ExprWhile, ExprYield, PointerMutability, RangeLimits, ReturnType, Stmt,
    Token, UnOp,
};

#[test]
fn test_expr_parse() {
    let tokens = quote!(..100u32);
    snapshot!(tokens as Expr, @r#"
    Expr::Range {
        limits: RangeLimits::HalfOpen,
        end: Some(Expr::Lit {
            lit: 100u32,
        }),
    }
    "#);

    let tokens = quote!(..100u32);
    snapshot!(tokens as ExprRange, @r#"
    ExprRange {
        limits: RangeLimits::HalfOpen,
        end: Some(Expr::Lit {
            lit: 100u32,
        }),
    }
    "#);
}

#[test]
fn test_await() {
    // Must not parse as Expr::Field.
    let tokens = quote!(fut.await);

    snapshot!(tokens as Expr, @r#"
    Expr::Await {
        base: Expr::Path {
            path: Path {
                segments: [
                    PathSegment {
                        ident: "fut",
                    },
                ],
            },
        },
    }
    "#);
}

#[rustfmt::skip]
#[test]
fn test_tuple_multi_index() {
    let expected = snapshot!("tuple.0.0" as Expr, @r#"
    Expr::Field {
        base: Expr::Field {
            base: Expr::Path {
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "tuple",
                        },
                    ],
                },
            },
            member: Member::Unnamed(Index {
                index: 0,
            }),
        },
        member: Member::Unnamed(Index {
            index: 0,
        }),
    }
    "#);

    for &input in &[
        "tuple .0.0",
        "tuple. 0.0",
        "tuple.0 .0",
        "tuple.0. 0",
        "tuple . 0 . 0",
    ] {
        assert_eq!(expected, syn::parse_str(input).unwrap());
    }

    for tokens in [
        quote!(tuple.0.0),
        quote!(tuple .0.0),
        quote!(tuple. 0.0),
        quote!(tuple.0 .0),
        quote!(tuple.0. 0),
        quote!(tuple . 0 . 0),
    ] {
        assert_eq!(expected, syn::parse2(tokens).unwrap());
    }
}

#[test]
fn test_macro_variable_func() {
    // mimics the token stream corresponding to `$fn()`
    let path = Group::new(Delimiter::None, quote!(f));
    let tokens = quote!(#path());

    snapshot!(tokens as Expr, @r#"
    Expr::Call {
        func: Expr::Group {
            expr: Expr::Path {
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "f",
                        },
                    ],
                },
            },
        },
    }
    "#);

    let path = Group::new(Delimiter::None, quote! { #[inside] f });
    let tokens = quote!(#[outside] #path());

    snapshot!(tokens as Expr, @r#"
    Expr::Call {
        attrs: [
            Attribute {
                style: AttrStyle::Outer,
                meta: Meta::Path {
                    segments: [
                        PathSegment {
                            ident: "outside",
                        },
                    ],
                },
            },
        ],
        func: Expr::Group {
            expr: Expr::Path {
                attrs: [
                    Attribute {
                        style: AttrStyle::Outer,
                        meta: Meta::Path {
                            segments: [
                                PathSegment {
                                    ident: "inside",
                                },
                            ],
                        },
                    },
                ],
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "f",
                        },
                    ],
                },
            },
        },
    }
    "#);
}

#[test]
fn test_macro_variable_macro() {
    // mimics the token stream corresponding to `$macro!()`
    let mac = Group::new(Delimiter::None, quote!(m));
    let tokens = quote!(#mac!());

    snapshot!(tokens as Expr, @r#"
    Expr::Macro {
        mac: Macro {
            path: Path {
                segments: [
                    PathSegment {
                        ident: "m",
                    },
                ],
            },
            delimiter: MacroDelimiter::Paren,
            tokens: TokenStream(``),
        },
    }
    "#);
}

#[test]
fn test_macro_variable_struct() {
    // mimics the token stream corresponding to `$struct {}`
    let s = Group::new(Delimiter::None, quote! { S });
    let tokens = quote!(#s {});

    snapshot!(tokens as Expr, @r#"
    Expr::Struct {
        path: Path {
            segments: [
                PathSegment {
                    ident: "S",
                },
            ],
        },
    }
    "#);
}

#[test]
fn test_macro_variable_unary() {
    // mimics the token stream corresponding to `$expr.method()` where expr is `&self`
    let inner = Group::new(Delimiter::None, quote!(&self));
    let tokens = quote!(#inner.method());
    snapshot!(tokens as Expr, @r#"
    Expr::MethodCall {
        receiver: Expr::Group {
            expr: Expr::Reference {
                expr: Expr::Path {
                    path: Path {
                        segments: [
                            PathSegment {
                                ident: "self",
                            },
                        ],
                    },
                },
            },
        },
        method: "method",
    }
    "#);
}

#[test]
fn test_macro_variable_match_arm() {
    // mimics the token stream corresponding to `match v { _ => $expr }`
    let expr = Group::new(Delimiter::None, quote! { #[a] () });
    let tokens = quote!(match v { _ => #expr });
    snapshot!(tokens as Expr, @r#"
    Expr::Match {
        expr: Expr::Path {
            path: Path {
                segments: [
                    PathSegment {
                        ident: "v",
                    },
                ],
            },
        },
        arms: [
            Arm {
                pat: Pat::Wild,
                body: Expr::Group {
                    expr: Expr::Tuple {
                        attrs: [
                            Attribute {
                                style: AttrStyle::Outer,
                                meta: Meta::Path {
                                    segments: [
                                        PathSegment {
                                            ident: "a",
                                        },
                                    ],
                                },
                            },
                        ],
                    },
                },
            },
        ],
    }
    "#);

    let expr = Group::new(Delimiter::None, quote!(loop {} + 1));
    let tokens = quote!(match v { _ => #expr });
    snapshot!(tokens as Expr, @r#"
    Expr::Match {
        expr: Expr::Path {
            path: Path {
                segments: [
                    PathSegment {
                        ident: "v",
                    },
                ],
            },
        },
        arms: [
            Arm {
                pat: Pat::Wild,
                body: Expr::Group {
                    expr: Expr::Binary {
                        left: Expr::Loop {
                            body: Block {
                                stmts: [],
                            },
                        },
                        op: BinOp::Add,
                        right: Expr::Lit {
                            lit: 1,
                        },
                    },
                },
            },
        ],
    }
    "#);
}

// https://github.com/dtolnay/syn/issues/1019
#[test]
fn test_closure_vs_rangefull() {
    #[rustfmt::skip] // rustfmt bug: https://github.com/rust-lang/rustfmt/issues/4808
    let tokens = quote!(|| .. .method());
    snapshot!(tokens as Expr, @r#"
    Expr::MethodCall {
        receiver: Expr::Closure {
            output: ReturnType::Default,
            body: Expr::Range {
                limits: RangeLimits::HalfOpen,
            },
        },
        method: "method",
    }
    "#);
}

#[test]
fn test_postfix_operator_after_cast() {
    syn::parse_str::<Expr>("|| &x as T[0]").unwrap_err();
    syn::parse_str::<Expr>("|| () as ()()").unwrap_err();
}

#[test]
fn test_range_kinds() {
    syn::parse_str::<Expr>("..").unwrap();
    syn::parse_str::<Expr>("..hi").unwrap();
    syn::parse_str::<Expr>("lo..").unwrap();
    syn::parse_str::<Expr>("lo..hi").unwrap();

    syn::parse_str::<Expr>("..=").unwrap_err();
    syn::parse_str::<Expr>("..=hi").unwrap();
    syn::parse_str::<Expr>("lo..=").unwrap_err();
    syn::parse_str::<Expr>("lo..=hi").unwrap();

    syn::parse_str::<Expr>("...").unwrap_err();
    syn::parse_str::<Expr>("...hi").unwrap_err();
    syn::parse_str::<Expr>("lo...").unwrap_err();
    syn::parse_str::<Expr>("lo...hi").unwrap_err();
}

#[test]
fn test_range_precedence() {
    snapshot!(".. .." as Expr, @r#"
    Expr::Range {
        limits: RangeLimits::HalfOpen,
        end: Some(Expr::Range {
            limits: RangeLimits::HalfOpen,
        }),
    }
    "#);

    snapshot!(".. .. ()" as Expr, @r#"
    Expr::Range {
        limits: RangeLimits::HalfOpen,
        end: Some(Expr::Range {
            limits: RangeLimits::HalfOpen,
            end: Some(Expr::Tuple),
        }),
    }
    "#);

    snapshot!("() .. .." as Expr, @r#"
    Expr::Range {
        start: Some(Expr::Tuple),
        limits: RangeLimits::HalfOpen,
        end: Some(Expr::Range {
            limits: RangeLimits::HalfOpen,
        }),
    }
    "#);

    // A range with a lower bound cannot be the upper bound of another range,
    // and a range with an upper bound cannot be the lower bound of another
    // range.
    syn::parse_str::<Expr>(".. x ..").unwrap_err();
    syn::parse_str::<Expr>("x .. x ..").unwrap_err();
}

#[test]
fn test_ambiguous_label() {
    for stmt in [
        quote! {
            return 'label: loop { break 'label 42; };
        },
        quote! {
            break ('label: loop { break 'label 42; });
        },
        quote! {
            break 1 + 'label: loop { break 'label 42; };
        },
        quote! {
            break 'outer 'inner: loop { break 'inner 42; };
        },
    ] {
        syn::parse2::<Stmt>(stmt).unwrap();
    }

    for stmt in [
        // Parentheses required. See https://github.com/rust-lang/rust/pull/87026.
        quote! {
            break 'label: loop { break 'label 42; };
        },
    ] {
        syn::parse2::<Stmt>(stmt).unwrap_err();
    }
}

#[test]
fn test_extended_interpolated_path() {
    let path = Group::new(Delimiter::None, quote!(a::b));

    let tokens = quote!(if #path {});
    snapshot!(tokens as Expr, @r#"
    Expr::If {
        cond: Expr::Group {
            expr: Expr::Path {
                path: Path {
                    segments: [
                        PathSegment {
                            ident: "a",
                        },
                        Token![::],
                        PathSegment {
                            ident: "b",
                        },
                    ],
                },
            },
        },
        then_branch: Block {
            stmts: [],
        },
    }
    "#);

    let tokens = quote!(#path {});
    snapshot!(tokens as Expr, @r#"
    Expr::Struct {
        path: Path {
            segments: [
                PathSegment {
                    ident: "a",
                },
                Token![::],
                PathSegment {
                    ident: "b",
                },
            ],
        },
    }
    "#);

    let tokens = quote!(#path :: c);
    snapshot!(tokens as Expr, @r#"
    Expr::Path {
        path: Path {
            segments: [
                PathSegment {
                    ident: "a",
                },
                Token![::],
                PathSegment {
                    ident: "b",
                },
                Token![::],
                PathSegment {
                    ident: "c",
                },
            ],
        },
    }
    "#);

    let nested = Group::new(Delimiter::None, quote!(a::b || true));
    let tokens = quote!(if #nested && false {});
    snapshot!(tokens as Expr, @r#"
    Expr::If {
        cond: Expr::Binary {
            left: Expr::Group {
                expr: Expr::Binary {
                    left: Expr::Path {
                        path: Path {
                            segments: [
                                PathSegment {
                                    ident: "a",
                                },
                                Token![::],
                                PathSegment {
                                    ident: "b",
                                },
                            ],
                        },
                    },
                    op: BinOp::Or,
                    right: Expr::Lit {
                        lit: Lit::Bool {
                            value: true,
                        },
                    },
                },
            },
            op: BinOp::And,
            right: Expr::Lit {
                lit: Lit::Bool {
                    value: false,
                },
            },
        },
        then_branch: Block {
            stmts: [],
        },
    }
    "#);
}

#[test]
fn test_tuple_comma() {
    let mut expr = ExprTuple {
        attrs: Vec::new(),
        paren_token: token::Paren::default(),
        elems: Punctuated::new(),
    };
    snapshot!(expr.to_token_stream() as Expr, @"Expr::Tuple");

    expr.elems.push_value(parse_quote!(continue));
    // Must not parse to Expr::Paren
    snapshot!(expr.to_token_stream() as Expr, @r#"
    Expr::Tuple {
        elems: [
            Expr::Continue,
            Token![,],
        ],
    }
    "#);

    expr.elems.push_punct(<Token![,]>::default());
    snapshot!(expr.to_token_stream() as Expr, @r#"
    Expr::Tuple {
        elems: [
            Expr::Continue,
            Token![,],
        ],
    }
    "#);

    expr.elems.push_value(parse_quote!(continue));
    snapshot!(expr.to_token_stream() as Expr, @r#"
    Expr::Tuple {
        elems: [
            Expr::Continue,
            Token![,],
            Expr::Continue,
        ],
    }
    "#);

    expr.elems.push_punct(<Token![,]>::default());
    snapshot!(expr.to_token_stream() as Expr, @r#"
    Expr::Tuple {
        elems: [
            Expr::Continue,
            Token![,],
            Expr::Continue,
            Token![,],
        ],
    }
    "#);
}

#[test]
fn test_binop_associativity() {
    // Left to right.
    snapshot!("() + () + ()" as Expr, @r#"
    Expr::Binary {
        left: Expr::Binary {
            left: Expr::Tuple,
            op: BinOp::Add,
            right: Expr::Tuple,
        },
        op: BinOp::Add,
        right: Expr::Tuple,
    }
    "#);

    // Right to left.
    snapshot!("() += () += ()" as Expr, @r#"
    Expr::Binary {
        left: Expr::Tuple,
        op: BinOp::AddAssign,
        right: Expr::Binary {
            left: Expr::Tuple,
            op: BinOp::AddAssign,
            right: Expr::Tuple,
        },
    }
    "#);

    // Parenthesization is required.
    syn::parse_str::<Expr>("() == () == ()").unwrap_err();
}

#[test]
fn test_assign_range_precedence() {
    // Range has higher precedence as the right-hand of an assignment, but
    // ambiguous precedence as the left-hand of an assignment.
    snapshot!("() = () .. ()" as Expr, @r#"
    Expr::Assign {
        left: Expr::Tuple,
        right: Expr::Range {
            start: Some(Expr::Tuple),
            limits: RangeLimits::HalfOpen,
            end: Some(Expr::Tuple),
        },
    }
    "#);

    snapshot!("() += () .. ()" as Expr, @r#"
    Expr::Binary {
        left: Expr::Tuple,
        op: BinOp::AddAssign,
        right: Expr::Range {
            start: Some(Expr::Tuple),
            limits: RangeLimits::HalfOpen,
            end: Some(Expr::Tuple),
        },
    }
    "#);

    syn::parse_str::<Expr>("() .. () = ()").unwrap_err();
    syn::parse_str::<Expr>("() .. () += ()").unwrap_err();
}

#[test]
fn test_chained_comparison() {
    // https://github.com/dtolnay/syn/issues/1738
    let _ = syn::parse_str::<Expr>("a = a < a <");
    let _ = syn::parse_str::<Expr>("a = a .. a ..");
    let _ = syn::parse_str::<Expr>("a = a .. a +=");

    let err = syn::parse_str::<Expr>("a < a < a").unwrap_err();
    assert_eq!("comparison operators cannot be chained", err.to_string());

    let err = syn::parse_str::<Expr>("a .. a .. a").unwrap_err();
    assert_eq!("unexpected token", err.to_string());

    let err = syn::parse_str::<Expr>("a .. a += a").unwrap_err();
    assert_eq!("unexpected token", err.to_string());
}

#[test]
fn test_fixup() {
    struct FlattenParens;

    impl VisitMut for FlattenParens {
        fn visit_expr_mut(&mut self, e: &mut Expr) {
            while let Expr::Paren(paren) = e {
                *e = mem::replace(&mut *paren.expr, Expr::PLACEHOLDER);
            }
            visit_mut::visit_expr_mut(self, e);
        }
    }

    for tokens in [
        quote! { 2 * (1 + 1) },
        quote! { 0 + (0 + 0) },
        quote! { (a = b) = c },
        quote! { (x as i32) < 0 },
        quote! { 1 + (x as i32) < 0 },
        quote! { (1 + 1).abs() },
        quote! { (lo..hi)[..] },
        quote! { (a..b)..(c..d) },
        quote! { (&mut fut).await },
        quote! { &mut (x as i32) },
        quote! { -(x as i32) },
        quote! { if (S {} == 1) {} },
        quote! { { (m! {}) - 1 } },
        quote! { match m { _ => ({}) - 1 } },
        quote! { if let _ = (a && b) && c {} },
        quote! { if let _ = (S {}) {} },
        quote! { break ('a: loop { break 'a 1 } + 1) },
        quote! { a + (|| b) + c },
        quote! { if let _ = ((break) - 1 || true) {} },
        quote! { if let _ = (break + 1 || true) {} },
        quote! { (break)() },
        quote! { (..) = () },
        quote! { (..) += () },
        quote! { (1 < 2) == (3 < 4) },
        quote! { { (let _ = ()) } },
        quote! { (#[attr] thing).field },
        quote! { (self.f)() },
        quote! { (return)..=return },
        quote! { 1 + (return)..=1 + return },
    ] {
        let original: Expr = syn::parse2(tokens).unwrap();

        let mut flat = original.clone();
        FlattenParens.visit_expr_mut(&mut flat);
        let reconstructed: Expr = match syn::parse2(flat.to_token_stream()) {
            Ok(reconstructed) => reconstructed,
            Err(err) => panic!("failed to parse `{}`: {}", flat.to_token_stream(), err),
        };

        assert!(
            original == reconstructed,
            "original: {}\nreconstructed: {}",
            original.to_token_stream(),
            reconstructed.to_token_stream(),
        );
    }
}

#[test]
fn test_permutations() -> ExitCode {
    fn iter(depth: usize, f: &mut dyn FnMut(Expr)) {
        // Expr::Path
        f(parse_quote!(x));
        f(parse_quote!(x::<T>));
        f(parse_quote!(<T as Trait>::CONST));

        let Some(depth) = depth.checked_sub(1) else {
            return;
        };

        // Expr::Array
        f(parse_quote!([]));

        // Expr::Assign
        iter(depth, &mut |expr| {
            iter(0, &mut |simple| {
                f(Expr::Assign(ExprAssign {
                    attrs: Vec::new(),
                    left: Box::new(simple.clone()),
                    eq_token: Token![=](Span::call_site()),
                    right: Box::new(expr.clone()),
                }));
                f(Expr::Assign(ExprAssign {
                    attrs: Vec::new(),
                    left: Box::new(expr.clone()),
                    eq_token: Token![=](Span::call_site()),
                    right: Box::new(simple),
                }));
            });
        });

        // Expr::Async
        f(parse_quote!(async {}));

        // Expr::Await
        iter(depth, &mut |base| {
            f(Expr::Await(ExprAwait {
                attrs: Vec::new(),
                base: Box::new(base),
                dot_token: Token![.](Span::call_site()),
                await_token: Token![await](Span::call_site()),
            }));
        });

        // Expr::Binary
        iter(depth, &mut |expr| {
            iter(0, &mut |simple| {
                for op in [
                    BinOp::Add(Token![+](Span::call_site())),
                    BinOp::Sub(Token![-](Span::call_site())),
                    BinOp::Mul(Token![*](Span::call_site())),
                    BinOp::Div(Token![/](Span::call_site())),
                    BinOp::Rem(Token![%](Span::call_site())),
                    BinOp::And(Token![&&](Span::call_site())),
                    BinOp::Or(Token![||](Span::call_site())),
                    BinOp::BitXor(Token![^](Span::call_site())),
                    BinOp::BitAnd(Token![&](Span::call_site())),
                    BinOp::BitOr(Token![|](Span::call_site())),
                    BinOp::Shl(Token![<<](Span::call_site())),
                    BinOp::Shr(Token![>>](Span::call_site())),
                    BinOp::Eq(Token![==](Span::call_site())),
                    BinOp::Lt(Token![<](Span::call_site())),
                    BinOp::Le(Token![<=](Span::call_site())),
                    BinOp::Ne(Token![!=](Span::call_site())),
                    BinOp::Ge(Token![>=](Span::call_site())),
                    BinOp::Gt(Token![>](Span::call_site())),
                    BinOp::AddAssign(Token![+=](Span::call_site())),
                    BinOp::SubAssign(Token![-=](Span::call_site())),
                    BinOp::MulAssign(Token![*=](Span::call_site())),
                    BinOp::DivAssign(Token![/=](Span::call_site())),
                    BinOp::RemAssign(Token![%=](Span::call_site())),
                    BinOp::BitXorAssign(Token![^=](Span::call_site())),
                    BinOp::BitAndAssign(Token![&=](Span::call_site())),
                    BinOp::BitOrAssign(Token![|=](Span::call_site())),
                    BinOp::ShlAssign(Token![<<=](Span::call_site())),
                    BinOp::ShrAssign(Token![>>=](Span::call_site())),
                ] {
                    f(Expr::Binary(ExprBinary {
                        attrs: Vec::new(),
                        left: Box::new(simple.clone()),
                        op,
                        right: Box::new(expr.clone()),
                    }));
                    f(Expr::Binary(ExprBinary {
                        attrs: Vec::new(),
                        left: Box::new(expr.clone()),
                        op,
                        right: Box::new(simple.clone()),
                    }));
                }
            });
        });

        // Expr::Block
        f(parse_quote!('a: {}));
        iter(depth, &mut |expr| {
            f(Expr::Block(ExprBlock {
                attrs: Vec::new(),
                label: None,
                block: Block {
                    brace_token: token::Brace(Span::call_site()),
                    stmts: Vec::from([Stmt::Expr(expr.clone(), None)]),
                },
            }));
            f(Expr::Block(ExprBlock {
                attrs: Vec::new(),
                label: None,
                block: Block {
                    brace_token: token::Brace(Span::call_site()),
                    stmts: Vec::from([Stmt::Expr(
                        expr.clone(),
                        Some(Token![;](Span::call_site())),
                    )]),
                },
            }));
        });

        // Expr::Break
        f(parse_quote!(break));
        f(parse_quote!(break 'a));
        iter(depth, &mut |expr| {
            f(Expr::Break(ExprBreak {
                attrs: Vec::new(),
                break_token: Token![break](Span::call_site()),
                label: None,
                expr: Some(Box::new(expr.clone())),
            }));
            f(Expr::Break(ExprBreak {
                attrs: Vec::new(),
                break_token: Token![break](Span::call_site()),
                label: Some(parse_quote!('a)),
                expr: Some(Box::new(expr)),
            }));
        });

        // Expr::Call
        iter(depth, &mut |expr| {
            f(Expr::Call(ExprCall {
                attrs: Vec::new(),
                func: Box::new(expr),
                paren_token: token::Paren(Span::call_site()),
                args: Punctuated::new(),
            }));
        });

        // Expr::Cast
        iter(depth, &mut |expr| {
            f(Expr::Cast(ExprCast {
                attrs: Vec::new(),
                expr: Box::new(expr.clone()),
                as_token: Token![as](Span::call_site()),
                ty: parse_quote!(T),
            }));
            f(Expr::Cast(ExprCast {
                attrs: Vec::new(),
                expr: Box::new(expr),
                as_token: Token![as](Span::call_site()),
                ty: parse_quote!(Thing<T>),
            }));
        });

        // Expr::Closure
        iter(depth, &mut |expr| {
            f(Expr::Closure(ExprClosure {
                attrs: Vec::new(),
                lifetimes: None,
                constness: None,
                movability: None,
                asyncness: None,
                capture: None,
                or1_token: Token![|](Span::call_site()),
                inputs: Punctuated::new(),
                or2_token: Token![|](Span::call_site()),
                output: ReturnType::Default,
                body: Box::new(expr.clone()),
            }));
            f(Expr::Closure(ExprClosure {
                attrs: Vec::new(),
                lifetimes: None,
                constness: None,
                movability: None,
                asyncness: None,
                capture: None,
                or1_token: Token![|](Span::call_site()),
                inputs: Punctuated::new(),
                or2_token: Token![|](Span::call_site()),
                output: ReturnType::Type(Token![->](Span::call_site()), parse_quote!(T)),
                body: Box::new(expr),
            }));
        });

        // Expr::Const
        f(parse_quote!(const {}));

        // Expr::Continue
        f(parse_quote!(continue));
        f(parse_quote!(continue 'a));

        // Expr::Field
        iter(depth, &mut |expr| {
            f(Expr::Field(ExprField {
                attrs: Vec::new(),
                base: Box::new(expr.clone()),
                dot_token: Token![.](Span::call_site()),
                member: parse_quote!(field),
            }));
            f(Expr::Field(ExprField {
                attrs: Vec::new(),
                base: Box::new(expr),
                dot_token: Token![.](Span::call_site()),
                member: parse_quote!(0),
            }));
        });

        // Expr::ForLoop
        iter(depth, &mut |expr| {
            f(Expr::ForLoop(ExprForLoop {
                attrs: Vec::new(),
                label: None,
                for_token: Token![for](Span::call_site()),
                pat: parse_quote!(_),
                in_token: Token![in](Span::call_site()),
                expr: Box::new(expr.clone()),
                body: parse_quote!({}),
            }));
            f(Expr::ForLoop(ExprForLoop {
                attrs: Vec::new(),
                label: Some(parse_quote!('a:)),
                for_token: Token![for](Span::call_site()),
                pat: parse_quote!(_),
                in_token: Token![in](Span::call_site()),
                expr: Box::new(expr),
                body: parse_quote!({}),
            }));
        });

        // Expr::If
        iter(depth, &mut |expr| {
            f(Expr::If(ExprIf {
                attrs: Vec::new(),
                if_token: Token![if](Span::call_site()),
                cond: Box::new(expr),
                then_branch: parse_quote!({}),
                else_branch: None,
            }));
        });

        // Expr::Index
        iter(depth, &mut |expr| {
            f(Expr::Index(ExprIndex {
                attrs: Vec::new(),
                expr: Box::new(expr),
                bracket_token: token::Bracket(Span::call_site()),
                index: parse_quote!(0),
            }));
        });

        // Expr::Loop
        f(parse_quote!(loop {}));
        f(parse_quote!('a: loop {}));

        // Expr::Macro
        f(parse_quote!(m!()));
        f(parse_quote!(m! {}));

        // Expr::Match
        iter(depth, &mut |expr| {
            f(Expr::Match(ExprMatch {
                attrs: Vec::new(),
                match_token: Token![match](Span::call_site()),
                expr: Box::new(expr.clone()),
                brace_token: token::Brace(Span::call_site()),
                arms: Vec::new(),
            }));
            f(Expr::Match(ExprMatch {
                attrs: Vec::new(),
                match_token: Token![match](Span::call_site()),
                expr: parse_quote!(x),
                brace_token: token::Brace(Span::call_site()),
                arms: Vec::from([Arm {
                    attrs: Vec::new(),
                    pat: parse_quote!(_),
                    guard: None,
                    fat_arrow_token: Token![=>](Span::call_site()),
                    body: Box::new(expr.clone()),
                    comma: None,
                }]),
            }));
            f(Expr::Match(ExprMatch {
                attrs: Vec::new(),
                match_token: Token![match](Span::call_site()),
                expr: parse_quote!(x),
                brace_token: token::Brace(Span::call_site()),
                arms: Vec::from([Arm {
                    attrs: Vec::new(),
                    pat: parse_quote!(_),
                    guard: Some((Token![if](Span::call_site()), Box::new(expr))),
                    fat_arrow_token: Token![=>](Span::call_site()),
                    body: parse_quote!({}),
                    comma: None,
                }]),
            }));
        });

        // Expr::MethodCall
        iter(depth, &mut |expr| {
            f(Expr::MethodCall(ExprMethodCall {
                attrs: Vec::new(),
                receiver: Box::new(expr.clone()),
                dot_token: Token![.](Span::call_site()),
                method: parse_quote!(method),
                turbofish: None,
                paren_token: token::Paren(Span::call_site()),
                args: Punctuated::new(),
            }));
            f(Expr::MethodCall(ExprMethodCall {
                attrs: Vec::new(),
                receiver: Box::new(expr),
                dot_token: Token![.](Span::call_site()),
                method: parse_quote!(method),
                turbofish: Some(parse_quote!(::<T>)),
                paren_token: token::Paren(Span::call_site()),
                args: Punctuated::new(),
            }));
        });

        // Expr::Range
        f(parse_quote!(..));
        f(parse_quote!(0..));
        f(parse_quote!(..0));
        iter(depth, &mut |expr| {
            f(Expr::Range(ExprRange {
                attrs: Vec::new(),
                start: None,
                limits: RangeLimits::HalfOpen(Token![..](Span::call_site())),
                end: Some(Box::new(expr.clone())),
            }));
            f(Expr::Range(ExprRange {
                attrs: Vec::new(),
                start: Some(Box::new(expr.clone())),
                limits: RangeLimits::HalfOpen(Token![..](Span::call_site())),
                end: None,
            }));
        });

        // Expr::RawAddr
        iter(depth, &mut |expr| {
            f(Expr::RawAddr(ExprRawAddr {
                attrs: Vec::new(),
                and_token: Token![&](Span::call_site()),
                raw: Token![raw](Span::call_site()),
                mutability: PointerMutability::Const(Token![const](Span::call_site())),
                expr: Box::new(expr),
            }));
        });

        // Expr::Reference
        iter(depth, &mut |expr| {
            f(Expr::Reference(ExprReference {
                attrs: Vec::new(),
                and_token: Token![&](Span::call_site()),
                mutability: None,
                expr: Box::new(expr.clone()),
            }));
            f(Expr::Reference(ExprReference {
                attrs: Vec::new(),
                and_token: Token![&](Span::call_site()),
                mutability: Some(Token![mut](Span::call_site())),
                expr: Box::new(expr),
            }));
        });

        // Expr::Return
        f(parse_quote!(return));
        iter(depth, &mut |expr| {
            f(Expr::Return(ExprReturn {
                attrs: Vec::new(),
                return_token: Token![return](Span::call_site()),
                expr: Some(Box::new(expr)),
            }));
        });

        // Expr::Struct
        f(parse_quote!(Struct {}));

        // Expr::Try
        iter(depth, &mut |expr| {
            f(Expr::Try(ExprTry {
                attrs: Vec::new(),
                expr: Box::new(expr),
                question_token: Token![?](Span::call_site()),
            }));
        });

        // Expr::TryBlock
        f(parse_quote!(try {}));

        // Expr::Unary
        iter(depth, &mut |expr| {
            for op in [
                UnOp::Deref(Token![*](Span::call_site())),
                UnOp::Not(Token![!](Span::call_site())),
                UnOp::Neg(Token![-](Span::call_site())),
            ] {
                f(Expr::Unary(ExprUnary {
                    attrs: Vec::new(),
                    op,
                    expr: Box::new(expr.clone()),
                }));
            }
        });

        // Expr::Unsafe
        f(parse_quote!(unsafe {}));

        // Expr::While
        iter(depth, &mut |expr| {
            f(Expr::While(ExprWhile {
                attrs: Vec::new(),
                label: None,
                while_token: Token![while](Span::call_site()),
                cond: Box::new(expr.clone()),
                body: parse_quote!({}),
            }));
            f(Expr::While(ExprWhile {
                attrs: Vec::new(),
                label: Some(parse_quote!('a:)),
                while_token: Token![while](Span::call_site()),
                cond: Box::new(expr),
                body: parse_quote!({}),
            }));
        });

        // Expr::Yield
        f(parse_quote!(yield));
        iter(depth, &mut |expr| {
            f(Expr::Yield(ExprYield {
                attrs: Vec::new(),
                yield_token: Token![yield](Span::call_site()),
                expr: Some(Box::new(expr)),
            }));
        });
    }

    let mut status = ExitCode::SUCCESS;
    macro_rules! fail {
        ($($message:tt)*) => {{
            eprintln!($($message)*);
            status = ExitCode::FAILURE;
            return;
        }};
    }
    let mut assert = |expr: Expr| {
        let tokens = expr.to_token_stream();
        if syn::parse2::<Expr>(tokens.clone()).is_err() {
            fail!("failed to parse: {}", tokens);
        }
    };

    iter(2, &mut assert);
    status
}
