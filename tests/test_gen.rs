#![allow(clippy::uninlined_format_args)]

#[macro_use]
mod macros;

use syn::{Expr, Item};

#[test]
fn test_gen_block() {
    let input = "gen { }";

    snapshot!(input as Expr, @r###"
    Expr::Gen {
        block: Block {
            stmts: [],
        },
    }
    "###);
}

#[test]
fn test_gen_move_block() {
    let input = "gen move { }";

    snapshot!(input as Expr, @r###"
    Expr::Gen {
        capture: Some,
        block: Block {
            stmts: [],
        },
    }
    "###);
}

#[test]
fn test_async_gen_block() {
    let input = "async gen { }";

    snapshot!(input as Expr, @r###"
    Expr::Gen {
        async_token: Some,
        block: Block {
            stmts: [],
        },
    }
    "###);
}

#[test]
fn test_async_gen_move_block() {
    let input = "async gen move { }";

    snapshot!(input as Expr, @r###"
    Expr::Gen {
        async_token: Some,
        capture: Some,
        block: Block {
            stmts: [],
        },
    }
    "###);
}

#[test]
fn test_gen_fn() {
    let input = "gen fn foo() -> i32 { }";

    snapshot!(input as Item, @r###"
    Item::Fn {
        vis: Visibility::Inherited,
        sig: Signature {
            generator: Some,
            ident: "foo",
            generics: Generics,
            output: ReturnType::Type(
                Type::Path {
                    path: Path {
                        segments: [
                            PathSegment {
                                ident: "i32",
                            },
                        ],
                    },
                },
            ),
        },
        block: Block {
            stmts: [],
        },
    }
    "###);
}


#[test]
fn test_async_gen_fn() {
    let input = "async gen fn foo() -> i32 { }";

    snapshot!(input as Item, @r###"
    Item::Fn {
        vis: Visibility::Inherited,
        sig: Signature {
            asyncness: Some,
            generator: Some,
            ident: "foo",
            generics: Generics,
            output: ReturnType::Type(
                Type::Path {
                    path: Path {
                        segments: [
                            PathSegment {
                                ident: "i32",
                            },
                        ],
                    },
                },
            ),
        },
        block: Block {
            stmts: [],
        },
    }
    "###);
}
