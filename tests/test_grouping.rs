#![cfg(all(feature = "extra-traits", feature = "full"))]

extern crate syn;
use syn::{Expr, ExprKind, ExprGroup, ExprBinary, Lit, LitKind, BinOp};

extern crate synom;
use synom::{tokens, Synom};

extern crate proc_macro2;
use proc_macro2::*;

fn tt(k: TokenKind) -> TokenTree {
    TokenTree {
        span: Span::default(),
        kind: k,
    }
}

fn expr<T: Into<ExprKind>>(t: T) -> Expr {
    t.into().into()
}

fn lit<T: Into<Literal>>(t: T) -> Expr {
    expr(Lit {
        value: LitKind::Other(t.into()),
        span: syn::Span::default(),
    })
}

#[test]
fn test_grouping() {
    let raw: TokenStream = vec![
        tt(TokenKind::Literal(Literal::from(1))),
        tt(TokenKind::Op('+', OpKind::Alone)),
        tt(TokenKind::Sequence(Delimiter::None, vec![
            tt(TokenKind::Literal(Literal::from(2))),
            tt(TokenKind::Op('+', OpKind::Alone)),
            tt(TokenKind::Literal(Literal::from(3))),
        ].into_iter().collect())),
        tt(TokenKind::Op('*', OpKind::Alone)),
        tt(TokenKind::Literal(Literal::from(4))),
    ].into_iter().collect();

    assert_eq!(raw.to_string(), "1i32 +  2i32 + 3i32  * 4i32");

    assert_eq!(Expr::parse_all(raw).unwrap(), expr(ExprBinary {
        left: Box::new(lit(1)),
        op: BinOp::Add(tokens::Add::default()),
        right: Box::new(expr(ExprBinary {
            left: Box::new(expr(ExprGroup {
                group_token: tokens::Group::default(),
                expr: Box::new(expr(ExprBinary {
                    left: Box::new(lit(2)),
                    op: BinOp::Add(tokens::Add::default()),
                    right: Box::new(lit(3)),
                })),
            })),
            op: BinOp::Mul(tokens::Star::default()),
            right: Box::new(lit(4)),
        })),
    }));
}

#[test]
fn test_invalid_grouping() {
    let raw: TokenStream = vec![
        tt(TokenKind::Literal(Literal::from(1))),
        tt(TokenKind::Op('+', OpKind::Alone)),
        tt(TokenKind::Sequence(Delimiter::None, vec![
            tt(TokenKind::Literal(Literal::from(2))),
            tt(TokenKind::Op('+', OpKind::Alone)),
        ].into_iter().collect())),
        tt(TokenKind::Literal(Literal::from(3))),
        tt(TokenKind::Op('*', OpKind::Alone)),
        tt(TokenKind::Literal(Literal::from(4))),
    ].into_iter().collect();

    assert_eq!(raw.to_string(), "1i32 +  2i32 +  3i32 * 4i32");

    assert_eq!(Expr::parse_all(raw).unwrap(), expr(ExprBinary {
        left: Box::new(expr(ExprBinary {
            left: Box::new(lit(1)),
            op: BinOp::Add(tokens::Add::default()),
            right: Box::new(lit(2)),
        })),
        op: BinOp::Add(tokens::Add::default()),
        right: Box::new(expr(ExprBinary {
            left: Box::new(lit(3)),
            op: BinOp::Mul(tokens::Star::default()),
            right: Box::new(lit(4)),
        })),
    }));
}
