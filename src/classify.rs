use crate::expr::Expr;
use crate::generics::TypeParamBound;
use crate::path::{Path, PathArguments};
use crate::punctuated::Punctuated;
use crate::ty::{ReturnType, Type};
use proc_macro2::{Delimiter, TokenStream, TokenTree};
use std::ops::ControlFlow;

#[cfg(feature = "parsing")]
pub(crate) fn requires_semi_to_be_stmt(expr: &Expr) -> bool {
    match expr {
        Expr::Macro(expr) => !expr.mac.delimiter.is_brace(),
        _ => requires_comma_to_be_match_arm(expr),
    }
}

pub(crate) fn requires_comma_to_be_match_arm(expr: &Expr) -> bool {
    match expr {
        Expr::If(_)
        | Expr::Match(_)
        | Expr::Block(_) | Expr::Unsafe(_) // both under ExprKind::Block in rustc
        | Expr::While(_)
        | Expr::Loop(_)
        | Expr::ForLoop(_)
        | Expr::TryBlock(_)
        | Expr::Const(_) => false,

        Expr::Array(_)
        | Expr::Assign(_)
        | Expr::Async(_)
        | Expr::Await(_)
        | Expr::Binary(_)
        | Expr::Break(_)
        | Expr::Call(_)
        | Expr::Cast(_)
        | Expr::Closure(_)
        | Expr::Continue(_)
        | Expr::Field(_)
        | Expr::Group(_)
        | Expr::Index(_)
        | Expr::Infer(_)
        | Expr::Let(_)
        | Expr::Lit(_)
        | Expr::Macro(_)
        | Expr::MethodCall(_)
        | Expr::Paren(_)
        | Expr::Path(_)
        | Expr::Range(_)
        | Expr::Reference(_)
        | Expr::Repeat(_)
        | Expr::Return(_)
        | Expr::Struct(_)
        | Expr::Try(_)
        | Expr::Tuple(_)
        | Expr::Unary(_)
        | Expr::Yield(_)
        | Expr::Verbatim(_) => true
    }
}

/// Whether the expression's last token is `}`.
pub(crate) fn expr_trailing_brace(mut expr: &Expr) -> bool {
    loop {
        match expr {
            Expr::Async(_)
            | Expr::Block(_)
            | Expr::Const(_)
            | Expr::ForLoop(_)
            | Expr::If(_)
            | Expr::Loop(_)
            | Expr::Match(_)
            | Expr::Struct(_)
            | Expr::TryBlock(_)
            | Expr::Unsafe(_)
            | Expr::While(_) => return true,

            Expr::Assign(e) => expr = &e.right,
            Expr::Binary(e) => expr = &e.right,
            Expr::Break(e) => match &e.expr {
                Some(e) => expr = e,
                None => return false,
            },
            Expr::Cast(e) => return type_trailing_brace(&e.ty),
            Expr::Closure(e) => expr = &e.body,
            Expr::Let(e) => expr = &e.expr,
            Expr::Macro(e) => return e.mac.delimiter.is_brace(),
            Expr::Range(e) => match &e.end {
                Some(end) => expr = end,
                None => return false,
            },
            Expr::Reference(e) => expr = &e.expr,
            Expr::Return(e) => match &e.expr {
                Some(e) => expr = e,
                None => return false,
            },
            Expr::Unary(e) => expr = &e.expr,
            Expr::Verbatim(e) => return tokens_trailing_brace(e),
            Expr::Yield(e) => match &e.expr {
                Some(e) => expr = e,
                None => return false,
            },

            Expr::Array(_)
            | Expr::Await(_)
            | Expr::Call(_)
            | Expr::Continue(_)
            | Expr::Field(_)
            | Expr::Group(_)
            | Expr::Index(_)
            | Expr::Infer(_)
            | Expr::Lit(_)
            | Expr::MethodCall(_)
            | Expr::Paren(_)
            | Expr::Path(_)
            | Expr::Repeat(_)
            | Expr::Try(_)
            | Expr::Tuple(_) => return false,
        }
    }

    fn type_trailing_brace(mut ty: &Type) -> bool {
        loop {
            match ty {
                Type::BareFn(t) => match &t.output {
                    ReturnType::Default => return false,
                    ReturnType::Type(_, ret) => ty = ret,
                },
                Type::ImplTrait(t) => match last_type_in_bounds(&t.bounds) {
                    ControlFlow::Break(trailing_brace) => return trailing_brace,
                    ControlFlow::Continue(t) => ty = t,
                },
                Type::Macro(t) => return t.mac.delimiter.is_brace(),
                Type::Path(t) => match last_type_in_path(&t.path) {
                    Some(t) => ty = t,
                    None => return false,
                },
                Type::Ptr(t) => ty = &t.elem,
                Type::Reference(t) => ty = &t.elem,
                Type::TraitObject(t) => match last_type_in_bounds(&t.bounds) {
                    ControlFlow::Break(trailing_brace) => return trailing_brace,
                    ControlFlow::Continue(t) => ty = t,
                },
                Type::Verbatim(t) => return tokens_trailing_brace(t),

                Type::Array(_)
                | Type::Group(_)
                | Type::Infer(_)
                | Type::Never(_)
                | Type::Paren(_)
                | Type::Slice(_)
                | Type::Tuple(_) => return false,
            }
        }
    }

    fn last_type_in_path(path: &Path) -> Option<&Type> {
        match &path.segments.last().unwrap().arguments {
            PathArguments::None | PathArguments::AngleBracketed(_) => None,
            PathArguments::Parenthesized(arg) => match &arg.output {
                ReturnType::Default => None,
                ReturnType::Type(_, ret) => Some(ret),
            },
        }
    }

    fn last_type_in_bounds(
        bounds: &Punctuated<TypeParamBound, Token![+]>,
    ) -> ControlFlow<bool, &Type> {
        match bounds.last().unwrap() {
            TypeParamBound::Trait(t) => match last_type_in_path(&t.path) {
                Some(t) => ControlFlow::Continue(t),
                None => ControlFlow::Break(false),
            },
            TypeParamBound::Lifetime(_) => ControlFlow::Break(false),
            TypeParamBound::Verbatim(t) => ControlFlow::Break(tokens_trailing_brace(t)),
        }
    }

    fn tokens_trailing_brace(tokens: &TokenStream) -> bool {
        if let Some(TokenTree::Group(last)) = tokens.clone().into_iter().last() {
            last.delimiter() == Delimiter::Brace
        } else {
            false
        }
    }
}
