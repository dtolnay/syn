use crate::expr::Expr;
#[cfg(feature = "parsing")]
use crate::generics::TypeParamBound;
#[cfg(feature = "parsing")]
use crate::path::{Path, PathArguments};
#[cfg(feature = "parsing")]
use crate::punctuated::Punctuated;
#[cfg(feature = "parsing")]
use crate::ty::{ReturnType, Type};
#[cfg(feature = "parsing")]
use proc_macro2::{Delimiter, TokenStream, TokenTree};
#[cfg(feature = "parsing")]
use std::ops::ControlFlow;

pub(crate) fn requires_terminator(expr: &Expr) -> bool {
    // see https://github.com/rust-lang/rust/blob/9a19e7604/compiler/rustc_ast/src/util/classify.rs#L7-L26
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
#[cfg(feature = "parsing")]
pub(crate) fn expr_trailing_brace(mut expr: &Expr) -> bool {
    loop {
        match expr {
            Expr::Array(_) => return false,
            Expr::Assign(e) => expr = &e.right,
            Expr::Async(_) => return true,
            Expr::Await(_) => return false,
            Expr::Binary(e) => expr = &e.right,
            Expr::Block(_) => return true,
            Expr::Break(e) => match &e.expr {
                Some(e) => expr = e,
                None => return false,
            },
            Expr::Call(_) => return false,
            Expr::Cast(e) => return type_trailing_brace(&e.ty),
            Expr::Closure(e) => expr = &e.body,
            Expr::Const(_) => return true,
            Expr::Continue(_) => return false,
            Expr::Field(_) => return false,
            Expr::ForLoop(_) => return true,
            Expr::Group(_) => return false,
            Expr::If(_) => return true,
            Expr::Index(_) => return false,
            Expr::Infer(_) => return false,
            Expr::Let(e) => expr = &e.expr,
            Expr::Lit(_) => return false,
            Expr::Loop(_) => return true,
            Expr::Macro(e) => return e.mac.delimiter.is_brace(),
            Expr::Match(_) => return true,
            Expr::MethodCall(_) => return false,
            Expr::Paren(_) => return false,
            Expr::Path(_) => return false,
            Expr::Range(e) => match &e.end {
                Some(end) => expr = end,
                None => return false,
            },
            Expr::Reference(e) => expr = &e.expr,
            Expr::Repeat(_) => return false,
            Expr::Return(e) => match &e.expr {
                Some(e) => expr = e,
                None => return false,
            },
            Expr::Struct(_) => return true,
            Expr::Try(_) => return false,
            Expr::TryBlock(_) => return true,
            Expr::Tuple(_) => return false,
            Expr::Unary(e) => expr = &e.expr,
            Expr::Unsafe(_) => return true,
            Expr::Verbatim(e) => return tokens_trailing_brace(e),
            Expr::While(_) => return true,
            Expr::Yield(e) => match &e.expr {
                Some(e) => expr = e,
                None => return false,
            },
        }
    }
}

#[cfg(feature = "parsing")]
fn type_trailing_brace(mut ty: &Type) -> bool {
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

    loop {
        match ty {
            Type::Array(_) => return false,
            Type::BareFn(t) => match &t.output {
                ReturnType::Default => return false,
                ReturnType::Type(_, ret) => ty = ret,
            },
            Type::Group(_) => return false,
            Type::ImplTrait(t) => match last_type_in_bounds(&t.bounds) {
                ControlFlow::Break(trailing_brace) => return trailing_brace,
                ControlFlow::Continue(t) => ty = t,
            },
            Type::Infer(_) => return false,
            Type::Macro(t) => return t.mac.delimiter.is_brace(),
            Type::Never(_) => return false,
            Type::Paren(_) => return false,
            Type::Path(t) => match last_type_in_path(&t.path) {
                Some(t) => ty = t,
                None => return false,
            },
            Type::Ptr(t) => ty = &t.elem,
            Type::Reference(t) => ty = &t.elem,
            Type::Slice(_) => return false,
            Type::TraitObject(t) => match last_type_in_bounds(&t.bounds) {
                ControlFlow::Break(trailing_brace) => return trailing_brace,
                ControlFlow::Continue(t) => ty = t,
            },
            Type::Tuple(_) => return false,
            Type::Verbatim(t) => return tokens_trailing_brace(t),
        }
    }
}

#[cfg(feature = "parsing")]
fn tokens_trailing_brace(tokens: &TokenStream) -> bool {
    if let Some(TokenTree::Group(last)) = tokens.clone().into_iter().last() {
        last.delimiter() == Delimiter::Brace
    } else {
        false
    }
}
