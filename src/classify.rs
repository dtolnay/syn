#[cfg(feature = "full")]
use crate::expr::Expr;
#[cfg(any(feature = "printing", feature = "full"))]
use crate::generics::TypeParamBound;
#[cfg(any(feature = "printing", feature = "full"))]
use crate::path::{Path, PathArguments};
#[cfg(any(feature = "printing", feature = "full"))]
use crate::punctuated::Punctuated;
#[cfg(any(feature = "printing", feature = "full"))]
use crate::ty::{ReturnType, Type};
#[cfg(feature = "full")]
use proc_macro2::{Delimiter, TokenStream, TokenTree};
#[cfg(any(feature = "printing", feature = "full"))]
use std::ops::ControlFlow;

#[cfg(feature = "full")]
pub(crate) fn requires_semi_to_be_stmt(expr: &Expr) -> bool {
    match expr {
        Expr::Macro(expr) => !expr.mac.delimiter.is_brace(),
        _ => requires_comma_to_be_match_arm(expr),
    }
}

#[cfg(feature = "full")]
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
        | Expr::RawAddr(_)
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

#[cfg(all(feature = "printing", feature = "full"))]
pub(crate) fn confusable_with_adjacent_block(expr: &Expr) -> bool {
    let jump = false;
    let rightmost_subexpression = true;
    return confusable(expr, jump, rightmost_subexpression);

    fn confusable(expr: &Expr, jump: bool, rightmost_subexpression: bool) -> bool {
        match expr {
            Expr::Assign(e) => {
                confusable(&e.left, jump, false)
                    || confusable(&e.right, jump, rightmost_subexpression)
            }
            Expr::Await(e) => confusable(&e.base, jump, false),
            Expr::Binary(e) => {
                confusable(&e.left, jump, false)
                    || confusable(&e.right, jump, rightmost_subexpression)
            }
            Expr::Break(e) => {
                if let Some(value) = &e.expr {
                    matches!(**value, Expr::Block(_))
                        || confusable(value, true, rightmost_subexpression)
                } else {
                    false
                }
            }
            Expr::Call(e) => confusable(&e.func, jump, false),
            Expr::Cast(e) => confusable(&e.expr, jump, false),
            Expr::Closure(e) => confusable(&e.body, true, rightmost_subexpression),
            Expr::Field(e) => confusable(&e.base, jump, false),
            Expr::Index(e) => confusable(&e.expr, jump, false),
            Expr::MethodCall(e) => confusable(&e.receiver, jump, false),
            Expr::Path(_) => jump && rightmost_subexpression,
            Expr::Range(e) => {
                (match &e.start {
                    Some(start) => confusable(start, jump, false),
                    None => false,
                } || match &e.end {
                    Some(end) => {
                        matches!(**end, Expr::Block(_))
                            || confusable(end, jump, rightmost_subexpression)
                    }
                    None => false,
                })
            }
            Expr::RawAddr(e) => confusable(&e.expr, jump, rightmost_subexpression),
            Expr::Reference(e) => confusable(&e.expr, jump, rightmost_subexpression),
            Expr::Return(e) => match &e.expr {
                Some(expr) => confusable(expr, true, rightmost_subexpression),
                None => rightmost_subexpression,
            },
            Expr::Struct(_) => !jump,
            Expr::Try(e) => confusable(&e.expr, jump, false),
            Expr::Unary(e) => confusable(&e.expr, jump, rightmost_subexpression),
            Expr::Yield(e) => match &e.expr {
                Some(expr) => confusable(expr, true, rightmost_subexpression),
                None => rightmost_subexpression,
            },

            Expr::Array(_)
            | Expr::Async(_)
            | Expr::Block(_)
            | Expr::Const(_)
            | Expr::Continue(_)
            | Expr::ForLoop(_)
            | Expr::Group(_)
            | Expr::If(_)
            | Expr::Infer(_)
            | Expr::Let(_)
            | Expr::Lit(_)
            | Expr::Loop(_)
            | Expr::Macro(_)
            | Expr::Match(_)
            | Expr::Paren(_)
            | Expr::Repeat(_)
            | Expr::TryBlock(_)
            | Expr::Tuple(_)
            | Expr::Unsafe(_)
            | Expr::Verbatim(_)
            | Expr::While(_) => false,
        }
    }
}

#[cfg(feature = "printing")]
pub(crate) fn trailing_unparameterized_path(mut ty: &Type) -> bool {
    loop {
        match ty {
            Type::BareFn(t) => match &t.output {
                ReturnType::Default => return false,
                ReturnType::Type(_, ret) => ty = ret,
            },
            Type::ImplTrait(t) => match last_type_in_bounds(&t.bounds) {
                ControlFlow::Break(trailing_path) => return trailing_path,
                ControlFlow::Continue(t) => ty = t,
            },
            Type::Path(t) => match last_type_in_path(&t.path) {
                ControlFlow::Break(trailing_path) => return trailing_path,
                ControlFlow::Continue(t) => ty = t,
            },
            Type::Ptr(t) => ty = &t.elem,
            Type::Reference(t) => ty = &t.elem,
            Type::TraitObject(t) => match last_type_in_bounds(&t.bounds) {
                ControlFlow::Break(trailing_path) => return trailing_path,
                ControlFlow::Continue(t) => ty = t,
            },

            Type::Array(_)
            | Type::Group(_)
            | Type::Infer(_)
            | Type::Macro(_)
            | Type::Never(_)
            | Type::Paren(_)
            | Type::Slice(_)
            | Type::Tuple(_)
            | Type::Verbatim(_) => return false,
        }
    }

    fn last_type_in_path(path: &Path) -> ControlFlow<bool, &Type> {
        match &path.segments.last().unwrap().arguments {
            PathArguments::None => ControlFlow::Break(true),
            PathArguments::AngleBracketed(_) => ControlFlow::Break(false),
            PathArguments::Parenthesized(arg) => match &arg.output {
                ReturnType::Default => ControlFlow::Break(false),
                ReturnType::Type(_, ret) => ControlFlow::Continue(ret),
            },
        }
    }

    fn last_type_in_bounds(
        bounds: &Punctuated<TypeParamBound, Token![+]>,
    ) -> ControlFlow<bool, &Type> {
        match bounds.last().unwrap() {
            TypeParamBound::Trait(t) => last_type_in_path(&t.path),
            TypeParamBound::Lifetime(_)
            | TypeParamBound::PreciseCapture(_)
            | TypeParamBound::Verbatim(_) => ControlFlow::Break(false),
        }
    }
}

/// Whether the expression's first token is the label of a loop/block.
#[cfg(all(feature = "printing", feature = "full"))]
pub(crate) fn expr_leading_label(mut expr: &Expr) -> bool {
    loop {
        match expr {
            Expr::Block(e) => return e.label.is_some(),
            Expr::ForLoop(e) => return e.label.is_some(),
            Expr::Loop(e) => return e.label.is_some(),
            Expr::While(e) => return e.label.is_some(),

            Expr::Assign(e) => expr = &e.left,
            Expr::Await(e) => expr = &e.base,
            Expr::Binary(e) => expr = &e.left,
            Expr::Call(e) => expr = &e.func,
            Expr::Cast(e) => expr = &e.expr,
            Expr::Field(e) => expr = &e.base,
            Expr::Index(e) => expr = &e.expr,
            Expr::MethodCall(e) => expr = &e.receiver,
            Expr::Range(e) => match &e.start {
                Some(start) => expr = start,
                None => return false,
            },
            Expr::Try(e) => expr = &e.expr,

            Expr::Array(_)
            | Expr::Async(_)
            | Expr::Break(_)
            | Expr::Closure(_)
            | Expr::Const(_)
            | Expr::Continue(_)
            | Expr::Group(_)
            | Expr::If(_)
            | Expr::Infer(_)
            | Expr::Let(_)
            | Expr::Lit(_)
            | Expr::Macro(_)
            | Expr::Match(_)
            | Expr::Paren(_)
            | Expr::Path(_)
            | Expr::RawAddr(_)
            | Expr::Reference(_)
            | Expr::Repeat(_)
            | Expr::Return(_)
            | Expr::Struct(_)
            | Expr::TryBlock(_)
            | Expr::Tuple(_)
            | Expr::Unary(_)
            | Expr::Unsafe(_)
            | Expr::Verbatim(_)
            | Expr::Yield(_) => return false,
        }
    }
}

/// Whether the expression's last token is `}`.
#[cfg(feature = "full")]
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
            Expr::RawAddr(e) => expr = &e.expr,
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
            TypeParamBound::Lifetime(_) | TypeParamBound::PreciseCapture(_) => {
                ControlFlow::Break(false)
            }
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
