#![feature(proc_macro)]

extern crate proc_macro;

#[macro_use]
extern crate syn;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use quote::ToTokens;
use std::collections::HashSet as Set;
use syn::fold::{self, Fold};
use syn::punctuated::Punctuated;
use syn::synom::Synom;
use syn::{Expr, Ident, ItemFn, Local, Pat, Stmt};

/// Parses a parenthesized nonempty list of variable names separated by commas.
///
///     (a, b, c)
///
/// This is how the compiler passes in arguments to our attribute -- it is
/// everything that comes after the attribute name.
///
///     #[trace_var(a, b, c)]
///                ^^^^^^^^^
struct Args {
    vars: Set<Ident>,
}

impl Synom for Args {
    named!(parse -> Self, map!(
        call!(Punctuated::<Ident, Token![,]>::parse_terminated_nonempty),
        |vars| Args {
            vars: vars.into_iter().collect(),
        }
    ));
}

impl Args {
    /// Determines whether the given `Expr` is a path referring to one of the
    /// variables we intend to print. Expressions are used as the left-hand side
    /// of the assignment operator.
    fn should_print_expr(&self, e: &Expr) -> bool {
        match *e {
            Expr::Path(ref e) => {
                if e.path.leading_colon.is_some() {
                    false
                } else if e.path.segments.len() != 1 {
                    false
                } else {
                    let first = e.path.segments.first().unwrap();
                    let segment = first.value();
                    self.vars.contains(&segment.ident) && segment.arguments.is_empty()
                }
            }
            _ => false,
        }
    }

    /// Determines whether the given `Pat` is an identifier equal to one of the
    /// variables we intend to print. Patterns are used as the left-hand side of
    /// a `let` binding.
    fn should_print_pat(&self, p: &Punctuated<Pat, Token![|]>) -> bool {
        if p.len() != 1 {
            return false;
        }
        match p[0] {
            Pat::Ident(ref p) => self.vars.contains(&p.ident),
            _ => false,
        }
    }

    /// Produces an expression that assigns the right-hand side to the left-hand
    /// side and then prints the value.
    ///
    ///     // Before
    ///     VAR = INIT
    ///
    ///     // After
    ///     { VAR = INIT; println!("VAR = {:?}", VAR); }
    fn assign_and_print(&mut self, left: Expr, op: &ToTokens, right: Expr) -> Expr {
        let right = fold::fold_expr(self, right);
        parse_quote!({
            #left #op #right;
            println!(concat!(stringify!(#left), " = {:?}"), #left);
        })
    }

    /// Produces a let-binding that assigns the right-hand side to the left-hand
    /// side and then prints the value.
    ///
    ///     // Before
    ///     let VAR = INIT;
    ///
    ///     // After
    ///     let VAR = { let VAR = INIT; println!("VAR = {:?}", VAR); VAR };
    fn let_and_print(&mut self, local: Local) -> Stmt {
        let Local { pats, ty, init, .. } = local;
        let pat = &pats[0];
        let ty = ty.map(|(colon_token, ty)| quote!(#colon_token #ty));
        let init = self.fold_expr(*init.unwrap().1);
        let ident = match *pat {
            Pat::Ident(ref p) => &p.ident,
            _ => unreachable!(),
        };
        parse_quote! {
            let #pat #ty = {
                let #pat = #init;
                println!(concat!(stringify!(#ident), " = {:?}"), #ident);
                #ident
            };
        }
    }
}

/// The `Fold` trait is a way to traverse an owned syntax tree and replace some
/// of its nodes.
///
/// Syn provides two other syntax tree traversal traits: `Visit` which walks a
/// shared borrow of a syntax tree, and `VisitMut` which walks an exclusive
/// borrow of a syntax tree and can mutate it in place.
///
/// All three traits have a method corresponding to each type of node in Syn's
/// syntax tree. All of these methods have default no-op implementations that
/// simply recurse on any child nodes. We can override only those methods for
/// which we want non-default behavior. In this case the traversal needs to
/// transform `Expr` and `Stmt` nodes.
impl Fold for Args {
    fn fold_expr(&mut self, e: Expr) -> Expr {
        match e {
            Expr::Assign(e) => {
                if self.should_print_expr(&e.left) {
                    self.assign_and_print(*e.left, &e.eq_token, *e.right)
                } else {
                    Expr::Assign(fold::fold_expr_assign(self, e))
                }
            }
            Expr::AssignOp(e) => {
                if self.should_print_expr(&e.left) {
                    self.assign_and_print(*e.left, &e.op, *e.right)
                } else {
                    Expr::AssignOp(fold::fold_expr_assign_op(self, e))
                }
            }
            _ => fold::fold_expr(self, e),
        }
    }

    fn fold_stmt(&mut self, s: Stmt) -> Stmt {
        match s {
            Stmt::Local(s) => {
                if s.init.is_some() && self.should_print_pat(&s.pats) {
                    self.let_and_print(s)
                } else {
                    Stmt::Local(fold::fold_local(self, s))
                }
            }
            _ => fold::fold_stmt(self, s),
        }
    }
}

/// Attribute to print the value of the given variables each time they are
/// reassigned.
///
/// # Example
///
/// ```
/// #[trace_var(p, n)]
/// fn factorial(mut n: u64) -> u64 {
///     let mut p = 1;
///     while n > 1 {
///         p *= n;
///         n -= 1;
///     }
///     p
/// }
/// ```
#[proc_macro_attribute]
pub fn trace_var(args: TokenStream, input: TokenStream) -> TokenStream {
    // Return the input unchanged if it failed to parse. The compiler will show
    // the right diagnostics.
    let input: ItemFn = match syn::parse(input.clone()) {
        Ok(input) => input,
        Err(_) => return input,
    };

    // Parse the list of variables the user wanted to print.
    let mut args: Args = syn::parse(args).unwrap();

    // Use a syntax tree traversal to transform the function body.
    let output = args.fold_item_fn(input);

    // Hand the resulting function body back to the compiler.
    quote!(#output).into()
}
