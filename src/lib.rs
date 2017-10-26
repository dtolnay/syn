#![doc(html_root_url = "https://dtolnay.github.io/syn")]

#![cfg_attr(feature = "cargo-clippy", allow(large_enum_variant))]

extern crate proc_macro;
extern crate proc_macro2;
extern crate unicode_xid;

#[cfg(any(feature = "printing", feature = "parsing"))]
extern crate quote;

#[cfg_attr(feature = "parsing", macro_use)]
extern crate synom;

#[macro_use]
mod macros;

mod attr;
pub use attr::{Attribute, AttrStyle, MetaItem, NestedMetaItem, MetaItemList,
               MetaNameValue};

mod data;
pub use data::{Field, Variant, VariantData, Visibility, VisRestricted, VisCrate,
               VisPublic, VisInherited};

mod expr;
pub use expr::{Expr, ExprKind, ExprBox, ExprInPlace, ExprArray, ExprCall,
               ExprMethodCall, ExprTup, ExprBinary, ExprUnary, ExprCast,
               ExprType, ExprIf, ExprIfLet, ExprWhile, ExprWhileLet,
               ExprForLoop, ExprLoop, ExprMatch, ExprClosure, ExprBlock,
               ExprAssign, ExprAssignOp, ExprField, ExprTupField, ExprIndex,
               ExprRange, ExprPath, ExprAddrOf, ExprBreak, ExprContinue,
               ExprRet, ExprStruct, ExprRepeat, ExprParen, ExprTry, ExprCatch,
               ExprGroup, ExprYield};

#[cfg(feature = "full")]
pub use expr::{Arm, BindingMode, Block, CaptureBy, FieldPat, FieldValue, Local,
               MacStmtStyle, Pat, RangeLimits, Stmt, PatIdent, PatWild,
               PatStruct, PatTuple, PatTupleStruct, PatPath, PatBox, PatRef,
               PatLit, PatRange, PatSlice, InPlaceKind};

mod generics;
pub use generics::{Generics, LifetimeDef, TraitBoundModifier, TyParam, TyParamBound,
                   WhereBoundPredicate, WhereClause, WhereEqPredicate, WherePredicate,
                   WhereRegionPredicate, BoundLifetimes};
#[cfg(feature = "printing")]
pub use generics::{ImplGenerics, Turbofish, TyGenerics};

mod ident;
pub use ident::Ident;

#[cfg(feature = "full")]
mod item;
#[cfg(feature = "full")]
pub use item::{Constness, Defaultness, FnArg, FnDecl, ForeignItemKind, ForeignItem, ItemForeignMod,
               ImplItem, ImplItemKind, ImplPolarity, Item, ItemKind, MethodSig, PathListItem,
               TraitItem, TraitItemKind, ViewPath, ItemExternCrate, ItemUse,
               ItemStatic, ItemConst, ItemFn, ItemMod, ItemTy, ItemEnum,
               ItemStruct, ItemUnion, ItemTrait, ItemDefaultImpl, ItemImpl,
               PathSimple, PathGlob, PathList, ForeignItemFn, ForeignItemStatic,
               TraitItemConst, TraitItemMethod, TraitItemType,
               ImplItemConst, ImplItemMethod, ImplItemType, ArgSelfRef,
               ArgSelf, ArgCaptured};

#[cfg(feature = "full")]
mod file;
#[cfg(feature = "full")]
pub use file::File;

mod lifetime;
pub use lifetime::Lifetime;

mod lit;
pub use lit::{Lit, LitKind};

mod mac;
pub use mac::{Mac, TokenTree};

mod derive;
pub use derive::{Body, DeriveInput, BodyEnum, BodyStruct};

mod op;
pub use op::{BinOp, UnOp};

mod ty;
pub use ty::{Abi, AbiKind, AngleBracketedParameterData, BareFnArg, BareFnArgName, BareFnTy,
             FunctionRetTy, MutTy, Mutability, ParenthesizedParameterData, Path,
             PathParameters, PathSegment, PolyTraitRef, QSelf, Ty, TypeBinding, Unsafety,
             TySlice, TyArray, TyPtr, TyRptr, TyBareFn, TyNever, TyTup, TyPath,
             TyTraitObject, TyImplTrait, TyParen, TyInfer, TyGroup};
#[cfg(feature = "printing")]
pub use ty::PathTokens;

pub use synom::span::Span;
pub use synom::tokens;
pub use synom::delimited;

mod gen {
    #[cfg(feature = "visit")]
    pub mod visit;

    #[cfg(feature = "visit_mut")]
    pub mod visit_mut;

    #[cfg(feature = "fold")]
    pub mod fold;
}
pub use gen::*;

////////////////////////////////////////////////////////////////////////////////

#[cfg(feature = "parsing")]
pub use synom::ParseError;

#[cfg(feature = "parsing")]
use synom::{Synom, SynomBuffer};

/// Parse tokens of source code into the chosen syn data type.
///
/// This is preferred over parsing a string because tokens are able to preserve
/// information about where in the user's code they were originally written (the
/// "span" of the token), possibly allowing the compiler to produce better error
/// messages.
///
/// # Examples
///
/// ```rust,ignore
/// extern crate proc_macro;
/// use proc_macro::TokenStream;
///
/// extern crate syn;
///
/// #[macro_use]
/// extern crate quote;
///
/// use syn::DeriveInput;
///
/// #[proc_macro_derive(MyMacro)]
/// pub fn my_macro(input: TokenStream) -> TokenStream {
///     // Parse the tokens into a syntax tree
///     let ast: DeriveInput = syn::parse(input).unwrap();
///
///     // Build the output, possibly using quasi-quotation
///     let expanded = quote! {
///         /* ... */
///     };
///
///     // Parse back to a token stream and return it
///     expanded.parse().unwrap()
/// }
/// ```
#[cfg(feature = "parsing")]
pub fn parse<T>(tokens: proc_macro::TokenStream) -> Result<T, ParseError>
    where T: Synom,
{
    _parse(tokens.into())
}

#[cfg(feature = "parsing")]
fn _parse<T>(tokens: proc_macro2::TokenStream) -> Result<T, ParseError>
    where T: Synom,
{
    let buf = SynomBuffer::new(tokens);
    let result = T::parse(buf.begin());
    let err = match result {
        Ok((rest, t)) => {
            if rest.eof() {
                return Ok(t);
            } else if rest == buf.begin() {
                // parsed nothing
                ParseError::new("failed to parse anything")
            } else {
                ParseError::new("failed to parse all tokens")
            }
        }
        Err(err) => err,
    };
    match T::description() {
        Some(s) => Err(ParseError::new(format!("failed to parse {}: {}", s, err))),
        None => Err(err),
    }
}

/// Parse a `quote::Tokens` of Rust code into the chosen syn data type.
///
/// # Examples
///
/// ```rust
/// extern crate syn;
/// #
/// # #[macro_use]
/// # extern crate error_chain;
/// # #[macro_use]
/// # extern crate quote;
///
/// use syn::Expr;
/// #
/// # error_chain! {
/// #     foreign_links {
/// #         Syn(syn::ParseError);
/// #     }
/// # }
///
/// fn run() -> Result<()> {
///     let code = quote!(assert_eq!(u8::max_value(), 255));
///     let expr = syn::parse_tokens::<Expr>(code)?;
///     println!("{:#?}", expr);
///     Ok(())
/// }
/// #
/// # fn main() { run().unwrap() }
/// ```
#[cfg(feature = "parsing")]
pub fn parse_tokens<T: Synom>(tokens: quote::Tokens) -> Result<T, ParseError> {
    _parse(tokens.into())
}

/// Parse a string of Rust code into the chosen syn data type.
///
/// # Examples
///
/// ```rust
/// extern crate syn;
/// #
/// # #[macro_use]
/// # extern crate error_chain;
///
/// use syn::Expr;
/// #
/// # error_chain! {
/// #     foreign_links {
/// #         Syn(syn::ParseError);
/// #     }
/// # }
///
/// fn run() -> Result<()> {
///     let code = "assert_eq!(u8::max_value(), 255)";
///     let expr = syn::parse_str::<Expr>(code)?;
///     println!("{:#?}", expr);
///     Ok(())
/// }
/// #
/// # fn main() { run().unwrap() }
/// ```
#[cfg(feature = "parsing")]
pub fn parse_str<T: Synom>(s: &str) -> Result<T, ParseError> {
    _parse(s.parse()?)
}

// FIXME the name parse_file makes it sound like you might pass in a path to a
// file, rather than the content.
/// Parse the content of a file of Rust code.
///
/// This is different from `syn::parse_str::<File>(content)` in two ways:
///
/// - It discards a leading byte order mark `\u{FEFF}` if the file has one.
/// - It preserves the shebang line of the file, such as `#!/usr/bin/env rustx`.
///
/// If present, either of these would be an error using `from_str`.
///
/// # Examples
///
/// ```rust,no_run
/// extern crate syn;
/// #
/// # #[macro_use]
/// # extern crate error_chain;
///
/// use std::fs::File;
/// use std::io::Read;
/// #
/// # error_chain! {
/// #     foreign_links {
/// #         Io(std::io::Error);
/// #         Syn(syn::ParseError);
/// #     }
/// # }
///
/// fn run() -> Result<()> {
///     let mut file = File::open("path/to/code.rs")?;
///     let mut content = String::new();
///     file.read_to_string(&mut content)?;
///
///     let ast = syn::parse_file(&content)?;
///     if let Some(shebang) = ast.shebang {
///         println!("{}", shebang);
///     }
///     println!("{} items", ast.items.len());
///
///     Ok(())
/// }
/// #
/// # fn main() { run().unwrap() }
/// ```
#[cfg(all(feature = "parsing", feature = "full"))]
pub fn parse_file(mut content: &str) -> Result<File, ParseError> {
    // Strip the BOM if it is present
    const BOM: &'static str = "\u{feff}";
    if content.starts_with(BOM) {
        content = &content[BOM.len()..];
    }

    let mut shebang = None;
    if content.starts_with("#!") && !content.starts_with("#![") {
        if let Some(idx) = content.find('\n') {
            shebang = Some(content[..idx].to_string());
            content = &content[idx..];
        } else {
            shebang = Some(content.to_string());
            content = "";
        }
    }

    let mut file: File = parse_str(content)?;
    file.shebang = shebang;
    Ok(file)
}

#[cfg(feature = "printing")]
struct TokensOrDefault<'a, T: 'a>(&'a Option<T>);

#[cfg(feature = "printing")]
impl<'a, T> quote::ToTokens for TokensOrDefault<'a, T>
    where T: quote::ToTokens + Default,
{
    fn to_tokens(&self, tokens: &mut quote::Tokens) {
        match *self.0 {
            Some(ref t) => t.to_tokens(tokens),
            None => T::default().to_tokens(tokens),
        }
    }
}
