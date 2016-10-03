use super::*;

/// An item
///
/// The name might be a dummy name in case of anonymous items
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Item {
    pub ident: Ident,
    pub vis: Visibility,
    pub attrs: Vec<Attribute>,
    pub node: ItemKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ItemKind {
    /// An`extern crate` item, with optional original crate name.
    ///
    /// E.g. `extern crate foo` or `extern crate foo_bar as foo`
    ExternCrate(Option<Ident>),
    /// A use declaration (`use` or `pub use`) item.
    ///
    /// E.g. `use foo;`, `use foo::bar;` or `use foo::bar as FooBar;`
    Use(Box<ViewPath>),
    /// A static item (`static` or `pub static`).
    ///
    /// E.g. `static FOO: i32 = 42;` or `static FOO: &'static str = "bar";`
    Static(Box<Ty>, Mutability, Box<Expr>),
    /// A constant item (`const` or `pub const`).
    ///
    /// E.g. `const FOO: i32 = 42;`
    Const(Box<Ty>, Box<Expr>),
    /// A function declaration (`fn` or `pub fn`).
    ///
    /// E.g. `fn foo(bar: usize) -> usize { .. }`
    Fn(Box<FnDecl>, Unsafety, Constness, Option<Abi>, Generics, Box<Block>),
    /// A module declaration (`mod` or `pub mod`).
    ///
    /// E.g. `mod foo;` or `mod foo { .. }`
    Mod(Vec<Item>),
    /// An external module (`extern` or `pub extern`).
    ///
    /// E.g. `extern {}` or `extern "C" {}`
    ForeignMod(ForeignMod),
    /// A type alias (`type` or `pub type`).
    ///
    /// E.g. `type Foo = Bar<u8>;`
    Ty(Box<Ty>, Generics),
    /// An enum definition (`enum` or `pub enum`).
    ///
    /// E.g. `enum Foo<A, B> { C<A>, D<B> }`
    Enum(Vec<Variant>, Generics),
    /// A struct definition (`struct` or `pub struct`).
    ///
    /// E.g. `struct Foo<A> { x: A }`
    Struct(VariantData, Generics),
    /// A union definition (`union` or `pub union`).
    ///
    /// E.g. `union Foo<A, B> { x: A, y: B }`
    Union(VariantData, Generics),
    /// A Trait declaration (`trait` or `pub trait`).
    ///
    /// E.g. `trait Foo { .. }` or `trait Foo<T> { .. }`
    Trait(Unsafety, Generics, Vec<TyParamBound>, Vec<TraitItem>),
    // Default trait implementation.
    ///
    /// E.g. `impl Trait for .. {}` or `impl<T> Trait<T> for .. {}`
    DefaultImpl(Unsafety, Path),
    /// An implementation.
    ///
    /// E.g. `impl<A> Foo<A> { .. }` or `impl<A> Trait for Foo<A> { .. }`
    Impl(Unsafety,
             ImplPolarity,
             Generics,
             Option<Path>, // (optional) trait this impl implements
             Box<Ty>, // self
             Vec<ImplItem>),
    /// A macro invocation (which includes macro definition).
    ///
    /// E.g. `macro_rules! foo { .. }` or `foo!(..)`
    Mac(Mac),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ViewPath {
    /// `foo::bar::baz as quux`
    ///
    /// or just
    ///
    /// `foo::bar::baz` (with `as baz` implicitly on the right)
    Simple(Ident, Path),

    /// `foo::bar::*`
    Glob(Path),

    /// `foo::bar::{a, b, c}`
    List(Path, Vec<PathListItem>)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PathListItem {
    pub name: Ident,
    /// renamed in list, e.g. `use foo::{bar as baz};`
    pub rename: Option<Ident>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Unsafety {
    Unsafe,
    Normal,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Constness {
    Const,
    NotConst,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Defaultness {
    Default,
    Final,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Abi(pub String);

/// Foreign module declaration.
///
/// E.g. `extern { .. }` or `extern C { .. }`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ForeignMod {
    pub abi: Abi,
    pub items: Vec<ForeignItem>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ForeignItem {
    pub ident: Ident,
    pub attrs: Vec<Attribute>,
    pub node: ForeignItemKind,
    pub vis: Visibility,
}

/// An item within an `extern` block
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ForeignItemKind {
    /// A foreign function
    Fn(Box<FnDecl>, Generics),
    /// A foreign static item (`static ext: u8`), with optional mutability
    /// (the boolean is true when mutable)
    Static(Box<Ty>, bool),
}

/// Represents an item declaration within a trait declaration,
/// possibly including a default implementation. A trait item is
/// either required (meaning it doesn't have an implementation, just a
/// signature) or provided (meaning it has a default implementation).
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TraitItem {
    pub ident: Ident,
    pub attrs: Vec<Attribute>,
    pub node: TraitItemKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TraitItemKind {
    Const(Ty, Option<Expr>),
    Method(MethodSig, Option<Block>),
    Type(Vec<TyParamBound>, Option<Ty>),
    Macro(Mac),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ImplPolarity {
    /// `impl Trait for Type`
    Positive,
    /// `impl !Trait for Type`
    Negative,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ImplItem {
    pub ident: Ident,
    pub vis: Visibility,
    pub defaultness: Defaultness,
    pub attrs: Vec<Attribute>,
    pub node: ImplItemKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ImplItemKind {
    Const(Ty, Expr),
    Method(MethodSig, Block),
    Type(Ty),
    Macro(Mac),
}

/// Represents a method's signature in a trait declaration,
/// or in an implementation.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MethodSig {
    pub unsafety: Unsafety,
    pub constness: Constness,
    pub abi: Abi,
    pub decl: FnDecl,
    pub generics: Generics,
}

/// Header (not the body) of a function declaration.
///
/// E.g. `fn foo(bar: baz)`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FnDecl {
    pub inputs: Vec<FnArg>,
    pub output: FunctionRetTy,
}

/// An argument in a function header.
///
/// E.g. `bar: usize` as in `fn foo(bar: usize)`
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FnArg {
    pub pat: Pat,
    pub ty: Ty,
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use {FunctionRetTy, Generics, Ident, Mac, TokenTree, Visibility};
    use attr::parsing::outer_attr;
    use data::parsing::visibility;
    use expr::parsing::{block, expr, pat};
    use generics::parsing::{generics, where_clause};
    use ident::parsing::ident;
    use lit::parsing::quoted_string;
    use mac::parsing::delimited;
    use macro_input::{Body, MacroInput};
    use macro_input::parsing::macro_input;
    use ty::parsing::{mutability, ty};

    named!(pub item -> Item, alt!(
        item_extern_crate
        // TODO: Use
        |
        item_static
        |
        item_const
        |
        item_fn
        // TODO: Mod
        // TODO: ForeignMod
        |
        item_ty
        |
        item_struct_or_enum
        // TODO: Union
        // TODO: Trait
        // TODO: DefaultImpl
        // TODO: Impl
        |
        item_mac
    ));

    named!(item_mac -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        path: ident >>
        punct!("!") >>
        name: option!(ident) >>
        body: delimited >>
        (Item {
            ident: name.unwrap_or_else(|| Ident::new("")),
            vis: Visibility::Inherited,
            attrs: attrs,
            node: ItemKind::Mac(Mac {
                path: path.into(),
                tts: vec![TokenTree::Delimited(body)],
            }),
        })
    ));

    named!(item_extern_crate -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("extern") >>
        keyword!("crate") >>
        id: ident >>
        rename: option!(preceded!(
            keyword!("as"),
            ident
        )) >>
        punct!(";") >>
        ({
            let (name, original_name) = match rename {
                Some(rename) => (rename, Some(id)),
                None => (id, None),
            };
            Item {
                ident: name,
                vis: vis,
                attrs: attrs,
                node: ItemKind::ExternCrate(original_name),
            }
        })
    ));

    named!(item_static -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("static") >>
        mutability: mutability >>
        id: ident >>
        punct!(":") >>
        ty: ty >>
        punct!("=") >>
        value: expr >>
        punct!(";") >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemKind::Static(Box::new(ty), mutability, Box::new(value)),
        })
    ));

    named!(item_const -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("const") >>
        id: ident >>
        punct!(":") >>
        ty: ty >>
        punct!("=") >>
        value: expr >>
        punct!(";") >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemKind::Const(Box::new(ty), Box::new(value)),
        })
    ));

    named!(item_fn -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        constness: constness >>
        unsafety: unsafety >>
        abi: option!(preceded!(keyword!("extern"), quoted_string)) >>
        keyword!("fn") >>
        name: ident >>
        generics: generics >>
        punct!("(") >>
        inputs: separated_list!(punct!(","), fn_arg) >>
        punct!(")") >>
        ret: option!(preceded!(punct!("->"), ty)) >>
        where_clause: where_clause >>
        body: block >>
        (Item {
            ident: name,
            vis: vis,
            attrs: attrs,
            node: ItemKind::Fn(
                Box::new(FnDecl {
                    inputs: inputs,
                    output: ret.map(FunctionRetTy::Ty).unwrap_or(FunctionRetTy::Default),
                }),
                unsafety,
                constness,
                abi.map(Abi),
                Generics {
                    where_clause: where_clause,
                    .. generics
                },
                Box::new(body),
            ),
        })
    ));

    named!(fn_arg -> FnArg, do_parse!(
        pat: pat >>
        punct!(":") >>
        ty: ty >>
        (FnArg {
            pat: pat,
            ty: ty,
        })
    ));

    named!(item_ty -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("type") >>
        id: ident >>
        generics: generics >>
        punct!("=") >>
        ty: ty >>
        punct!(";") >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemKind::Ty(Box::new(ty), generics),
        })
    ));

    named!(item_struct_or_enum -> Item, map!(
        macro_input,
        |def: MacroInput| Item {
            ident: def.ident,
            vis: def.vis,
            attrs: def.attrs,
            node: match def.body {
                Body::Enum(variants) => {
                    ItemKind::Enum(variants, def.generics)
                }
                Body::Struct(variant_data) => {
                    ItemKind::Struct(variant_data, def.generics)
                }
            }
        }
    ));

    named!(constness -> Constness, alt!(
        keyword!("const") => { |_| Constness::Const }
        |
        epsilon!() => { |_| Constness::NotConst }
    ));

    named!(unsafety -> Unsafety, alt!(
        keyword!("unsafe") => { |_| Unsafety::Unsafe }
        |
        epsilon!() => { |_| Unsafety::Normal }
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use FunctionRetTy;
    use attr::FilterAttrs;
    use data::VariantData;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Item {
        fn to_tokens(&self, tokens: &mut Tokens) {
            for attr in self.attrs.outer() {
                attr.to_tokens(tokens);
            }
            match self.node {
                ItemKind::ExternCrate(ref original) => {
                    tokens.append("extern");
                    tokens.append("crate");
                    if let Some(ref original) = *original {
                        original.to_tokens(tokens);
                        tokens.append("as");
                    }
                    self.ident.to_tokens(tokens);
                    tokens.append(";");
                }
                ItemKind::Use(ref _view_path) => unimplemented!(),
                ItemKind::Static(ref ty, ref mutability, ref expr) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("static");
                    mutability.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    tokens.append(":");
                    ty.to_tokens(tokens);
                    tokens.append("=");
                    expr.to_tokens(tokens);
                    tokens.append(";");
                }
                ItemKind::Const(ref ty, ref expr) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("const");
                    self.ident.to_tokens(tokens);
                    tokens.append(":");
                    ty.to_tokens(tokens);
                    tokens.append("=");
                    expr.to_tokens(tokens);
                    tokens.append(";");
                }
                ItemKind::Fn(ref decl, unsafety, constness, ref abi, ref generics, ref block) => {
                    self.vis.to_tokens(tokens);
                    constness.to_tokens(tokens);
                    unsafety.to_tokens(tokens);
                    abi.to_tokens(tokens);
                    tokens.append("fn");
                    self.ident.to_tokens(tokens);
                    generics.to_tokens(tokens);
                    tokens.append("(");
                    tokens.append_separated(&decl.inputs, ",");
                    tokens.append(")");
                    if let FunctionRetTy::Ty(ref ty) = decl.output {
                        tokens.append("->");
                        ty.to_tokens(tokens);
                    }
                    generics.where_clause.to_tokens(tokens);
                    block.to_tokens(tokens);
                }
                ItemKind::Mod(ref _items) => unimplemented!(),
                ItemKind::ForeignMod(ref _foreign_mod) => unimplemented!(),
                ItemKind::Ty(ref ty, ref generics) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("type");
                    self.ident.to_tokens(tokens);
                    generics.to_tokens(tokens);
                    tokens.append("=");
                    ty.to_tokens(tokens);
                    tokens.append(";");
                }
                ItemKind::Enum(ref variants, ref generics) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("enum");
                    self.ident.to_tokens(tokens);
                    generics.to_tokens(tokens);
                    generics.where_clause.to_tokens(tokens);
                    tokens.append("{");
                    for variant in variants {
                        variant.to_tokens(tokens);
                        tokens.append(",");
                    }
                    tokens.append("}");
                }
                ItemKind::Struct(ref variant_data, ref generics) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("struct");
                    self.ident.to_tokens(tokens);
                    generics.to_tokens(tokens);
                    generics.where_clause.to_tokens(tokens);
                    variant_data.to_tokens(tokens);
                    match *variant_data {
                        VariantData::Struct(_) => { /* no semicolon */ }
                        VariantData::Tuple(_) |
                        VariantData::Unit => tokens.append(";"),
                    }
                }
                ItemKind::Union(ref _variant_data, ref _generics) => unimplemented!(),
                ItemKind::Trait(_unsafety, ref _generics, ref _bound, ref _item) => unimplemented!(),
                ItemKind::DefaultImpl(_unsafety, ref _path) => unimplemented!(),
                ItemKind::Impl(_unsafety, _polarity, ref _generics, ref _path, ref _ty, ref _item) => unimplemented!(),
                ItemKind::Mac(ref _mac) => unimplemented!(),
            }
        }
    }

    impl ToTokens for FnArg {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.pat.to_tokens(tokens);
            tokens.append(":");
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for Unsafety {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Unsafety::Unsafe => tokens.append("unsafe"),
                Unsafety::Normal => { /* nothing */ },
            }
        }
    }

    impl ToTokens for Constness {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Constness::Const => tokens.append("const"),
                Constness::NotConst => { /* nothing */ },
            }
        }
    }

    impl ToTokens for Abi {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("extern");
            self.0.to_tokens(tokens);
        }
    }
}
