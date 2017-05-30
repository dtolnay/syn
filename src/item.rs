use super::*;
use delimited::Delimited;

ast_struct! {
    /// An item
    ///
    /// The name might be a dummy name in case of anonymous items
    pub struct Item {
        pub ident: Ident,
        pub vis: Visibility,
        pub attrs: Vec<Attribute>,
        pub node: ItemKind,
    }
}

ast_enum_of_structs! {
    pub enum ItemKind {
        /// An`extern crate` item, with optional original crate name.
        ///
        /// E.g. `extern crate foo` or `extern crate foo_bar as foo`
        pub ExternCrate(ItemExternCrate {
            pub extern_token: tokens::Extern,
            pub crate_token: tokens::Crate,
            pub as_token: Option<tokens::As>,
            pub original: Option<Ident>,
            pub semi_token: tokens::Semi,
        }),
        /// A use declaration (`use` or `pub use`) item.
        ///
        /// E.g. `use foo;`, `use foo::bar;` or `use foo::bar as FooBar;`
        pub Use(ItemUse {
            pub use_token: tokens::Use,
            pub path: Box<ViewPath>,
            pub semi_token: tokens::Semi,
        }),
        /// A static item (`static` or `pub static`).
        ///
        /// E.g. `static FOO: i32 = 42;` or `static FOO: &'static str = "bar";`
        pub Static(ItemStatic {
            pub static_token: tokens::Static,
            pub colon_token: tokens::Colon,
            pub eq_token: tokens::Eq,
            pub semi_token: tokens::Semi,
            pub ty: Box<Ty>,
            pub mutbl: Mutability,
            pub expr: Box<Expr>,
        }),
        /// A constant item (`const` or `pub const`).
        ///
        /// E.g. `const FOO: i32 = 42;`
        pub Const(ItemConst {
            pub const_token: tokens::Const,
            pub colon_token: tokens::Colon,
            pub eq_token: tokens::Eq,
            pub semi_token: tokens::Semi,
            pub ty: Box<Ty>,
            pub expr: Box<Expr>,
        }),
        /// A function declaration (`fn` or `pub fn`).
        ///
        /// E.g. `fn foo(bar: usize) -> usize { .. }`
        pub Fn(ItemFn {
            pub decl: Box<FnDecl>,
            pub unsafety: Unsafety,
            pub constness: Constness,
            pub abi: Option<Abi>,
            pub block: Box<Block>,
        }),
        /// A module declaration (`mod` or `pub mod`).
        ///
        /// E.g. `mod foo;` or `mod foo { .. }`
        pub Mod(ItemMod {
            pub mod_token: tokens::Mod,
            pub semi_token: Option<tokens::Semi>,
            pub items: Option<(Vec<Item>, tokens::Brace)>,
        }),
        /// An external module (`extern` or `pub extern`).
        ///
        /// E.g. `extern {}` or `extern "C" {}`
        pub ForeignMod(ItemForeignMod {
            pub brace_token: tokens::Brace,
            pub abi: Abi,
            pub items: Vec<ForeignItem>,
        }),

        /// A type alias (`type` or `pub type`).
        ///
        /// E.g. `type Foo = Bar<u8>;`
        pub Ty(ItemTy {
            pub type_token: tokens::Type,
            pub eq_token: tokens::Eq,
            pub semi_token: tokens::Semi,
            pub ty: Box<Ty>,
            pub generics: Generics,
        }),
        /// An enum definition (`enum` or `pub enum`).
        ///
        /// E.g. `enum Foo<A, B> { C<A>, D<B> }`
        pub Enum(ItemEnum {
            pub enum_token: tokens::Enum,
            pub brace_token: tokens::Brace,
            pub variants: Delimited<Variant, tokens::Comma>,
            pub generics: Generics,
        }),
        /// A struct definition (`struct` or `pub struct`).
        ///
        /// E.g. `struct Foo<A> { x: A }`
        pub Struct(ItemStruct {
            pub struct_token: tokens::Struct,
            pub data: VariantData,
            pub generics: Generics,
            pub semi_token: Option<tokens::Semi>,
        }),
        /// A union definition (`union` or `pub union`).
        ///
        /// E.g. `union Foo<A, B> { x: A, y: B }`
        pub Union(ItemUnion {
            pub union_token: tokens::Union,
            pub data: VariantData,
            pub generics: Generics,
        }),
        /// A Trait declaration (`trait` or `pub trait`).
        ///
        /// E.g. `trait Foo { .. }` or `trait Foo<T> { .. }`
        pub Trait(ItemTrait {
            pub trait_token: tokens::Trait,
            pub unsafety: Unsafety,
            pub generics: Generics,
            pub colon_token: Option<tokens::Colon>,
            pub supertraits: Delimited<TyParamBound, tokens::Add>,
            pub brace_token: tokens::Brace,
            pub items: Vec<TraitItem>,
        }),
        /// Default trait implementation.
        ///
        /// E.g. `impl Trait for .. {}` or `impl<T> Trait<T> for .. {}`
        pub DefaultImpl(ItemDefaultImpl {
            pub unsafety: Unsafety,
            pub path: Path,
            pub impl_token: tokens::Impl,
            pub for_token: tokens::For,
            pub dot2_token: tokens::Dot2,
            pub brace_token: tokens::Brace,
        }),
        /// An implementation.
        ///
        /// E.g. `impl<A> Foo<A> { .. }` or `impl<A> Trait for Foo<A> { .. }`
        pub Impl(ItemImpl {
            pub impl_token: tokens::Impl,
            pub unsafety: Unsafety,
            pub polarity: ImplPolarity,
            pub generics: Generics,
            pub for_token: Option<tokens::For>,
            pub trait_: Option<Path>, // (optional) trait this impl implements
            pub self_ty: Box<Ty>, // self
            pub brace_token: tokens::Brace,
            pub items: Vec<ImplItem>,
        }),
        /// A macro invocation (which includes macro definition).
        ///
        /// E.g. `macro_rules! foo { .. }` or `foo!(..)`
        pub Mac(Mac),
    }

    do_not_generate_to_tokens
}

impl From<DeriveInput> for Item {
    fn from(input: DeriveInput) -> Item {
        Item {
            ident: input.ident,
            vis: input.vis,
            attrs: input.attrs,
            node: match input.body {
                Body::Enum(data) => {
                    ItemEnum {
                        variants: data.variants,
                        generics: input.generics,
                        brace_token: data.brace_token,
                        enum_token: data.enum_token,
                    }.into()
                }
                Body::Struct(data) => {
                    ItemStruct {
                        data: data.data,
                        generics: input.generics,
                        semi_token: data.semi_token,
                        struct_token: data.struct_token,
                    }.into()
                }
            },
        }
    }
}

ast_enum_of_structs! {
    pub enum ViewPath {
        /// `foo::bar::baz as quux`
        ///
        /// or just
        ///
        /// `foo::bar::baz` (with `as baz` implicitly on the right)
        pub Simple(PathSimple {
            pub path: Path,
            pub as_token: Option<tokens::As>,
            pub rename: Option<Ident>,
        }),

        /// `foo::bar::*`
        pub Glob(PathGlob {
            pub path: Path,
            pub colon2_token: tokens::Colon2,
            pub star_token: tokens::Star,
        }),

        /// `foo::bar::{a, b, c}`
        pub List(PathList {
            pub path: Path,
            pub colon2_token: tokens::Colon2,
            pub brace_token: tokens::Brace,
            pub items: Delimited<PathListItem, tokens::Comma>,
        }),
    }
}

ast_struct! {
    pub struct PathListItem {
        pub name: Ident,
        /// renamed in list, e.g. `use foo::{bar as baz};`
        pub rename: Option<Ident>,
        pub as_token: Option<tokens::As>,
    }
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum Constness {
        Const(tokens::Const),
        NotConst,
    }
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum Defaultness {
        Default(tokens::Default),
        Final,
    }
}

ast_struct! {
    pub struct ForeignItem {
        pub ident: Ident,
        pub attrs: Vec<Attribute>,
        pub node: ForeignItemKind,
        pub vis: Visibility,
        pub semi_token: tokens::Semi,
    }
}

ast_enum_of_structs! {
    /// An item within an `extern` block
    pub enum ForeignItemKind {
        /// A foreign function
        pub Fn(ForeignItemFn {
            pub decl: Box<FnDecl>,
        }),
        /// A foreign static item (`static ext: u8`)
        pub Static(ForeignItemStatic {
            pub static_token: tokens::Static,
            pub ty: Box<Ty>,
            pub colon_token: tokens::Colon,
            pub mutbl: Mutability,
        }),
    }

    do_not_generate_to_tokens
}

ast_struct! {
    /// Represents an item declaration within a trait declaration,
    /// possibly including a default implementation. A trait item is
    /// either required (meaning it doesn't have an implementation, just a
    /// signature) or provided (meaning it has a default implementation).
    pub struct TraitItem {
        pub ident: Ident,
        pub attrs: Vec<Attribute>,
        pub node: TraitItemKind,
    }
}

ast_enum_of_structs! {
    pub enum TraitItemKind {
        pub Const(TraitItemConst {
            pub const_token: tokens::Const,
            pub colon_token: tokens::Colon,
            pub ty: Ty,
            pub default: Option<Expr>,
            pub eq_token: Option<tokens::Eq>,
            pub semi_token: tokens::Semi,
        }),
        pub Method(TraitItemMethod {
            pub sig: MethodSig,
            pub default: Option<Block>,
            pub semi_token: Option<tokens::Semi>,
        }),
        pub Type(TraitItemType {
            pub type_token: tokens::Type,
            pub colon_token: Option<tokens::Colon>,
            pub bounds: Delimited<TyParamBound, tokens::Add>,
            pub eq_token: Option<tokens::Eq>,
            pub default: Option<Ty>,
            pub semi_token: tokens::Semi,
        }),
        pub Macro(Mac),
    }

    do_not_generate_to_tokens
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum ImplPolarity {
        /// `impl Trait for Type`
        Positive,
        /// `impl !Trait for Type`
        Negative(tokens::Bang),
    }
}

ast_struct! {
    pub struct ImplItem {
        pub ident: Ident,
        pub vis: Visibility,
        pub defaultness: Defaultness,
        pub attrs: Vec<Attribute>,
        pub node: ImplItemKind,
    }
}

ast_enum_of_structs! {
    pub enum ImplItemKind {
        pub Const(ImplItemConst {
            pub const_token: tokens::Const,
            pub colon_token: tokens::Colon,
            pub eq_token: tokens::Eq,
            pub semi_token: tokens::Semi,
            pub ty: Ty,
            pub expr: Expr,
        }),
        pub Method(ImplItemMethod {
            pub sig: MethodSig,
            pub block: Block,
        }),
        pub Type(ImplItemType {
            pub type_token: tokens::Type,
            pub eq_token: tokens::Eq,
            pub semi_token: tokens::Semi,
            pub ty: Ty,
        }),
        pub Macro(Mac),
    }

    do_not_generate_to_tokens
}

ast_struct! {
    /// Represents a method's signature in a trait declaration,
    /// or in an implementation.
    pub struct MethodSig {
        pub unsafety: Unsafety,
        pub constness: Constness,
        pub abi: Option<Abi>,
        pub decl: FnDecl,
    }
}

ast_struct! {
    /// Header (not the body) of a function declaration.
    ///
    /// E.g. `fn foo(bar: baz)`
    pub struct FnDecl {
        pub fn_token: tokens::Fn,
        pub paren_token: tokens::Paren,
        pub inputs: Delimited<FnArg, tokens::Comma>,
        pub output: FunctionRetTy,
        pub generics: Generics,
        pub variadic: bool,
        pub dot_tokens: Option<tokens::Dot3>,
    }
}

ast_enum_of_structs! {
    /// An argument in a function header.
    ///
    /// E.g. `bar: usize` as in `fn foo(bar: usize)`
    pub enum FnArg {
        pub SelfRef(ArgSelfRef {
            pub and_token: tokens::And,
            pub self_token: tokens::Self_,
            pub lifetime: Option<Lifetime>,
            pub mutbl: Mutability,
        }),
        pub SelfValue(ArgSelf {
            pub mutbl: Mutability,
            pub self_token: tokens::Self_,
        }),
        pub Captured(ArgCaptured {
            pub pat: Pat,
            pub colon_token: tokens::Colon,
            pub ty: Ty,
        }),
        pub Ignored(Ty),
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use {Block, Generics, Ident, Mac, Path, VariantData};
    use attr::parsing::{inner_attr, outer_attr};
    use data::parsing::{struct_like_body, visibility};
    use expr::parsing::{expr, pat, within_block};
    use generics::parsing::{generics, lifetime, ty_param_bound, where_clause};
    use ident::parsing::ident;
    use mac::parsing::delimited;
    use derive::{Body, DeriveInput};
    use derive::parsing::derive_input;
    use ty::parsing::{abi, mutability, path, ty, unsafety, fn_ret_ty};

    named!(pub item -> Item, alt!(
        item_extern_crate
        |
        item_use
        |
        item_static
        |
        item_const
        |
        item_fn
        |
        item_mod
        |
        item_foreign_mod
        |
        item_ty
        |
        item_struct_or_enum
        |
        item_union
        |
        item_trait
        |
        item_default_impl
        |
        item_impl
        |
        item_mac
    ));

    named!(pub items -> Vec<Item>, many0!(item));

    named!(item_mac -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        what: path >>
        punct!("!") >>
        name: option!(ident) >>
        body: delimited >>
        cond!(!body.is_braced(), punct!(";")) >>
        (Item {
            ident: name.unwrap_or_else(|| Ident::from("")),
            vis: VisInherited {}.into(),
            attrs: attrs,
            node: ItemKind::Mac(Mac {
                bang_token: tokens::Bang::default(),
                path: what,
                tokens: vec![body],
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
                node: ItemExternCrate {
                    as_token: original_name.as_ref().map(|_| tokens::As::default()),
                    original: original_name,
                    extern_token: tokens::Extern::default(),
                    crate_token: tokens::Crate::default(),
                    semi_token: tokens::Semi::default(),
                }.into(),
            }
        })
    ));

    named!(item_use -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("use") >>
        what: view_path >>
        punct!(";") >>
        (Item {
            ident: "".into(),
            vis: vis,
            attrs: attrs,
            node: ItemUse {
                path: Box::new(what),
                use_token: tokens::Use::default(),
                semi_token: tokens::Semi::default(),
            }.into(),
        })
    ));

    named!(view_path -> ViewPath, alt!(
        view_path_glob
        |
        view_path_list
        |
        view_path_list_root
        |
        view_path_simple // must be last
    ));


    named!(view_path_simple -> ViewPath, do_parse!(
        path: path >>
        rename: option!(preceded!(keyword!("as"), ident)) >>
        (PathSimple {
            path: path,
            as_token: rename.as_ref().map(|_| tokens::As::default()),
            rename: rename,
        }.into())
    ));

    named!(view_path_glob -> ViewPath, do_parse!(
        path: path >>
        punct!("::") >>
        punct!("*") >>
        (PathGlob {
            path: path,
            colon2_token: tokens::Colon2::default(),
            star_token: tokens::Star::default(),
        }.into())
    ));

    named!(view_path_list -> ViewPath, do_parse!(
        path: path >>
        punct!("::") >>
        punct!("{") >>
        items: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                path_list_item) >>
        punct!("}") >>
        (PathList {
            path: path,
            items: items,
            brace_token: tokens::Brace::default(),
            colon2_token: tokens::Colon2::default(),
        }.into())
    ));

    named!(view_path_list_root -> ViewPath, do_parse!(
        global: option!(punct!("::")) >>
        punct!("{") >>
        items: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                path_list_item) >>
        punct!("}") >>
        (PathList {
            path: Path {
                global: global.is_some(),
                segments: Delimited::new(),
                leading_colon: None,
            },
            colon2_token: tokens::Colon2::default(),
            brace_token: tokens::Brace::default(),
            items: items,
        }.into())
    ));

    named!(path_list_item -> PathListItem, do_parse!(
        name: alt!(
            ident
            |
            map!(keyword!("self"), Into::into)
        ) >>
        rename: option!(preceded!(keyword!("as"), ident)) >>
        (PathListItem {
            name: name,
            as_token: rename.as_ref().map(|_| tokens::As::default()),
            rename: rename,
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
            node: ItemStatic {
                ty: Box::new(ty),
                mutbl: mutability,
                expr: Box::new(value),
                static_token: tokens::Static::default(),
                colon_token: tokens::Colon::default(),
                eq_token: tokens::Eq::default(),
                semi_token: tokens::Semi::default(),
            }.into(),
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
            node: ItemConst {
                ty: Box::new(ty),
                expr: Box::new(value),
                const_token: tokens::Const::default(),
                colon_token: tokens::Colon::default(),
                eq_token: tokens::Eq::default(),
                semi_token: tokens::Semi::default(),
            }.into(),
        })
    ));

    named!(item_fn -> Item, do_parse!(
        outer_attrs: many0!(outer_attr) >>
        vis: visibility >>
        constness: constness >>
        unsafety: unsafety >>
        abi: option!(abi) >>
        keyword!("fn") >>
        name: ident >>
        generics: generics >>
        punct!("(") >>
        inputs: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 fn_arg) >>
        punct!(")") >>
        ret: fn_ret_ty >>
        where_clause: where_clause >>
        inner_attrs_stmts: delim!(Brace, tuple!(
            many0!(inner_attr), within_block
        )) >>
        (Item {
            ident: name,
            vis: vis,
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend(inner_attrs_stmts.0);
                attrs
            },
            node: ItemFn {
                decl: Box::new(FnDecl {
                    dot_tokens: None,
                    fn_token: tokens::Fn::default(),
                    paren_token: tokens::Paren::default(),
                    inputs: inputs,
                    output: ret,
                    variadic: false,
                    generics: Generics {
                        where_clause: where_clause,
                        .. generics
                    },
                }),
                unsafety: unsafety,
                constness: constness,
                abi: abi,
                block: Box::new(Block {
                    stmts: stmts,
                    brace_token: tokens::Brace::default(),
                }),
            }.into(),
        })
    ));

    named!(fn_arg -> FnArg, alt!(
        do_parse!(
            punct!("&") >>
            lt: option!(lifetime) >>
            mutability: mutability >>
            keyword!("self") >>
            not!(punct!(":")) >>
            (ArgSelfRef {
                lifetime: lt,
                mutbl: mutability,
                and_token: tokens::And::default(),
                self_token: tokens::Self_::default(),
            }.into())
        )
        |
        do_parse!(
            mutability: mutability >>
            keyword!("self") >>
            not!(punct!(":")) >>
            (ArgSelf {
                mutbl: mutability,
                self_token: tokens::Self_::default(),
            }.into())
        )
        |
        do_parse!(
            pat: pat >>
            punct!(":") >>
            ty: ty >>
            (ArgCaptured {
                pat: pat,
                ty: ty,
                colon_token: tokens::Colon::default(),
            }.into())
        )
        |
        ty => { FnArg::Ignored }
    ));

    named!(item_mod -> Item, do_parse!(
        outer_attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("mod") >>
        id: ident >>
        content: alt!(
            punct!(";") => { |_| None }
            |
            delim!(
                Brace,
                tuple!(
                    many0!(inner_attr),
                    items
                )
            ) => { Some }
        ) >>
        (match content {
            Some((inner_attrs, items)) => Item {
                ident: id,
                vis: vis,
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend(inner_attrs);
                    attrs
                },
                node: ItemMod {
                    mod_token: tokens::Mod::default(),
                    semi_token: None,
                    items: Some((items, tokens::Brace::default())),
                }.into(),
            },
            None => Item {
                ident: id,
                vis: vis,
                attrs: outer_attrs,
                node: ItemMod {
                    items: None,
                    mod_token: tokens::Mod::default(),
                    semi_token: Some(tokens::Semi::default()),
                }.into(),
            },
        })
    ));

    named!(item_foreign_mod -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        abi: abi >>
        items: delim!(Brace, many0!(foreign_item)) >>
        (Item {
            ident: "".into(),
            vis: VisInherited {}.into(),
            attrs: attrs,
            node: ItemForeignMod {
                brace_token: tokens::Brace::default(),
                abi: abi,
                items: items,
            }.into(),
        })
    ));

    named!(foreign_item -> ForeignItem, alt!(
        foreign_fn
        |
        foreign_static
    ));

    named!(foreign_fn -> ForeignItem, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("fn") >>
        name: ident >>
        generics: generics >>
        punct!("(") >>
        inputs: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 fn_arg) >>
        variadic: cond!(inputs.is_empty() || inputs.trailing_delim(),
                        option!(punct!("..."))) >>
        punct!(")") >>
        ret: fn_ret_ty >>
        where_clause: where_clause >>
        punct!(";") >>
        (ForeignItem {
            ident: name,
            attrs: attrs,
            semi_token: tokens::Semi::default(),
            node: ForeignItemFn {
                decl: Box::new(FnDecl {
                    fn_token: tokens::Fn::default(),
                    paren_token: tokens::Paren::default(),
                    inputs: inputs,
                    variadic: variadic.map(|m| m.is_some()).unwrap_or(false),
                    dot_tokens: if variadic.map(|m| m.is_some()).unwrap_or(false) {
                        Some(tokens::Dot3::default())
                    } else {
                        None
                    },
                    output: ret,
                    generics: Generics {
                        where_clause: where_clause,
                        .. generics
                    },
                }),
            }.into(),
            vis: vis,
        })
    ));

    named!(foreign_static -> ForeignItem, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("static") >>
        mutability: mutability >>
        id: ident >>
        punct!(":") >>
        ty: ty >>
        punct!(";") >>
        (ForeignItem {
            ident: id,
            attrs: attrs,
            semi_token: tokens::Semi::default(),
            node: ForeignItemStatic {
                ty: Box::new(ty),
                mutbl: mutability,
                static_token: tokens::Static::default(),
                colon_token: tokens::Colon::default(),
            }.into(),
            vis: vis,
        })
    ));

    named!(item_ty -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("type") >>
        id: ident >>
        generics: generics >>
        where_clause: where_clause >>
        punct!("=") >>
        ty: ty >>
        punct!(";") >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemTy {
                type_token: tokens::Type::default(),
                eq_token: tokens::Eq::default(),
                semi_token: tokens::Semi::default(),
                ty: Box::new(ty),
                generics: Generics {
                    where_clause: where_clause,
                    ..generics
                },
            }.into(),
        })
    ));

    named!(item_struct_or_enum -> Item, map!(
        derive_input,
        |def: DeriveInput| Item {
            ident: def.ident,
            vis: def.vis,
            attrs: def.attrs,
            node: match def.body {
                Body::Enum(data) => {
                    ItemEnum {
                        variants: data.variants,
                        brace_token: data.brace_token,
                        enum_token: data.enum_token,
                        generics: def.generics,
                    }.into()
                }
                Body::Struct(data) => {
                    ItemStruct {
                        data: data.data,
                        struct_token: data.struct_token,
                        semi_token: data.semi_token,
                        generics: def.generics,
                    }.into()
                }
            }
        }
    ));

    named!(item_union -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        keyword!("union") >>
        id: ident >>
        generics: generics >>
        where_clause: where_clause >>
        fields: struct_like_body >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemUnion {
                union_token: tokens::Union::default(),
                data: VariantData::Struct(fields.0, fields.1),
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
            }.into(),
        })
    ));

    named!(item_trait -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        unsafety: unsafety >>
        keyword!("trait") >>
        id: ident >>
        generics: generics >>
        colon: option!(punct!(":")) >>
        bounds: cond!(colon.is_some(),
            separated_nonempty_list!(map!(punct!("+"), |_| tokens::Add::default()),
                                     ty_param_bound)
        ) >>
        where_clause: where_clause >>
        body: delim!(Brace, many0!(trait_item)) >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemTrait {
                trait_token: tokens::Trait::default(),
                brace_token: tokens::Brace::default(),
                colon_token: colon.map(|_| tokens::Colon::default()),
                unsafety: unsafety,
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
                supertraits: bounds.unwrap_or_default(),
                items: body,
            }.into(),
        })
    ));

    named!(item_default_impl -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        unsafety: unsafety >>
        keyword!("impl") >>
        path: path >>
        keyword!("for") >>
        punct!("..") >>
        delim!(Brace, epsilon!()) >>
        (Item {
            ident: "".into(),
            vis: VisInherited {}.into(),
            attrs: attrs,
            node: ItemDefaultImpl {
                unsafety: unsafety,
                path: path,
                impl_token: tokens::Impl::default(),
                for_token: tokens::For::default(),
                dot2_token: tokens::Dot2::default(),
                brace_token: tokens::Brace::default(),
            }.into(),
        })
    ));

    named!(trait_item -> TraitItem, alt!(
        trait_item_const
        |
        trait_item_method
        |
        trait_item_type
        |
        trait_item_mac
    ));

    named!(trait_item_const -> TraitItem, do_parse!(
        attrs: many0!(outer_attr) >>
        keyword!("const") >>
        id: ident >>
        punct!(":") >>
        ty: ty >>
        value: option!(preceded!(punct!("="), expr)) >>
        punct!(";") >>
        (TraitItem {
            ident: id,
            attrs: attrs,
            node: TraitItemConst {
                ty: ty,
                const_token: tokens::Const::default(),
                colon_token: tokens::Colon::default(),
                eq_token: value.as_ref().map(|_| tokens::Eq::default()),
                default: value,
                semi_token: tokens::Semi::default(),
            }.into(),
        })
    ));

    named!(trait_item_method -> TraitItem, do_parse!(
        outer_attrs: many0!(outer_attr) >>
        constness: constness >>
        unsafety: unsafety >>
        abi: option!(abi) >>
        keyword!("fn") >>
        name: ident >>
        generics: generics >>
        punct!("(") >>
        inputs: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()), fn_arg) >>
        punct!(")") >>
        ret: fn_ret_ty >>
        where_clause: where_clause >>
        body: option!(delim!(
            Brace,
            tuple!(many0!(inner_attr), within_block)
        )) >>
        semi: cond!(body.is_none(), punct!(";")) >>
        ({
            let (inner_attrs, stmts) = match body {
                Some((inner_attrs, stmts)) => (inner_attrs, Some(stmts)),
                None => (Vec::new(), None),
            };
            TraitItem {
                ident: name,
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend(inner_attrs);
                    attrs
                },
                node: TraitItemMethod {
                    semi_token: semi.map(|_| tokens::Semi::default()),
                    sig: MethodSig {
                        unsafety: unsafety,
                        constness: constness,
                        abi: abi,
                        decl: FnDecl {
                            inputs: inputs,
                            output: ret,
                            variadic: false,
                            fn_token: tokens::Fn::default(),
                            paren_token: tokens::Paren::default(),
                            dot_tokens: None,
                            generics: Generics {
                                where_clause: where_clause,
                                .. generics
                            },
                        },
                    },
                    default: stmts.map(|stmts| {
                        Block {
                            stmts: stmts,
                            brace_token: tokens::Brace::default(),
                        }
                    }),
                }.into(),
            }
        })
    ));

    named!(trait_item_type -> TraitItem, do_parse!(
        attrs: many0!(outer_attr) >>
        keyword!("type") >>
        id: ident >>
        colon: option!(punct!(":")) >>
        bounds: cond!(colon.is_some(),
            separated_nonempty_list!(map!(punct!("+"), |_| tokens::Add::default()),
                                     ty_param_bound)
        ) >>
        default: option!(preceded!(punct!("="), ty)) >>
        punct!(";") >>
        (TraitItem {
            ident: id,
            attrs: attrs,
            node: TraitItemType {
                type_token: tokens::Type::default(),
                colon_token: colon.map(|_| tokens::Colon::default()),
                bounds: bounds.unwrap_or_default(),
                eq_token: default.as_ref().map(|_| tokens::Eq::default()),
                semi_token: tokens::Semi::default(),
                default: default,
            }.into(),
        })
    ));

    named!(trait_item_mac -> TraitItem, do_parse!(
        attrs: many0!(outer_attr) >>
        what: path >>
        punct!("!") >>
        body: delimited >>
        cond!(!body.is_braced(), punct!(";")) >>
        (TraitItem {
            ident: "".into(),
            attrs: attrs,
            node: TraitItemKind::Macro(Mac {
                path: what,
                bang_token: tokens::Bang::default(),
                tokens: vec![body],
            }),
        })
    ));

    named!(item_impl -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        unsafety: unsafety >>
        keyword!("impl") >>
        generics: generics >>
        polarity_path: alt!(
            do_parse!(
                polarity: impl_polarity >>
                path: path >>
                keyword!("for") >>
                (polarity, Some(path))
            )
            |
            epsilon!() => { |_| (ImplPolarity::Positive, None) }
        ) >>
        self_ty: ty >>
        where_clause: where_clause >>
        body: delim!(Brace, many0!(impl_item)) >>
        (Item {
            ident: "".into(),
            vis: VisInherited {}.into(),
            attrs: attrs,
            node: ItemImpl {
                impl_token: tokens::Impl::default(),
                brace_token: tokens::Brace::default(),
                for_token: polarity_path.1.as_ref().map(|_| tokens::For::default()),
                unsafety: unsafety,
                polarity: polarity_path.0,
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
                trait_: polarity_path.1,
                self_ty: Box::new(self_ty),
                items: body,
            }.into(),
        })
    ));

    named!(impl_item -> ImplItem, alt!(
        impl_item_const
        |
        impl_item_method
        |
        impl_item_type
        |
        impl_item_macro
    ));

    named!(impl_item_const -> ImplItem, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        defaultness: defaultness >>
        keyword!("const") >>
        id: ident >>
        punct!(":") >>
        ty: ty >>
        punct!("=") >>
        value: expr >>
        punct!(";") >>
        (ImplItem {
            ident: id,
            vis: vis,
            defaultness: defaultness,
            attrs: attrs,
            node: ImplItemConst {
                ty: ty,
                expr: value,
                const_token: tokens::Const::default(),
                colon_token: tokens::Colon::default(),
                eq_token: tokens::Eq::default(),
                semi_token: tokens::Semi::default(),
            }.into(),
        })
    ));

    named!(impl_item_method -> ImplItem, do_parse!(
        outer_attrs: many0!(outer_attr) >>
        vis: visibility >>
        defaultness: defaultness >>
        constness: constness >>
        unsafety: unsafety >>
        abi: option!(abi) >>
        keyword!("fn") >>
        name: ident >>
        generics: generics >>
        punct!("(") >>
        inputs: terminated_list!(map!(punct!(","), |_| tokens::Comma::default()),
                                 fn_arg) >>
        punct!(")") >>
        ret: fn_ret_ty >>
        where_clause: where_clause >>
        inner_attrs_stmts: delim!(Brace, tuple!(
            many0!(inner_attr), within_block
        )) >>
        (ImplItem {
            ident: name,
            vis: vis,
            defaultness: defaultness,
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend(inner_attrs_stmts.0);
                attrs
            },
            node: ImplItemMethod {
                sig: MethodSig {
                    unsafety: unsafety,
                    constness: constness,
                    abi: abi,
                    decl: FnDecl {
                        fn_token: tokens::Fn::default(),
                        paren_token: tokens::Paren::default(),
                        inputs: inputs,
                        output: ret,
                        variadic: false,
                        generics: Generics {
                            where_clause: where_clause,
                            .. generics
                        },
                        dot_tokens: None,
                    },
                },
                block: Block {
                    brace_token: tokens::Brace::default(),
                    stmts: stmts,
                },
            }.into(),
        })
    ));

    named!(impl_item_type -> ImplItem, do_parse!(
        attrs: many0!(outer_attr) >>
        vis: visibility >>
        defaultness: defaultness >>
        keyword!("type") >>
        id: ident >>
        punct!("=") >>
        ty: ty >>
        punct!(";") >>
        (ImplItem {
            ident: id,
            vis: vis,
            defaultness: defaultness,
            attrs: attrs,
            node: ImplItemType {
                type_token: tokens::Type::default(),
                eq_token: tokens::Eq::default(),
                semi_token: tokens::Semi::default(),
                ty: ty,
            }.into(),
        })
    ));

    named!(impl_item_macro -> ImplItem, do_parse!(
        attrs: many0!(outer_attr) >>
        what: path >>
        punct!("!") >>
        body: delimited >>
        cond!(!body.is_braced(), punct!(";")) >>
        (ImplItem {
            ident: "".into(),
            vis: VisInherited {}.into(),
            defaultness: Defaultness::Final,
            attrs: attrs,
            node: ImplItemKind::Macro(Mac {
                path: what,
                bang_token: tokens::Bang::default(),
                tokens: vec![body],
            }),
        })
    ));

    named!(impl_polarity -> ImplPolarity, alt!(
        punct!("!") => { |_| ImplPolarity::Negative(tokens::Bang::default()) }
        |
        epsilon!() => { |_| ImplPolarity::Positive }
    ));

    named!(constness -> Constness, alt!(
        keyword!("const") => { |_| Constness::Const(tokens::Const::default()) }
        |
        epsilon!() => { |_| Constness::NotConst }
    ));

    named!(defaultness -> Defaultness, alt!(
        keyword!("default") => { |_| Defaultness::Default(tokens::Default::default()) }
        |
        epsilon!() => { |_| Defaultness::Final }
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use attr::FilterAttrs;
    use data::VariantData;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Item {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            match self.node {
                ItemKind::ExternCrate(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.extern_token.to_tokens(tokens);
                    item.crate_token.to_tokens(tokens);
                    item.original.to_tokens(tokens);
                    item.as_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Use(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.use_token.to_tokens(tokens);
                    item.path.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Static(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.static_token.to_tokens(tokens);
                    item.mutbl.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.expr.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Const(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.const_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.expr.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Fn(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.constness.to_tokens(tokens);
                    item.unsafety.to_tokens(tokens);
                    item.abi.to_tokens(tokens);
                    NamedDecl(&item.decl, &self.ident).to_tokens(tokens);
                    item.block.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(self.attrs.inner());
                        tokens.append_all(&item.block.stmts);
                    });
                }
                ItemKind::Mod(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.mod_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    if let Some((ref items, ref brace)) = item.items {
                        brace.surround(tokens, |tokens| {
                            tokens.append_all(self.attrs.inner());
                            tokens.append_all(items);
                        });
                    }
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::ForeignMod(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.abi.to_tokens(tokens);
                    item.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(&item.items);
                    });
                }
                ItemKind::Ty(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.type_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Enum(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.enum_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    item.brace_token.surround(tokens, |tokens| {
                        item.variants.to_tokens(tokens);
                    });
                }
                ItemKind::Struct(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.struct_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    match item.data {
                        VariantData::Struct(..) => {
                            item.generics.where_clause.to_tokens(tokens);
                            item.data.to_tokens(tokens);
                        }
                        VariantData::Tuple(..) => {
                            item.data.to_tokens(tokens);
                            item.generics.where_clause.to_tokens(tokens);
                        }
                        VariantData::Unit => {
                            item.generics.where_clause.to_tokens(tokens);
                        }
                    }
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Union(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.union_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    item.data.to_tokens(tokens);
                }
                ItemKind::Trait(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.unsafety.to_tokens(tokens);
                    item.trait_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.supertraits.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    item.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(&item.items);
                    });
                }
                ItemKind::DefaultImpl(ref item) => {
                    item.unsafety.to_tokens(tokens);
                    item.impl_token.to_tokens(tokens);
                    item.path.to_tokens(tokens);
                    item.for_token.to_tokens(tokens);
                    item.dot2_token.to_tokens(tokens);
                    item.brace_token.surround(tokens, |_tokens| {});
                }
                ItemKind::Impl(ref item) => {
                    item.unsafety.to_tokens(tokens);
                    item.impl_token.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.polarity.to_tokens(tokens);
                    item.trait_.to_tokens(tokens);
                    item.for_token.to_tokens(tokens);
                    item.self_ty.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    item.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(&item.items);
                    });
                }
                ItemKind::Mac(ref mac) => {
                    mac.path.to_tokens(tokens);
                    mac.bang_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    tokens.append_all(&mac.tokens);
                    if !mac.is_braced() {
                        tokens::Semi::default().to_tokens(tokens);
                    }
                }
            }
        }
    }

    impl ToTokens for PathSimple {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.as_token.to_tokens(tokens);
            self.rename.to_tokens(tokens);
        }
    }

    impl ToTokens for PathGlob {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.colon2_token.to_tokens(tokens);
            self.star_token.to_tokens(tokens);
        }
    }

    impl ToTokens for PathList {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            self.colon2_token.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                self.items.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for PathListItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.name.to_tokens(tokens);
            self.as_token.to_tokens(tokens);
            self.rename.to_tokens(tokens);
        }
    }

    impl ToTokens for TraitItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            match self.node {
                TraitItemKind::Const(ref item) => {
                    item.const_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.default.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                TraitItemKind::Method(ref item) => {
                    NamedMethod(&item.sig, &self.ident).to_tokens(tokens);
                    match item.default {
                        Some(ref block) => {
                            block.brace_token.surround(tokens, |tokens| {
                                tokens.append_all(self.attrs.inner());
                                tokens.append_all(&block.stmts);
                            });
                        }
                        None => {
                            item.semi_token.to_tokens(tokens);
                        }
                    }
                }
                TraitItemKind::Type(ref item) => {
                    item.type_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.bounds.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.default.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                TraitItemKind::Macro(ref mac) => {
                    mac.to_tokens(tokens);
                    if !mac.is_braced() {
                        tokens::Semi::default().to_tokens(tokens);
                    }
                }
            }
        }
    }

    impl ToTokens for ImplItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            match self.node {
                ImplItemKind::Const(ref item) => {
                    item.const_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.expr.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ImplItemKind::Method(ref item) => {
                    NamedMethod(&item.sig, &self.ident).to_tokens(tokens);
                    item.block.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(self.attrs.inner());
                        tokens.append_all(&item.block.stmts);
                    });
                }
                ImplItemKind::Type(ref item) => {
                    item.type_token.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ImplItemKind::Macro(ref mac) => {
                    mac.to_tokens(tokens);
                    if !mac.is_braced() {
                        tokens::Semi::default().to_tokens(tokens);
                    }
                }
            }
        }
    }

    impl ToTokens for ForeignItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            match self.node {
                ForeignItemKind::Fn(ref item) => {
                    NamedDecl(&item.decl, &self.ident).to_tokens(tokens)
                }
                ForeignItemKind::Static(ref item) => {
                    item.static_token.to_tokens(tokens);
                    item.mutbl.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                }
            }
            self.semi_token.to_tokens(tokens);
        }
    }

    struct NamedMethod<'a>(&'a MethodSig, &'a Ident);

    impl<'a> ToTokens for NamedMethod<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.0.constness.to_tokens(tokens);
            self.0.unsafety.to_tokens(tokens);
            self.0.abi.to_tokens(tokens);
            NamedDecl(&self.0.decl, self.1).to_tokens(tokens);
        }
    }

    struct NamedDecl<'a>(&'a FnDecl, &'a Ident);

    impl<'a> ToTokens for NamedDecl<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.0.fn_token.to_tokens(tokens);
            self.1.to_tokens(tokens);
            self.0.generics.to_tokens(tokens);
            self.0.paren_token.surround(tokens, |tokens| {
                self.0.inputs.to_tokens(tokens);
                self.0.dot_tokens.to_tokens(tokens);
            });
            self.0.output.to_tokens(tokens);
            self.0.generics.where_clause.to_tokens(tokens);
        }
    }

    impl ToTokens for ArgSelfRef {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.and_token.to_tokens(tokens);
            self.lifetime.to_tokens(tokens);
            self.mutbl.to_tokens(tokens);
            self.self_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ArgSelf {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.mutbl.to_tokens(tokens);
            self.self_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ArgCaptured {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.pat.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for Constness {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Constness::Const(ref t) => t.to_tokens(tokens),
                Constness::NotConst => {
                    // nothing
                }
            }
        }
    }

    impl ToTokens for Defaultness {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Defaultness::Default(ref t) => t.to_tokens(tokens),
                Defaultness::Final => {
                    // nothing
                }
            }
        }
    }

    impl ToTokens for ImplPolarity {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                ImplPolarity::Negative(ref t) => t.to_tokens(tokens),
                ImplPolarity::Positive => {
                    // nothing
                }
            }
        }
    }
}
