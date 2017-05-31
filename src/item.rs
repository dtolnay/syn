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
        Default(tokens::Default_),
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
        pub fn_token: tokens::Fn_,
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

    use proc_macro2::TokenTree;
    use synom::{IResult, Synom};
    use synom::tokens::*;
    use synom::tokens;

    impl Synom for Item {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
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
            }
        }

        fn description() -> Option<&'static str> {
            Some("item")
        }
    }

    named!(item_mac -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        what: syn!(Path) >>
        bang: syn!(Bang) >>
        name: option!(syn!(Ident)) >>
        body: call!(::TokenTree::parse_delimited) >>
        cond!(!body.is_braced(), syn!(Semi)) >>
        (Item {
            ident: name.unwrap_or_else(|| Ident::from("")),
            vis: VisInherited {}.into(),
            attrs: attrs,
            node: ItemKind::Mac(Mac {
                bang_token: bang,
                path: what,
                tokens: vec![body],
            }),
        })
    ));

    named!(item_extern_crate -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        extern_: syn!(Extern) >>
        crate_: syn!(tokens::Crate) >>
        id: syn!(Ident) >>
        rename: option!(tuple!(syn!(As), syn!(Ident))) >>
        semi: syn!(Semi) >>
        ({
            let (name, original_name, as_) = match rename {
                Some((as_, rename)) => (rename, Some(id), Some(as_)),
                None => (id, None, None),
            };
            Item {
                ident: name,
                vis: vis,
                attrs: attrs,
                node: ItemExternCrate {
                    as_token: as_,
                    original: original_name,
                    extern_token: extern_,
                    crate_token: crate_,
                    semi_token: semi,
                }.into(),
            }
        })
    ));

    named!(item_use -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        use_: syn!(Use) >>
        what: syn!(ViewPath) >>
        semi: syn!(Semi) >>
        (Item {
            ident: "".into(),
            vis: vis,
            attrs: attrs,
            node: ItemUse {
                path: Box::new(what),
                use_token: use_,
                semi_token: semi,
            }.into(),
        })
    ));

    impl Synom for ViewPath {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
                syn!(PathGlob) => { ViewPath::Glob }
                |
                syn!(PathList) => { ViewPath::List }
                |
                syn!(PathSimple) => { ViewPath::Simple } // must be last
            }
        }
    }

    impl Synom for PathSimple {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            do_parse! {
                input,
                path: syn!(Path) >>
                rename: option!(tuple!(syn!(As), syn!(Ident))) >>
                (PathSimple {
                    path: path,
                    as_token: rename.as_ref().map(|p| As((p.0).0)),
                    rename: rename.map(|p| p.1),
                })
            }
        }
    }

    impl Synom for PathGlob {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            do_parse! {
                input,
                path: syn!(Path) >>
                colon2: syn!(Colon2) >>
                star: syn!(Star) >>
                (PathGlob {
                    path: path,
                    colon2_token: colon2,
                    star_token: star,
                })
            }
        }
    }

    impl Synom for PathList {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
                do_parse!(
                    path: syn!(Path) >>
                    colon2: syn!(Colon2) >>
                    items: braces!(call!(Delimited::parse_terminated)) >>
                    (PathList {
                        path: path,
                        items: items.0,
                        brace_token: items.1,
                        colon2_token: colon2,
                    })
                )
                |
                do_parse!(
                    global: option!(syn!(Colon2)) >>
                    items: braces!(call!(Delimited::parse_terminated)) >>
                    (PathList {
                        path: Path {
                            global: global.is_some(),
                            segments: Delimited::new(),
                            leading_colon: None,
                        },
                        colon2_token: global.unwrap_or_default(),
                        brace_token: items.1,
                        items: items.0,
                    })
                )
            }
        }
    }

    impl Synom for PathListItem {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            do_parse! {
                input,
                name: alt!(
                    syn!(Ident)
                    |
                    map!(syn!(Self_), Into::into)
                ) >>
                rename: option!(tuple!(syn!(As), syn!(Ident))) >>
                (PathListItem {
                    name: name,
                    as_token: rename.as_ref().map(|p| As((p.0).0)),
                    rename: rename.map(|p| p.1),
                })
            }
        }
    }

    named!(item_static -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        static_: syn!(Static) >>
        mutability: syn!(Mutability) >>
        id: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        eq: syn!(Eq) >>
        value: syn!(Expr) >>
        semi: syn!(Semi) >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemStatic {
                ty: Box::new(ty),
                mutbl: mutability,
                expr: Box::new(value),
                static_token: static_,
                colon_token: colon,
                eq_token: eq,
                semi_token: semi,
            }.into(),
        })
    ));

    named!(item_const -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        const_: syn!(Const) >>
        id: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        eq: syn!(Eq) >>
        value: syn!(Expr) >>
        semi: syn!(Semi) >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemConst {
                ty: Box::new(ty),
                expr: Box::new(value),
                const_token: const_,
                colon_token: colon,
                eq_token: eq,
                semi_token: semi,
            }.into(),
        })
    ));

    named!(item_fn -> Item, do_parse!(
        outer_attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        constness: syn!(Constness) >>
        unsafety: syn!(Unsafety) >>
        abi: option!(syn!(Abi)) >>
        fn_: syn!(Fn_) >>
        name: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(Delimited::parse_terminated) >>
        ret: syn!(FunctionRetTy) >>
        where_clause: syn!(WhereClause) >>
        inner_attrs_stmts: braces!(tuple!(
            many0!(call!(Attribute::parse_inner)),
            call!(Block::parse_within)
        )) >>
        (Item {
            ident: name,
            vis: vis,
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend((inner_attrs_stmts.0).0);
                attrs
            },
            node: ItemFn {
                decl: Box::new(FnDecl {
                    dot_tokens: None,
                    fn_token: fn_,
                    paren_token: inputs.1,
                    inputs: inputs.0,
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
                    brace_token: inner_attrs_stmts.1,
                    stmts: (inner_attrs_stmts.0).1,
                }),
            }.into(),
        })
    ));

    impl Synom for FnArg {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
                do_parse!(
                    and: syn!(And) >>
                    lt: option!(syn!(Lifetime)) >>
                    mutability: syn!(Mutability) >>
                    self_: syn!(Self_) >>
                    not!(syn!(Colon)) >>
                    (ArgSelfRef {
                        lifetime: lt,
                        mutbl: mutability,
                        and_token: and,
                        self_token: self_,
                    }.into())
                )
                |
                do_parse!(
                    mutability: syn!(Mutability) >>
                    self_: syn!(Self_) >>
                    not!(syn!(Colon)) >>
                    (ArgSelf {
                        mutbl: mutability,
                        self_token: self_,
                    }.into())
                )
                |
                do_parse!(
                    pat: syn!(Pat) >>
                    colon: syn!(Colon) >>
                    ty: syn!(Ty) >>
                    (ArgCaptured {
                        pat: pat,
                        ty: ty,
                        colon_token: colon,
                    }.into())
                )
                |
                syn!(Ty) => { FnArg::Ignored }
            }
        }
    }

    named!(item_mod -> Item, do_parse!(
        outer_attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        mod_: syn!(Mod) >>
        id: syn!(Ident) >>
        content: alt!(
            syn!(Semi) => { Ok }
            |
            braces!(
                tuple!(
                    many0!(call!(Attribute::parse_inner)),
                    many0!(syn!(Item))
                )
            ) => { Err }
        ) >>
        (match content {
            Err(((inner_attrs, items), braces)) => Item {
                ident: id,
                vis: vis,
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend(inner_attrs);
                    attrs
                },
                node: ItemMod {
                    mod_token: mod_,
                    semi_token: None,
                    items: Some((items, braces)),
                }.into(),
            },
            Ok(semi) => Item {
                ident: id,
                vis: vis,
                attrs: outer_attrs,
                node: ItemMod {
                    items: None,
                    mod_token: mod_,
                    semi_token: Some(semi),
                }.into(),
            },
        })
    ));

    named!(item_foreign_mod -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        abi: syn!(Abi) >>
        items: braces!(many0!(syn!(ForeignItem))) >>
        (Item {
            ident: "".into(),
            vis: VisInherited {}.into(),
            attrs: attrs,
            node: ItemForeignMod {
                brace_token: items.1,
                abi: abi,
                items: items.0,
            }.into(),
        })
    ));

    impl Synom for ForeignItem {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
                foreign_fn
                |
                foreign_static
            }
        }
    }

    named!(foreign_fn -> ForeignItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        fn_: syn!(Fn_) >>
        name: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(do_parse!(
            args: call!(Delimited::parse_terminated) >>
            variadic: cond!(args.is_empty() || args.trailing_delim(),
                            option!(syn!(Dot3))) >>
            (args, variadic)
        )) >>
        ret: syn!(FunctionRetTy) >>
        where_clause: syn!(WhereClause) >>
        semi: syn!(Semi) >>
        ({
            let ((inputs, variadic), parens) = inputs;
            let variadic = variadic.and_then(|v| v);
            ForeignItem {
                ident: name,
                attrs: attrs,
                semi_token: semi,
                node: ForeignItemFn {
                    decl: Box::new(FnDecl {
                        fn_token: fn_,
                        paren_token: parens,
                        inputs: inputs,
                        variadic: variadic.is_some(),
                        dot_tokens: variadic,
                        output: ret,
                        generics: Generics {
                            where_clause: where_clause,
                            .. generics
                        },
                    }),
                }.into(),
                vis: vis,
            }
        })
    ));

    named!(foreign_static -> ForeignItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        static_: syn!(Static) >>
        mutability: syn!(Mutability) >>
        id: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        semi: syn!(Semi) >>
        (ForeignItem {
            ident: id,
            attrs: attrs,
            semi_token: semi,
            node: ForeignItemStatic {
                ty: Box::new(ty),
                mutbl: mutability,
                static_token: static_,
                colon_token: colon,
            }.into(),
            vis: vis,
        })
    ));

    named!(item_ty -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        type_: syn!(Type) >>
        id: syn!(Ident) >>
        generics: syn!(Generics) >>
        where_clause: syn!(WhereClause) >>
        eq: syn!(Eq) >>
        ty: syn!(Ty) >>
        semi: syn!(Semi) >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemTy {
                type_token: type_,
                eq_token: eq,
                semi_token: semi,
                ty: Box::new(ty),
                generics: Generics {
                    where_clause: where_clause,
                    ..generics
                },
            }.into(),
        })
    ));

    named!(item_struct_or_enum -> Item, map!(
        syn!(DeriveInput),
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
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        union_: syn!(Union) >>
        id: syn!(Ident) >>
        generics: syn!(Generics) >>
        where_clause: syn!(WhereClause) >>
        fields: braces!(call!(Delimited::parse_terminated_with,
                              Field::parse_struct)) >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemUnion {
                union_token: union_,
                data: VariantData::Struct(fields.0, fields.1),
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
            }.into(),
        })
    ));

    named!(item_trait -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        unsafety: syn!(Unsafety) >>
        trait_: syn!(Trait) >>
        id: syn!(Ident) >>
        generics: syn!(Generics) >>
        colon: option!(syn!(Colon)) >>
        bounds: cond!(colon.is_some(),
            call!(Delimited::parse_separated_nonempty)
        ) >>
        where_clause: syn!(WhereClause) >>
        body: braces!(many0!(syn!(TraitItem))) >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemTrait {
                trait_token: trait_,
                brace_token: body.1,
                colon_token: colon,
                unsafety: unsafety,
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
                supertraits: bounds.unwrap_or_default(),
                items: body.0,
            }.into(),
        })
    ));

    named!(item_default_impl -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        unsafety: syn!(Unsafety) >>
        impl_: syn!(Impl) >>
        path: syn!(Path) >>
        for_: syn!(For) >>
        dot2: syn!(Dot2) >>
        braces: braces!(epsilon!()) >>
        (Item {
            ident: "".into(),
            vis: VisInherited {}.into(),
            attrs: attrs,
            node: ItemDefaultImpl {
                unsafety: unsafety,
                path: path,
                impl_token: impl_,
                for_token: for_,
                dot2_token: dot2,
                brace_token: braces.1,
            }.into(),
        })
    ));

    impl Synom for TraitItem {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
                trait_item_const
                |
                trait_item_method
                |
                trait_item_type
                |
                trait_item_mac
            }
        }
    }

    named!(trait_item_const -> TraitItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        const_: syn!(Const) >>
        id: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        value: option!(tuple!(syn!(Eq), syn!(Expr))) >>
        semi: syn!(Semi) >>
        (TraitItem {
            ident: id,
            attrs: attrs,
            node: TraitItemConst {
                ty: ty,
                const_token: const_,
                colon_token: colon,
                eq_token: value.as_ref().map(|p| Eq((p.0).0)),
                default: value.map(|p| p.1),
                semi_token: semi,
            }.into(),
        })
    ));

    named!(trait_item_method -> TraitItem, do_parse!(
        outer_attrs: many0!(call!(Attribute::parse_outer)) >>
        constness: syn!(Constness) >>
        unsafety: syn!(Unsafety) >>
        abi: option!(syn!(Abi)) >>
        fn_: syn!(Fn_) >>
        name: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(call!(Delimited::parse_terminated)) >>
        ret: syn!(FunctionRetTy) >>
        where_clause: syn!(WhereClause) >>
        body: option!(braces!(
            tuple!(many0!(call!(Attribute::parse_inner)),
                   call!(Block::parse_within))
        )) >>
        semi: cond!(body.is_none(), syn!(Semi)) >>
        ({
            let (inner_attrs, stmts) = match body {
                Some(((inner_attrs, stmts), b)) => (inner_attrs, Some((stmts, b))),
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
                    semi_token: semi,
                    sig: MethodSig {
                        unsafety: unsafety,
                        constness: constness,
                        abi: abi,
                        decl: FnDecl {
                            inputs: inputs.0,
                            output: ret,
                            variadic: false,
                            fn_token: fn_,
                            paren_token: inputs.1,
                            dot_tokens: None,
                            generics: Generics {
                                where_clause: where_clause,
                                .. generics
                            },
                        },
                    },
                    default: stmts.map(|stmts| {
                        Block {
                            stmts: stmts.0,
                            brace_token: stmts.1,
                        }
                    }),
                }.into(),
            }
        })
    ));

    named!(trait_item_type -> TraitItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        type_: syn!(Type) >>
        id: syn!(Ident) >>
        colon: option!(syn!(Colon)) >>
        bounds: cond!(colon.is_some(),
            call!(Delimited::parse_separated_nonempty)
        ) >>
        default: option!(tuple!(syn!(Eq), syn!(Ty))) >>
        semi: syn!(Semi) >>
        (TraitItem {
            ident: id,
            attrs: attrs,
            node: TraitItemType {
                type_token: type_,
                colon_token: colon,
                eq_token: default.as_ref().map(|p| Eq((p.0).0)),
                bounds: bounds.unwrap_or_default(),
                semi_token: semi,
                default: default.map(|p| p.1),
            }.into(),
        })
    ));

    named!(trait_item_mac -> TraitItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        mac: syn!(Mac) >>
        cond!(!mac.is_braced(), syn!(Semi)) >>
        (TraitItem {
            ident: "".into(),
            attrs: attrs,
            node: TraitItemKind::Macro(mac),
        })
    ));

    named!(item_impl -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        unsafety: syn!(Unsafety) >>
        impl_: syn!(Impl) >>
        generics: syn!(Generics) >>
        polarity_path: alt!(
            do_parse!(
                polarity: syn!(ImplPolarity) >>
                path: syn!(Path) >>
                for_: syn!(For) >>
                (polarity, Some(path), Some(for_))
            )
            |
            epsilon!() => { |_| (ImplPolarity::Positive, None, None) }
        ) >>
        self_ty: syn!(Ty) >>
        where_clause: syn!(WhereClause) >>
        body: braces!(many0!(syn!(ImplItem))) >>
        (Item {
            ident: "".into(),
            vis: VisInherited {}.into(),
            attrs: attrs,
            node: ItemImpl {
                impl_token: impl_,
                brace_token: body.1,
                for_token: polarity_path.2,
                unsafety: unsafety,
                polarity: polarity_path.0,
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
                trait_: polarity_path.1,
                self_ty: Box::new(self_ty),
                items: body.0,
            }.into(),
        })
    ));

    impl Synom for ImplItem {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
                impl_item_const
                |
                impl_item_method
                |
                impl_item_type
                |
                impl_item_macro
            }
        }
    }

    named!(impl_item_const -> ImplItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        defaultness: syn!(Defaultness) >>
        const_: syn!(Const) >>
        id: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        eq: syn!(Eq) >>
        value: syn!(Expr) >>
        semi: syn!(Semi) >>
        (ImplItem {
            ident: id,
            vis: vis,
            defaultness: defaultness,
            attrs: attrs,
            node: ImplItemConst {
                ty: ty,
                expr: value,
                const_token: const_,
                colon_token: colon,
                eq_token: eq,
                semi_token: semi,
            }.into(),
        })
    ));

    named!(impl_item_method -> ImplItem, do_parse!(
        outer_attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        defaultness: syn!(Defaultness) >>
        constness: syn!(Constness) >>
        unsafety: syn!(Unsafety) >>
        abi: option!(syn!(Abi)) >>
        fn_: syn!(Fn_) >>
        name: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(call!(Delimited::parse_terminated)) >>
        ret: syn!(FunctionRetTy) >>
        where_clause: syn!(WhereClause) >>
        inner_attrs_stmts: braces!(tuple!(
            many0!(call!(Attribute::parse_inner)),
            call!(Block::parse_within)
        )) >>
        (ImplItem {
            ident: name,
            vis: vis,
            defaultness: defaultness,
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend((inner_attrs_stmts.0).0);
                attrs
            },
            node: ImplItemMethod {
                sig: MethodSig {
                    unsafety: unsafety,
                    constness: constness,
                    abi: abi,
                    decl: FnDecl {
                        fn_token: fn_,
                        paren_token: inputs.1,
                        inputs: inputs.0,
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
                    brace_token: inner_attrs_stmts.1,
                    stmts: (inner_attrs_stmts.0).1,
                },
            }.into(),
        })
    ));

    named!(impl_item_type -> ImplItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        defaultness: syn!(Defaultness) >>
        type_: syn!(Type) >>
        id: syn!(Ident) >>
        eq: syn!(Eq) >>
        ty: syn!(Ty) >>
        semi: syn!(Semi) >>
        (ImplItem {
            ident: id,
            vis: vis,
            defaultness: defaultness,
            attrs: attrs,
            node: ImplItemType {
                type_token: type_,
                eq_token: eq,
                semi_token: semi,
                ty: ty,
            }.into(),
        })
    ));

    named!(impl_item_macro -> ImplItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        mac: syn!(Mac) >>
        cond!(!mac.is_braced(), syn!(Semi)) >>
        (ImplItem {
            ident: "".into(),
            vis: VisInherited {}.into(),
            defaultness: Defaultness::Final,
            attrs: attrs,
            node: ImplItemKind::Macro(mac),
        })
    ));

    impl Synom for ImplPolarity {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
                syn!(Bang) => { ImplPolarity::Negative }
                |
                epsilon!() => { |_| ImplPolarity::Positive }
            }
        }
    }

    impl Synom for Constness {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
                syn!(Const) => { Constness::Const }
                |
                epsilon!() => { |_| Constness::NotConst }
            }
        }
    }

    impl Synom for Defaultness {
        fn parse(input: &[TokenTree]) -> IResult<&[TokenTree], Self> {
            alt! {
                input,
                syn!(Default_) => { Defaultness::Default }
                |
                epsilon!() => { |_| Defaultness::Final }
            }
        }
    }
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
