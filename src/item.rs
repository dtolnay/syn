use super::*;
use delimited::Delimited;

ast_struct! {
    /// Things that can appear directly inside of a module.
    pub struct Item {
        pub attrs: Vec<Attribute>,
        pub node: ItemKind,
    }
}

ast_enum_of_structs! {
    pub enum ItemKind {
        /// An `extern crate` item, with optional original crate name.
        ///
        /// E.g. `extern crate foo` or `extern crate foo_bar as foo`
        pub ExternCrate(ItemExternCrate {
            pub vis: Visibility,
            pub extern_token: tokens::Extern,
            pub crate_token: tokens::Crate,
            pub ident: Ident,
            pub rename: Option<(tokens::As, Ident)>,
            pub semi_token: tokens::Semi,
        }),
        /// A use declaration (`use` or `pub use`) item.
        ///
        /// E.g. `use foo;`, `use foo::bar;` or `use foo::bar as FooBar;`
        pub Use(ItemUse {
            pub vis: Visibility,
            pub use_token: tokens::Use,
            pub path: Box<ViewPath>,
            pub semi_token: tokens::Semi,
        }),
        /// A static item (`static` or `pub static`).
        ///
        /// E.g. `static FOO: i32 = 42;` or `static FOO: &'static str = "bar";`
        pub Static(ItemStatic {
            pub vis: Visibility,
            pub static_token: tokens::Static,
            pub mutbl: Mutability,
            pub ident: Ident,
            pub colon_token: tokens::Colon,
            pub ty: Box<Ty>,
            pub eq_token: tokens::Eq,
            pub expr: Box<Expr>,
            pub semi_token: tokens::Semi,
        }),
        /// A constant item (`const` or `pub const`).
        ///
        /// E.g. `const FOO: i32 = 42;`
        pub Const(ItemConst {
            pub vis: Visibility,
            pub const_token: tokens::Const,
            pub ident: Ident,
            pub colon_token: tokens::Colon,
            pub ty: Box<Ty>,
            pub eq_token: tokens::Eq,
            pub expr: Box<Expr>,
            pub semi_token: tokens::Semi,
        }),
        /// A function declaration (`fn` or `pub fn`).
        ///
        /// E.g. `fn foo(bar: usize) -> usize { .. }`
        pub Fn(ItemFn {
            pub vis: Visibility,
            pub constness: Constness,
            pub unsafety: Unsafety,
            pub abi: Option<Abi>,
            pub decl: Box<FnDecl>,
            pub ident: Ident,
            pub block: Box<Block>,
        }),
        /// A module declaration (`mod` or `pub mod`).
        ///
        /// E.g. `mod foo;` or `mod foo { .. }`
        pub Mod(ItemMod {
            pub vis: Visibility,
            pub mod_token: tokens::Mod,
            pub ident: Ident,
            pub content: Option<(tokens::Brace, Vec<Item>)>,
            pub semi: Option<tokens::Semi>,
        }),
        /// An external module (`extern` or `pub extern`).
        ///
        /// E.g. `extern {}` or `extern "C" {}`
        pub ForeignMod(ItemForeignMod {
            pub abi: Abi,
            pub brace_token: tokens::Brace,
            pub items: Vec<ForeignItem>,
        }),
        /// A type alias (`type` or `pub type`).
        ///
        /// E.g. `type Foo = Bar<u8>;`
        pub Ty(ItemTy {
            pub vis: Visibility,
            pub type_token: tokens::Type,
            pub ident: Ident,
            pub generics: Generics,
            pub eq_token: tokens::Eq,
            pub ty: Box<Ty>,
            pub semi_token: tokens::Semi,
        }),
        /// An enum definition (`enum` or `pub enum`).
        ///
        /// E.g. `enum Foo<A, B> { C<A>, D<B> }`
        pub Enum(ItemEnum {
            pub vis: Visibility,
            pub enum_token: tokens::Enum,
            pub ident: Ident,
            pub generics: Generics,
            pub brace_token: tokens::Brace,
            pub variants: Delimited<Variant, tokens::Comma>,
        }),
        /// A struct definition (`struct` or `pub struct`).
        ///
        /// E.g. `struct Foo<A> { x: A }`
        pub Struct(ItemStruct {
            pub vis: Visibility,
            pub struct_token: tokens::Struct,
            pub ident: Ident,
            pub generics: Generics,
            pub data: VariantData,
            pub semi_token: Option<tokens::Semi>,
        }),
        /// A union definition (`union` or `pub union`).
        ///
        /// E.g. `union Foo<A, B> { x: A, y: B }`
        pub Union(ItemUnion {
            pub vis: Visibility,
            pub union_token: tokens::Union,
            pub ident: Ident,
            pub generics: Generics,
            pub data: VariantData,
        }),
        /// A Trait declaration (`trait` or `pub trait`).
        ///
        /// E.g. `trait Foo { .. }` or `trait Foo<T> { .. }`
        pub Trait(ItemTrait {
            pub vis: Visibility,
            pub unsafety: Unsafety,
            pub trait_token: tokens::Trait,
            pub ident: Ident,
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
            pub impl_token: tokens::Impl,
            pub path: Path,
            pub for_token: tokens::For,
            pub dot2_token: tokens::Dot2,
            pub brace_token: tokens::Brace,
        }),
        /// An implementation.
        ///
        /// E.g. `impl<A> Foo<A> { .. }` or `impl<A> Trait for Foo<A> { .. }`
        pub Impl(ItemImpl {
            pub defaultness: Defaultness,
            pub unsafety: Unsafety,
            pub impl_token: tokens::Impl,
            pub generics: Generics,
            /// Trait this impl implements.
            pub trait_: Option<(ImplPolarity, Path, tokens::For)>,
            /// The Self type of the impl.
            pub self_ty: Box<Ty>,
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
            attrs: input.attrs,
            node: match input.body {
                Body::Enum(data) => {
                    ItemEnum {
                        vis: input.vis,
                        enum_token: data.enum_token,
                        ident: input.ident,
                        generics: input.generics,
                        brace_token: data.brace_token,
                        variants: data.variants,
                    }.into()
                }
                Body::Struct(data) => {
                    ItemStruct {
                        vis: input.vis,
                        struct_token: data.struct_token,
                        ident: input.ident,
                        generics: input.generics,
                        data: data.data,
                        semi_token: data.semi_token,
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
            pub colon2_token: Option<tokens::Colon2>,
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
        pub attrs: Vec<Attribute>,
        pub node: TraitItemKind,
    }
}

ast_enum_of_structs! {
    pub enum TraitItemKind {
        pub Const(TraitItemConst {
            pub const_token: tokens::Const,
            pub ident: Ident,
            pub colon_token: tokens::Colon,
            pub ty: Ty,
            pub default: Option<(tokens::Eq, Expr)>,
            pub semi_token: tokens::Semi,
        }),
        pub Method(TraitItemMethod {
            pub sig: MethodSig,
            pub default: Option<Block>,
            pub semi_token: Option<tokens::Semi>,
        }),
        pub Type(TraitItemType {
            pub type_token: tokens::Type,
            pub ident: Ident,
            pub colon_token: Option<tokens::Colon>,
            pub bounds: Delimited<TyParamBound, tokens::Add>,
            pub default: Option<(tokens::Eq, Ty)>,
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
        pub attrs: Vec<Attribute>,
        pub node: ImplItemKind,
    }
}

ast_enum_of_structs! {
    pub enum ImplItemKind {
        pub Const(ImplItemConst {
            pub vis: Visibility,
            pub defaultness: Defaultness,
            pub const_token: tokens::Const,
            pub ident: Ident,
            pub colon_token: tokens::Colon,
            pub ty: Ty,
            pub eq_token: tokens::Eq,
            pub expr: Expr,
            pub semi_token: tokens::Semi,
        }),
        pub Method(ImplItemMethod {
            pub vis: Visibility,
            pub defaultness: Defaultness,
            pub sig: MethodSig,
            pub block: Block,
        }),
        pub Type(ImplItemType {
            pub vis: Visibility,
            pub defaultness: Defaultness,
            pub type_token: tokens::Type,
            pub ident: Ident,
            pub eq_token: tokens::Eq,
            pub ty: Ty,
            pub semi_token: tokens::Semi,
        }),
        pub Macro(Mac),
    }

    do_not_generate_to_tokens
}

ast_struct! {
    /// Represents a method's signature in a trait declaration,
    /// or in an implementation.
    pub struct MethodSig {
        pub constness: Constness,
        pub unsafety: Unsafety,
        pub abi: Option<Abi>,
        pub ident: Ident,
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

    use synom::Synom;
    use synom::tokens::*;
    use synom::tokens;

    impl Synom for Item {
        named!(parse -> Self, alt!(
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

        fn description() -> Option<&'static str> {
            Some("item")
        }
    }

    named!(item_mac -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        what: syn!(Path) >>
        bang: syn!(Bang) >>
        ident: option!(syn!(Ident)) >>
        body: call!(::TokenTree::parse_delimited) >>
        cond!(!body.is_braced(), syn!(Semi)) >>
        (Item {
            attrs: attrs,
            node: ItemKind::Mac(Mac {
                path: what,
                bang_token: bang,
                ident: ident,
                tokens: vec![body],
            }),
        })
    ));

    named!(item_extern_crate -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        extern_: syn!(Extern) >>
        crate_: syn!(tokens::Crate) >>
        ident: syn!(Ident) >>
        rename: option!(tuple!(syn!(As), syn!(Ident))) >>
        semi: syn!(Semi) >>
        (Item {
            attrs: attrs,
            node: ItemExternCrate {
                vis: vis,
                extern_token: extern_,
                crate_token: crate_,
                ident: ident,
                rename: rename,
                semi_token: semi,
            }.into(),
        })
    ));

    named!(item_use -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        use_: syn!(Use) >>
        what: syn!(ViewPath) >>
        semi: syn!(Semi) >>
        (Item {
            attrs: attrs,
            node: ItemUse {
                vis: vis,
                use_token: use_,
                path: Box::new(what),
                semi_token: semi,
            }.into(),
        })
    ));

    impl Synom for ViewPath {
        named!(parse -> Self, alt!(
            syn!(PathGlob) => { ViewPath::Glob }
            |
            syn!(PathList) => { ViewPath::List }
            |
            syn!(PathSimple) => { ViewPath::Simple } // must be last
        ));
    }

    impl Synom for PathSimple {
        named!(parse -> Self, do_parse!(
            path: syn!(Path) >>
            rename: option!(tuple!(syn!(As), syn!(Ident))) >>
            (PathSimple {
                path: path,
                as_token: rename.as_ref().map(|p| As((p.0).0)),
                rename: rename.map(|p| p.1),
            })
        ));
    }

    impl Synom for PathGlob {
        named!(parse -> Self, do_parse!(
            path: option!(do_parse!(
                path: syn!(Path) >>
                colon2: syn!(Colon2) >>
                (path, colon2)
            )) >>
            star: syn!(Star) >>
            ({
                match path {
                    Some((path, colon2)) => {
                        PathGlob {
                            path: path,
                            colon2_token: Some(colon2),
                            star_token: star,
                        }
                    }
                    None => {
                        PathGlob {
                            path: Path {
                                leading_colon: None,
                                segments: Default::default(),
                            },
                            colon2_token: None,
                            star_token: star,
                        }
                    }
                }
            })
        ));
    }

    impl Synom for PathList {
        named!(parse -> Self, alt!(
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
                colon: option!(syn!(Colon2)) >>
                items: braces!(call!(Delimited::parse_terminated)) >>
                (PathList {
                    path: Path {
                        leading_colon: None,
                        segments: Delimited::new(),
                    },
                    colon2_token: colon.unwrap_or_default(),
                    brace_token: items.1,
                    items: items.0,
                })
            )
        ));
    }

    impl Synom for PathListItem {
        named!(parse -> Self, do_parse!(
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
        ));
    }

    named!(item_static -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        static_: syn!(Static) >>
        mutability: syn!(Mutability) >>
        ident: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        eq: syn!(Eq) >>
        value: syn!(Expr) >>
        semi: syn!(Semi) >>
        (Item {
            attrs: attrs,
            node: ItemStatic {
                vis: vis,
                static_token: static_,
                mutbl: mutability,
                ident: ident,
                colon_token: colon,
                ty: Box::new(ty),
                eq_token: eq,
                expr: Box::new(value),
                semi_token: semi,
            }.into(),
        })
    ));

    named!(item_const -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        const_: syn!(Const) >>
        ident: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        eq: syn!(Eq) >>
        value: syn!(Expr) >>
        semi: syn!(Semi) >>
        (Item {
            attrs: attrs,
            node: ItemConst {
                vis: vis,
                const_token: const_,
                ident: ident,
                colon_token: colon,
                ty: Box::new(ty),
                eq_token: eq,
                expr: Box::new(value),
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
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(Delimited::parse_terminated) >>
        ret: syn!(FunctionRetTy) >>
        where_clause: syn!(WhereClause) >>
        inner_attrs_stmts: braces!(tuple!(
            many0!(call!(Attribute::parse_inner)),
            call!(Block::parse_within)
        )) >>
        (Item {
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend((inner_attrs_stmts.0).0);
                attrs
            },
            node: ItemFn {
                vis: vis,
                constness: constness,
                unsafety: unsafety,
                abi: abi,
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
                ident: ident,
                block: Box::new(Block {
                    brace_token: inner_attrs_stmts.1,
                    stmts: (inner_attrs_stmts.0).1,
                }),
            }.into(),
        })
    ));

    impl Synom for FnArg {
        named!(parse -> Self, alt!(
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
        ));
    }

    named!(item_mod -> Item, do_parse!(
        outer_attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        mod_: syn!(Mod) >>
        ident: syn!(Ident) >>
        content_semi: alt!(
            syn!(Semi) => {|semi| (
                Vec::new(),
                None,
                Some(semi),
            )}
            |
            braces!(
                tuple!(
                    many0!(call!(Attribute::parse_inner)),
                    many0!(syn!(Item))
                )
            ) => {|((inner_attrs, items), brace)| (
                inner_attrs,
                Some((brace, items)),
                None,
            )}
        ) >>
        (Item {
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend(content_semi.0);
                attrs
            },
            node: ItemMod {
                vis: vis,
                mod_token: mod_,
                ident: ident,
                content: content_semi.1,
                semi: content_semi.2,
            }.into(),
        })
    ));

    named!(item_foreign_mod -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        abi: syn!(Abi) >>
        items: braces!(many0!(syn!(ForeignItem))) >>
        (Item {
            attrs: attrs,
            node: ItemForeignMod {
                abi: abi,
                brace_token: items.1,
                items: items.0,
            }.into(),
        })
    ));

    impl Synom for ForeignItem {
        named!(parse -> Self, alt!(
            foreign_fn
            |
            foreign_static
        ));
    }

    named!(foreign_fn -> ForeignItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        fn_: syn!(Fn_) >>
        ident: syn!(Ident) >>
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
                ident: ident,
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
        ident: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        semi: syn!(Semi) >>
        (ForeignItem {
            ident: ident,
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
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        where_clause: syn!(WhereClause) >>
        eq: syn!(Eq) >>
        ty: syn!(Ty) >>
        semi: syn!(Semi) >>
        (Item {
            attrs: attrs,
            node: ItemTy {
                vis: vis,
                type_token: type_,
                ident: ident,
                generics: Generics {
                    where_clause: where_clause,
                    ..generics
                },
                eq_token: eq,
                ty: Box::new(ty),
                semi_token: semi,
            }.into(),
        })
    ));

    named!(item_struct_or_enum -> Item, map!(syn!(DeriveInput), Into::into));

    named!(item_union -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        union_: syn!(Union) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        where_clause: syn!(WhereClause) >>
        fields: braces!(call!(Delimited::parse_terminated_with,
                              Field::parse_struct)) >>
        (Item {
            attrs: attrs,
            node: ItemUnion {
                vis: vis,
                union_token: union_,
                ident: ident,
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
                data: VariantData::Struct(fields.0, fields.1),
            }.into(),
        })
    ));

    named!(item_trait -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        unsafety: syn!(Unsafety) >>
        trait_: syn!(Trait) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        colon: option!(syn!(Colon)) >>
        bounds: cond!(colon.is_some(),
            call!(Delimited::parse_separated_nonempty)
        ) >>
        where_clause: syn!(WhereClause) >>
        body: braces!(many0!(syn!(TraitItem))) >>
        (Item {
            attrs: attrs,
            node: ItemTrait {
                vis: vis,
                unsafety: unsafety,
                trait_token: trait_,
                ident: ident,
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
                colon_token: colon,
                supertraits: bounds.unwrap_or_default(),
                brace_token: body.1,
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
            attrs: attrs,
            node: ItemDefaultImpl {
                unsafety: unsafety,
                impl_token: impl_,
                path: path,
                for_token: for_,
                dot2_token: dot2,
                brace_token: braces.1,
            }.into(),
        })
    ));

    impl Synom for TraitItem {
        named!(parse -> Self, alt!(
            trait_item_const
            |
            trait_item_method
            |
            trait_item_type
            |
            trait_item_mac
        ));
    }

    named!(trait_item_const -> TraitItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        const_: syn!(Const) >>
        ident: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        default: option!(tuple!(syn!(Eq), syn!(Expr))) >>
        semi: syn!(Semi) >>
        (TraitItem {
            attrs: attrs,
            node: TraitItemConst {
                const_token: const_,
                ident: ident,
                colon_token: colon,
                ty: ty,
                default: default,
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
        ident: syn!(Ident) >>
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
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend(inner_attrs);
                    attrs
                },
                node: TraitItemMethod {
                    sig: MethodSig {
                        constness: constness,
                        unsafety: unsafety,
                        abi: abi,
                        ident: ident,
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
                    semi_token: semi,
                }.into(),
            }
        })
    ));

    named!(trait_item_type -> TraitItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        type_: syn!(Type) >>
        ident: syn!(Ident) >>
        colon: option!(syn!(Colon)) >>
        bounds: cond!(colon.is_some(),
            call!(Delimited::parse_separated_nonempty)
        ) >>
        default: option!(tuple!(syn!(Eq), syn!(Ty))) >>
        semi: syn!(Semi) >>
        (TraitItem {
            attrs: attrs,
            node: TraitItemType {
                type_token: type_,
                ident: ident,
                colon_token: colon,
                bounds: bounds.unwrap_or_default(),
                default: default,
                semi_token: semi,
            }.into(),
        })
    ));

    named!(trait_item_mac -> TraitItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        mac: syn!(Mac) >>
        cond!(!mac.is_braced(), syn!(Semi)) >>
        (TraitItem {
            attrs: attrs,
            node: TraitItemKind::Macro(mac),
        })
    ));

    named!(item_impl -> Item, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        defaultness: syn!(Defaultness) >>
        unsafety: syn!(Unsafety) >>
        impl_: syn!(Impl) >>
        generics: syn!(Generics) >>
        polarity_path: alt!(
            do_parse!(
                polarity: syn!(ImplPolarity) >>
                path: syn!(Path) >>
                for_: syn!(For) >>
                (Some((polarity, path, for_)))
            )
            |
            epsilon!() => { |_| None }
        ) >>
        self_ty: syn!(Ty) >>
        where_clause: syn!(WhereClause) >>
        body: braces!(many0!(syn!(ImplItem))) >>
        (Item {
            attrs: attrs,
            node: ItemImpl {
                defaultness: defaultness,
                unsafety: unsafety,
                impl_token: impl_,
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
                trait_: polarity_path,
                self_ty: Box::new(self_ty),
                brace_token: body.1,
                items: body.0,
            }.into(),
        })
    ));

    impl Synom for ImplItem {
        named!(parse -> Self, alt!(
            impl_item_const
            |
            impl_item_method
            |
            impl_item_type
            |
            impl_item_mac
        ));
    }

    named!(impl_item_const -> ImplItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        vis: syn!(Visibility) >>
        defaultness: syn!(Defaultness) >>
        const_: syn!(Const) >>
        ident: syn!(Ident) >>
        colon: syn!(Colon) >>
        ty: syn!(Ty) >>
        eq: syn!(Eq) >>
        value: syn!(Expr) >>
        semi: syn!(Semi) >>
        (ImplItem {
            attrs: attrs,
            node: ImplItemConst {
                vis: vis,
                defaultness: defaultness,
                const_token: const_,
                ident: ident,
                colon_token: colon,
                ty: ty,
                eq_token: eq,
                expr: value,
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
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(call!(Delimited::parse_terminated)) >>
        ret: syn!(FunctionRetTy) >>
        where_clause: syn!(WhereClause) >>
        inner_attrs_stmts: braces!(tuple!(
            many0!(call!(Attribute::parse_inner)),
            call!(Block::parse_within)
        )) >>
        (ImplItem {
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend((inner_attrs_stmts.0).0);
                attrs
            },
            node: ImplItemMethod {
                vis: vis,
                defaultness: defaultness,
                sig: MethodSig {
                    constness: constness,
                    unsafety: unsafety,
                    abi: abi,
                    ident: ident,
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
        ident: syn!(Ident) >>
        eq: syn!(Eq) >>
        ty: syn!(Ty) >>
        semi: syn!(Semi) >>
        (ImplItem {
            attrs: attrs,
            node: ImplItemType {
                vis: vis,
                defaultness: defaultness,
                type_token: type_,
                ident: ident,
                eq_token: eq,
                ty: ty,
                semi_token: semi,
            }.into(),
        })
    ));

    named!(impl_item_mac -> ImplItem, do_parse!(
        attrs: many0!(call!(Attribute::parse_outer)) >>
        mac: syn!(Mac) >>
        cond!(!mac.is_braced(), syn!(Semi)) >>
        (ImplItem {
            attrs: attrs,
            node: ImplItemKind::Macro(mac),
        })
    ));

    impl Synom for ImplPolarity {
        named!(parse -> Self, alt!(
            syn!(Bang) => { ImplPolarity::Negative }
            |
            epsilon!() => { |_| ImplPolarity::Positive }
        ));
    }

    impl Synom for Constness {
        named!(parse -> Self, alt!(
            syn!(Const) => { Constness::Const }
            |
            epsilon!() => { |_| Constness::NotConst }
        ));
    }

    impl Synom for Defaultness {
        named!(parse -> Self, alt!(
            syn!(Default_) => { Defaultness::Default }
            |
            epsilon!() => { |_| Defaultness::Final }
        ));
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
                    item.vis.to_tokens(tokens);
                    item.extern_token.to_tokens(tokens);
                    item.crate_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    if let Some((ref as_token, ref rename)) = item.rename {
                        as_token.to_tokens(tokens);
                        rename.to_tokens(tokens);
                    }
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Use(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.use_token.to_tokens(tokens);
                    item.path.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Static(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.static_token.to_tokens(tokens);
                    item.mutbl.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.expr.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Const(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.const_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.expr.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Fn(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.constness.to_tokens(tokens);
                    item.unsafety.to_tokens(tokens);
                    item.abi.to_tokens(tokens);
                    NamedDecl(&item.decl, item.ident).to_tokens(tokens);
                    item.block.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(self.attrs.inner());
                        tokens.append_all(&item.block.stmts);
                    });
                }
                ItemKind::Mod(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.mod_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    if let Some((ref brace, ref items)) = item.content {
                        brace.surround(tokens, |tokens| {
                            tokens.append_all(self.attrs.inner());
                            tokens.append_all(items);
                        });
                    } else {
                        TokensOrDefault(&item.semi).to_tokens(tokens);
                    }
                }
                ItemKind::ForeignMod(ref item) => {
                    item.abi.to_tokens(tokens);
                    item.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(&item.items);
                    });
                }
                ItemKind::Ty(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.type_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ItemKind::Enum(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.enum_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    item.brace_token.surround(tokens, |tokens| {
                        item.variants.to_tokens(tokens);
                    });
                }
                ItemKind::Struct(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.struct_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    match item.data {
                        VariantData::Struct(..) => {
                            item.generics.where_clause.to_tokens(tokens);
                            item.data.to_tokens(tokens);
                        }
                        VariantData::Tuple(..) => {
                            item.data.to_tokens(tokens);
                            item.generics.where_clause.to_tokens(tokens);
                            TokensOrDefault(&item.semi_token).to_tokens(tokens);
                        }
                        VariantData::Unit => {
                            item.generics.where_clause.to_tokens(tokens);
                            TokensOrDefault(&item.semi_token).to_tokens(tokens);
                        }
                    }
                }
                ItemKind::Union(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.union_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    // XXX: Should we handle / complain when using a
                    // non-VariantData::Struct Variant in Union?
                    item.data.to_tokens(tokens);
                }
                ItemKind::Trait(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.unsafety.to_tokens(tokens);
                    item.trait_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    if !item.supertraits.is_empty() {
                        TokensOrDefault(&item.colon_token).to_tokens(tokens);
                        item.supertraits.to_tokens(tokens);
                    }
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
                    item.defaultness.to_tokens(tokens);
                    item.unsafety.to_tokens(tokens);
                    item.impl_token.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    if let Some((ref polarity, ref path, ref for_token)) = item.trait_ {
                        polarity.to_tokens(tokens);
                        path.to_tokens(tokens);
                        for_token.to_tokens(tokens);
                    }
                    item.self_ty.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    item.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(&item.items);
                    });
                }
                ItemKind::Mac(ref mac) => {
                    mac.path.to_tokens(tokens);
                    mac.bang_token.to_tokens(tokens);
                    mac.ident.to_tokens(tokens);
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
            if self.rename.is_some() {
                TokensOrDefault(&self.as_token).to_tokens(tokens);
                self.rename.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for PathGlob {
        fn to_tokens(&self, tokens: &mut Tokens) {
            if self.path.segments.len() > 0 {
                self.path.to_tokens(tokens);
                TokensOrDefault(&self.colon2_token).to_tokens(tokens);
            }
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
            if self.rename.is_some() {
                TokensOrDefault(&self.as_token).to_tokens(tokens);
                self.rename.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for TraitItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            match self.node {
                TraitItemKind::Const(ref item) => {
                    item.const_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    if let Some((ref eq_token, ref default)) = item.default {
                        eq_token.to_tokens(tokens);
                        default.to_tokens(tokens);
                    }
                    item.semi_token.to_tokens(tokens);
                }
                TraitItemKind::Method(ref item) => {
                    item.sig.to_tokens(tokens);
                    match item.default {
                        Some(ref block) => {
                            block.brace_token.surround(tokens, |tokens| {
                                tokens.append_all(self.attrs.inner());
                                tokens.append_all(&block.stmts);
                            });
                        }
                        None => {
                            TokensOrDefault(&item.semi_token).to_tokens(tokens);
                        }
                    }
                }
                TraitItemKind::Type(ref item) => {
                    item.type_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    if !item.bounds.is_empty() {
                        TokensOrDefault(&item.colon_token).to_tokens(tokens);
                        item.bounds.to_tokens(tokens);
                    }
                    if let Some((ref eq_token, ref default)) = item.default {
                        eq_token.to_tokens(tokens);
                        default.to_tokens(tokens);
                    }
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
            match self.node {
                ImplItemKind::Const(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.defaultness.to_tokens(tokens);
                    item.const_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.colon_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.expr.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ImplItemKind::Method(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.defaultness.to_tokens(tokens);
                    item.sig.to_tokens(tokens);
                    item.block.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(self.attrs.inner());
                        tokens.append_all(&item.block.stmts);
                    });
                }
                ImplItemKind::Type(ref item) => {
                    item.vis.to_tokens(tokens);
                    item.defaultness.to_tokens(tokens);
                    item.type_token.to_tokens(tokens);
                    item.ident.to_tokens(tokens);
                    item.eq_token.to_tokens(tokens);
                    item.ty.to_tokens(tokens);
                    item.semi_token.to_tokens(tokens);
                }
                ImplItemKind::Macro(ref mac) => {
                    mac.to_tokens(tokens);
                    if !mac.is_braced() {
                        // FIXME needs a span
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
                    NamedDecl(&item.decl, self.ident).to_tokens(tokens)
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

    impl ToTokens for MethodSig {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.constness.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.abi.to_tokens(tokens);
            NamedDecl(&self.decl, self.ident).to_tokens(tokens);
        }
    }

    struct NamedDecl<'a>(&'a FnDecl, Ident);

    impl<'a> ToTokens for NamedDecl<'a> {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.0.fn_token.to_tokens(tokens);
            self.1.to_tokens(tokens);
            self.0.generics.to_tokens(tokens);
            self.0.paren_token.surround(tokens, |tokens| {
                self.0.inputs.to_tokens(tokens);

                if self.0.variadic {
                    if !self.0.inputs.empty_or_trailing() {
                        tokens::Comma::default().to_tokens(tokens);
                    }
                    TokensOrDefault(&self.0.dot_tokens).to_tokens(tokens);
                }
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
