use super::*;

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
            pub original: Option<Ident>,
        }),
        /// A use declaration (`use` or `pub use`) item.
        ///
        /// E.g. `use foo;`, `use foo::bar;` or `use foo::bar as FooBar;`
        pub Use(ItemUse {
            pub path: Box<ViewPath>,
        }),
        /// A static item (`static` or `pub static`).
        ///
        /// E.g. `static FOO: i32 = 42;` or `static FOO: &'static str = "bar";`
        pub Static(ItemStatic {
            pub ty: Box<Ty>,
            pub mutbl: Mutability,
            pub expr: Box<Expr>,
        }),
        /// A constant item (`const` or `pub const`).
        ///
        /// E.g. `const FOO: i32 = 42;`
        pub Const(ItemConst {
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
            pub generics: Generics,
            pub block: Box<Block>,
        }),
        /// A module declaration (`mod` or `pub mod`).
        ///
        /// E.g. `mod foo;` or `mod foo { .. }`
        pub Mod(ItemMod {
            pub items: Option<Vec<Item>>,
        }),
        /// An external module (`extern` or `pub extern`).
        ///
        /// E.g. `extern {}` or `extern "C" {}`
        pub ForeignMod(ItemForeignMod),
        /// A type alias (`type` or `pub type`).
        ///
        /// E.g. `type Foo = Bar<u8>;`
        pub Ty(ItemTy {
            pub ty: Box<Ty>,
            pub generics: Generics,
        }),
        /// An enum definition (`enum` or `pub enum`).
        ///
        /// E.g. `enum Foo<A, B> { C<A>, D<B> }`
        pub Enum(ItemEnum {
            pub variants: Vec<Variant>,
            pub generics: Generics,
        }),
        /// A struct definition (`struct` or `pub struct`).
        ///
        /// E.g. `struct Foo<A> { x: A }`
        pub Struct(ItemStruct {
            pub data: VariantData,
            pub generics: Generics,
        }),
        /// A union definition (`union` or `pub union`).
        ///
        /// E.g. `union Foo<A, B> { x: A, y: B }`
        pub Union(ItemUnion {
            pub data: VariantData,
            pub generics: Generics,
        }),
        /// A Trait declaration (`trait` or `pub trait`).
        ///
        /// E.g. `trait Foo { .. }` or `trait Foo<T> { .. }`
        pub Trait(ItemTrait {
            pub unsafety: Unsafety,
            pub generics: Generics,
            pub supertraits: Vec<TyParamBound>,
            pub items: Vec<TraitItem>,
        }),
        /// Default trait implementation.
        ///
        /// E.g. `impl Trait for .. {}` or `impl<T> Trait<T> for .. {}`
        pub DefaultImpl(ItemDefaultImpl {
            pub unsafety: Unsafety,
            pub path: Path,
        }),
        /// An implementation.
        ///
        /// E.g. `impl<A> Foo<A> { .. }` or `impl<A> Trait for Foo<A> { .. }`
        pub Impl(ItemImpl {
            pub unsafety: Unsafety,
            pub polarity: ImplPolarity,
            pub generics: Generics,
            pub trait_: Option<Path>, // (optional) trait this impl implements
            pub self_ty: Box<Ty>, // self
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
                Body::Enum(variants) => {
                    ItemEnum {
                        variants: variants,
                        generics: input.generics,
                    }.into()
                }
                Body::Struct(variant_data) => {
                    ItemStruct {
                        data: variant_data,
                        generics: input.generics,
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
            pub rename: Option<Ident>,
        }),

        /// `foo::bar::*`
        pub Glob(PathGlob {
            pub path: Path,
        }),

        /// `foo::bar::{a, b, c}`
        pub List(PathList {
            pub path: Path,
            pub items: Vec<PathListItem>,
        }),
    }
}

ast_struct! {
    pub struct PathListItem {
        pub name: Ident,
        /// renamed in list, e.g. `use foo::{bar as baz};`
        pub rename: Option<Ident>,
    }
}

ast_enum! {
    #[derive(Copy)]
    pub enum Constness {
        Const,
        NotConst,
    }
}

ast_enum! {
    #[derive(Copy)]
    pub enum Defaultness {
        Default,
        Final,
    }
}

ast_struct! {
    /// Foreign module declaration.
    ///
    /// E.g. `extern { .. }` or `extern "C" { .. }`
    pub struct ItemForeignMod {
        pub abi: Abi,
        pub items: Vec<ForeignItem>,
    }
}

ast_struct! {
    pub struct ForeignItem {
        pub ident: Ident,
        pub attrs: Vec<Attribute>,
        pub node: ForeignItemKind,
        pub vis: Visibility,
    }
}

ast_enum_of_structs! {
    /// An item within an `extern` block
    pub enum ForeignItemKind {
        /// A foreign function
        pub Fn(ForeignItemFn {
            pub decl: Box<FnDecl>,
            pub generics: Generics,
        }),
        /// A foreign static item (`static ext: u8`)
        pub Static(ForeignItemStatic {
            pub ty: Box<Ty>,
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
            pub ty: Ty,
            pub default: Option<Expr>,
        }),
        pub Method(TraitItemMethod {
            pub sig: MethodSig,
            pub default: Option<Block>,
        }),
        pub Type(TraitItemType {
            pub bounds: Vec<TyParamBound>,
            pub default: Option<Ty>,
        }),
        pub Macro(Mac),
    }

    do_not_generate_to_tokens
}

ast_enum! {
    #[derive(Copy)]
    pub enum ImplPolarity {
        /// `impl Trait for Type`
        Positive,
        /// `impl !Trait for Type`
        Negative,
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
            pub ty: Ty,
            pub expr: Expr,
        }),
        pub Method(ImplItemMethod {
            pub sig: MethodSig,
            pub block: Block,
        }),
        pub Type(ImplItemType {
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
        pub generics: Generics,
    }
}

ast_struct! {
    /// Header (not the body) of a function declaration.
    ///
    /// E.g. `fn foo(bar: baz)`
    pub struct FnDecl {
        pub inputs: Vec<FnArg>,
        pub output: FunctionRetTy,
        pub variadic: bool,
    }
}

ast_enum_of_structs! {
    /// An argument in a function header.
    ///
    /// E.g. `bar: usize` as in `fn foo(bar: usize)`
    pub enum FnArg {
        pub SelfRef(ArgSelfRef {
            pub lifetime: Option<Lifetime>,
            pub mutbl: Mutability,
        }),
        pub SelfValue(ArgSelf {
            pub mutbl: Mutability,
        }),
        pub Captured(ArgCaptured {
            pub pat: Pat,
            pub ty: Ty,
        }),
        pub Ignored(Ty),
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use {Block, DelimToken, FunctionRetTy, Generics, Ident, Mac, Path, TokenTree, VariantData,
         Visibility};
    use attr::parsing::{inner_attr, outer_attr};
    use data::parsing::{struct_like_body, visibility};
    use expr::parsing::{expr, pat, within_block};
    use generics::parsing::{generics, lifetime, ty_param_bound, where_clause};
    use ident::parsing::ident;
    use mac::parsing::delimited;
    use derive::{Body, DeriveInput};
    use derive::parsing::derive_input;
    use ty::parsing::{abi, mutability, path, ty, unsafety};

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
        cond!(match body.delim {
            DelimToken::Paren | DelimToken::Bracket => true,
            DelimToken::Brace => false,
        }, punct!(";")) >>
        (Item {
            ident: name.unwrap_or_else(|| Ident::new("")),
            vis: Visibility::Inherited,
            attrs: attrs,
            node: ItemKind::Mac(Mac {
                path: what,
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
                node: ItemExternCrate { original: original_name }.into(),
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
            node: ItemUse { path: Box::new(what) }.into(),
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
        (PathSimple { path: path, rename: rename }.into())
    ));

    named!(view_path_glob -> ViewPath, do_parse!(
        path: path >>
        punct!("::") >>
        punct!("*") >>
        (PathGlob { path: path }.into())
    ));

    named!(view_path_list -> ViewPath, do_parse!(
        path: path >>
        punct!("::") >>
        punct!("{") >>
        items: terminated_list!(punct!(","), path_list_item) >>
        punct!("}") >>
        (PathList { path: path, items: items }.into())
    ));

    named!(view_path_list_root -> ViewPath, do_parse!(
        global: option!(punct!("::")) >>
        punct!("{") >>
        items: terminated_list!(punct!(","), path_list_item) >>
        punct!("}") >>
        (PathList {
            path: Path {
                global: global.is_some(),
                segments: Vec::new(),
            },
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
            node: ItemConst { ty: Box::new(ty), expr: Box::new(value) }.into(),
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
        inputs: terminated_list!(punct!(","), fn_arg) >>
        punct!(")") >>
        ret: option!(preceded!(punct!("->"), ty)) >>
        where_clause: where_clause >>
        punct!("{") >>
        inner_attrs: many0!(inner_attr) >>
        stmts: within_block >>
        punct!("}") >>
        (Item {
            ident: name,
            vis: vis,
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend(inner_attrs);
                attrs
            },
            node: ItemFn {
                decl: Box::new(FnDecl {
                    inputs: inputs,
                    output: ret.map(FunctionRetTy::Ty).unwrap_or(FunctionRetTy::Default),
                    variadic: false,
                }),
                unsafety: unsafety,
                constness: constness,
                abi: abi,
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
                block: Box::new(Block {
                    stmts: stmts,
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
            (ArgSelfRef { lifetime: lt, mutbl: mutability }.into())
        )
        |
        do_parse!(
            mutability: mutability >>
            keyword!("self") >>
            not!(punct!(":")) >>
            (ArgSelf { mutbl: mutability }.into())
        )
        |
        do_parse!(
            pat: pat >>
            punct!(":") >>
            ty: ty >>
            (ArgCaptured { pat: pat, ty: ty }.into())
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
            delimited!(
                punct!("{"),
                tuple!(
                    many0!(inner_attr),
                    items
                ),
                punct!("}")
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
                node: ItemMod { items: Some(items) }.into(),
            },
            None => Item {
                ident: id,
                vis: vis,
                attrs: outer_attrs,
                node: ItemMod { items: None }.into(),
            },
        })
    ));

    named!(item_foreign_mod -> Item, do_parse!(
        attrs: many0!(outer_attr) >>
        abi: abi >>
        punct!("{") >>
        items: many0!(foreign_item) >>
        punct!("}") >>
        (Item {
            ident: "".into(),
            vis: Visibility::Inherited,
            attrs: attrs,
            node: ItemForeignMod {
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
        inputs: separated_list!(punct!(","), fn_arg) >>
        trailing_comma: option!(punct!(",")) >>
        variadic: option!(cond_reduce!(trailing_comma.is_some(), punct!("..."))) >>
        punct!(")") >>
        ret: option!(preceded!(punct!("->"), ty)) >>
        where_clause: where_clause >>
        punct!(";") >>
        (ForeignItem {
            ident: name,
            attrs: attrs,
            node: ForeignItemFn {
                decl: Box::new(FnDecl {
                    inputs: inputs,
                    output: ret.map(FunctionRetTy::Ty).unwrap_or(FunctionRetTy::Default),
                    variadic: variadic.is_some(),
                }),
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
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
            node: ForeignItemStatic { ty: Box::new(ty), mutbl: mutability }.into(),
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
                Body::Enum(variants) => {
                    ItemEnum { variants: variants, generics: def.generics }.into()
                }
                Body::Struct(variant_data) => {
                    ItemStruct { data: variant_data, generics: def.generics }.into()
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
                data: VariantData::Struct(fields),
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
        bounds: opt_vec!(preceded!(
            punct!(":"),
            separated_nonempty_list!(punct!("+"), ty_param_bound)
        )) >>
        where_clause: where_clause >>
        punct!("{") >>
        body: many0!(trait_item) >>
        punct!("}") >>
        (Item {
            ident: id,
            vis: vis,
            attrs: attrs,
            node: ItemTrait {
                unsafety: unsafety,
                generics: Generics {
                    where_clause: where_clause,
                    .. generics
                },
                supertraits: bounds,
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
        punct!("{") >>
        punct!("}") >>
        (Item {
            ident: "".into(),
            vis: Visibility::Inherited,
            attrs: attrs,
            node: ItemDefaultImpl { unsafety: unsafety, path: path }.into(),
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
            node: TraitItemConst { ty: ty, default: value }.into(),
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
        inputs: terminated_list!(punct!(","), fn_arg) >>
        punct!(")") >>
        ret: option!(preceded!(punct!("->"), ty)) >>
        where_clause: where_clause >>
        body: option!(delimited!(
            punct!("{"),
            tuple!(many0!(inner_attr), within_block),
            punct!("}")
        )) >>
        cond!(body.is_none(), punct!(";")) >>
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
                    sig: MethodSig {
                        unsafety: unsafety,
                        constness: constness,
                        abi: abi,
                        decl: FnDecl {
                            inputs: inputs,
                            output: ret.map(FunctionRetTy::Ty).unwrap_or(FunctionRetTy::Default),
                            variadic: false,
                        },
                        generics: Generics {
                            where_clause: where_clause,
                            .. generics
                        },
                    },
                    default: stmts.map(|stmts| Block { stmts: stmts }),
                }.into(),
            }
        })
    ));

    named!(trait_item_type -> TraitItem, do_parse!(
        attrs: many0!(outer_attr) >>
        keyword!("type") >>
        id: ident >>
        bounds: opt_vec!(preceded!(
            punct!(":"),
            separated_nonempty_list!(punct!("+"), ty_param_bound)
        )) >>
        default: option!(preceded!(punct!("="), ty)) >>
        punct!(";") >>
        (TraitItem {
            ident: id,
            attrs: attrs,
            node: TraitItemType { bounds: bounds, default: default }.into(),
        })
    ));

    named!(trait_item_mac -> TraitItem, do_parse!(
        attrs: many0!(outer_attr) >>
        what: path >>
        punct!("!") >>
        body: delimited >>
        cond!(match body.delim {
            DelimToken::Paren | DelimToken::Bracket => true,
            DelimToken::Brace => false,
        }, punct!(";")) >>
        (TraitItem {
            ident: Ident::new(""),
            attrs: attrs,
            node: TraitItemKind::Macro(Mac {
                path: what,
                tts: vec![TokenTree::Delimited(body)],
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
        punct!("{") >>
        body: many0!(impl_item) >>
        punct!("}") >>
        (Item {
            ident: "".into(),
            vis: Visibility::Inherited,
            attrs: attrs,
            node: ItemImpl {
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
            node: ImplItemConst { ty: ty, expr: value}.into(),
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
        inputs: terminated_list!(punct!(","), fn_arg) >>
        punct!(")") >>
        ret: option!(preceded!(punct!("->"), ty)) >>
        where_clause: where_clause >>
        punct!("{") >>
        inner_attrs: many0!(inner_attr) >>
        stmts: within_block >>
        punct!("}") >>
        (ImplItem {
            ident: name,
            vis: vis,
            defaultness: defaultness,
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend(inner_attrs);
                attrs
            },
            node: ImplItemMethod {
                sig: MethodSig {
                    unsafety: unsafety,
                    constness: constness,
                    abi: abi,
                    decl: FnDecl {
                        inputs: inputs,
                        output: ret.map(FunctionRetTy::Ty).unwrap_or(FunctionRetTy::Default),
                        variadic: false,
                    },
                    generics: Generics {
                        where_clause: where_clause,
                        .. generics
                    },
                },
                block: Block {
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
            node: ImplItemType { ty: ty }.into(),
        })
    ));

    named!(impl_item_macro -> ImplItem, do_parse!(
        attrs: many0!(outer_attr) >>
        what: path >>
        punct!("!") >>
        body: delimited >>
        cond!(match body.delim {
            DelimToken::Paren | DelimToken::Bracket => true,
            DelimToken::Brace => false,
        }, punct!(";")) >>
        (ImplItem {
            ident: Ident::new(""),
            vis: Visibility::Inherited,
            defaultness: Defaultness::Final,
            attrs: attrs,
            node: ImplItemKind::Macro(Mac {
                path: what,
                tts: vec![TokenTree::Delimited(body)],
            }),
        })
    ));

    named!(impl_polarity -> ImplPolarity, alt!(
        punct!("!") => { |_| ImplPolarity::Negative }
        |
        epsilon!() => { |_| ImplPolarity::Positive }
    ));

    named!(constness -> Constness, alt!(
        keyword!("const") => { |_| Constness::Const }
        |
        epsilon!() => { |_| Constness::NotConst }
    ));

    named!(defaultness -> Defaultness, alt!(
        keyword!("default") => { |_| Defaultness::Default }
        |
        epsilon!() => { |_| Defaultness::Final }
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use {Delimited, DelimToken, FunctionRetTy, TokenTree};
    use attr::FilterAttrs;
    use data::VariantData;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Item {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            match self.node {
                ItemKind::ExternCrate(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("extern");
                    tokens.append("crate");
                    if let Some(ref original) = item.original {
                        original.to_tokens(tokens);
                        tokens.append("as");
                    }
                    self.ident.to_tokens(tokens);
                    tokens.append(";");
                }
                ItemKind::Use(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("use");
                    item.path.to_tokens(tokens);
                    tokens.append(";");
                }
                ItemKind::Static(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("static");
                    item.mutbl.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    tokens.append(":");
                    item.ty.to_tokens(tokens);
                    tokens.append("=");
                    item.expr.to_tokens(tokens);
                    tokens.append(";");
                }
                ItemKind::Const(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("const");
                    self.ident.to_tokens(tokens);
                    tokens.append(":");
                    item.ty.to_tokens(tokens);
                    tokens.append("=");
                    item.expr.to_tokens(tokens);
                    tokens.append(";");
                }
                ItemKind::Fn(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.constness.to_tokens(tokens);
                    item.unsafety.to_tokens(tokens);
                    item.abi.to_tokens(tokens);
                    tokens.append("fn");
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    tokens.append("(");
                    tokens.append_separated(&item.decl.inputs, ",");
                    tokens.append(")");
                    if let FunctionRetTy::Ty(ref ty) = item.decl.output {
                        tokens.append("->");
                        ty.to_tokens(tokens);
                    }
                    item.generics.where_clause.to_tokens(tokens);
                    tokens.append("{");
                    tokens.append_all(self.attrs.inner());
                    tokens.append_all(&item.block.stmts);
                    tokens.append("}");
                }
                ItemKind::Mod(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("mod");
                    self.ident.to_tokens(tokens);
                    match item.items {
                        Some(ref items) => {
                            tokens.append("{");
                            tokens.append_all(self.attrs.inner());
                            tokens.append_all(items);
                            tokens.append("}");
                        }
                        None => tokens.append(";"),
                    }
                }
                ItemKind::ForeignMod(ref foreign_mod) => {
                    self.vis.to_tokens(tokens);
                    foreign_mod.abi.to_tokens(tokens);
                    tokens.append("{");
                    tokens.append_all(&foreign_mod.items);
                    tokens.append("}");
                }
                ItemKind::Ty(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("type");
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    tokens.append("=");
                    item.ty.to_tokens(tokens);
                    tokens.append(";");
                }
                ItemKind::Enum(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("enum");
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    tokens.append("{");
                    for variant in &item.variants {
                        variant.to_tokens(tokens);
                        tokens.append(",");
                    }
                    tokens.append("}");
                }
                ItemKind::Struct(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("struct");
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    match item.data {
                        VariantData::Struct(_) => {
                            item.generics.where_clause.to_tokens(tokens);
                            item.data.to_tokens(tokens);
                            // no semicolon
                        }
                        VariantData::Tuple(_) => {
                            item.data.to_tokens(tokens);
                            item.generics.where_clause.to_tokens(tokens);
                            tokens.append(";");
                        }
                        VariantData::Unit => {
                            item.generics.where_clause.to_tokens(tokens);
                            tokens.append(";");
                        }
                    }
                }
                ItemKind::Union(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("union");
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    item.data.to_tokens(tokens);
                }
                ItemKind::Trait(ref item) => {
                    self.vis.to_tokens(tokens);
                    item.unsafety.to_tokens(tokens);
                    tokens.append("trait");
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    if !item.supertraits.is_empty() {
                        tokens.append(":");
                        tokens.append_separated(&item.supertraits, "+");
                    }
                    item.generics.where_clause.to_tokens(tokens);
                    tokens.append("{");
                    tokens.append_all(&item.items);
                    tokens.append("}");
                }
                ItemKind::DefaultImpl(ref item) => {
                    item.unsafety.to_tokens(tokens);
                    tokens.append("impl");
                    item.path.to_tokens(tokens);
                    tokens.append("for");
                    tokens.append("..");
                    tokens.append("{");
                    tokens.append("}");
                }
                ItemKind::Impl(ref item) => {
                    item.unsafety.to_tokens(tokens);
                    tokens.append("impl");
                    item.generics.to_tokens(tokens);
                    if let Some(ref path) = item.trait_ {
                        item.polarity.to_tokens(tokens);
                        path.to_tokens(tokens);
                        tokens.append("for");
                    }
                    item.self_ty.to_tokens(tokens);
                    item.generics.where_clause.to_tokens(tokens);
                    tokens.append("{");
                    tokens.append_all(&item.items);
                    tokens.append("}");
                }
                ItemKind::Mac(ref mac) => {
                    mac.path.to_tokens(tokens);
                    tokens.append("!");
                    self.ident.to_tokens(tokens);
                    for tt in &mac.tts {
                        tt.to_tokens(tokens);
                    }
                    match mac.tts.last() {
                        Some(&TokenTree::Delimited(Delimited { delim: DelimToken::Brace, .. })) => {
                            // no semicolon
                        }
                        _ => tokens.append(";"),
                    }
                }
            }
        }
    }

    impl ToTokens for PathSimple {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            if let Some(ref rename) = self.rename {
                tokens.append("as");
                rename.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for PathGlob {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            tokens.append("::");
            tokens.append("*");
        }
    }

    impl ToTokens for PathList {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.path.to_tokens(tokens);
            if self.path.global || !self.path.segments.is_empty() {
                tokens.append("::");
            }
            tokens.append("{");
            tokens.append_separated(&self.items, ",");
            tokens.append("}");
        }
    }

    impl ToTokens for PathListItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.name.to_tokens(tokens);
            if let Some(ref rename) = self.rename {
                tokens.append("as");
                rename.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for TraitItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            match self.node {
                TraitItemKind::Const(ref item) => {
                    tokens.append("const");
                    self.ident.to_tokens(tokens);
                    tokens.append(":");
                    item.ty.to_tokens(tokens);
                    if let Some(ref expr) = item.default {
                        tokens.append("=");
                        expr.to_tokens(tokens);
                    }
                    tokens.append(";");
                }
                TraitItemKind::Method(ref item) => {
                    item.sig.constness.to_tokens(tokens);
                    item.sig.unsafety.to_tokens(tokens);
                    item.sig.abi.to_tokens(tokens);
                    tokens.append("fn");
                    self.ident.to_tokens(tokens);
                    item.sig.generics.to_tokens(tokens);
                    tokens.append("(");
                    tokens.append_separated(&item.sig.decl.inputs, ",");
                    tokens.append(")");
                    if let FunctionRetTy::Ty(ref ty) = item.sig.decl.output {
                        tokens.append("->");
                        ty.to_tokens(tokens);
                    }
                    item.sig.generics.where_clause.to_tokens(tokens);
                    match item.default {
                        Some(ref block) => {
                            tokens.append("{");
                            tokens.append_all(self.attrs.inner());
                            tokens.append_all(&block.stmts);
                            tokens.append("}");
                        }
                        None => tokens.append(";"),
                    }
                }
                TraitItemKind::Type(ref item) => {
                    tokens.append("type");
                    self.ident.to_tokens(tokens);
                    if !item.bounds.is_empty() {
                        tokens.append(":");
                        tokens.append_separated(&item.bounds, "+");
                    }
                    if let Some(ref default) = item.default {
                        tokens.append("=");
                        default.to_tokens(tokens);
                    }
                    tokens.append(";");
                }
                TraitItemKind::Macro(ref mac) => {
                    mac.to_tokens(tokens);
                    match mac.tts.last() {
                        Some(&TokenTree::Delimited(Delimited { delim: DelimToken::Brace, .. })) => {
                            // no semicolon
                        }
                        _ => tokens.append(";"),
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
                    self.vis.to_tokens(tokens);
                    self.defaultness.to_tokens(tokens);
                    tokens.append("const");
                    self.ident.to_tokens(tokens);
                    tokens.append(":");
                    item.ty.to_tokens(tokens);
                    tokens.append("=");
                    item.expr.to_tokens(tokens);
                    tokens.append(";");
                }
                ImplItemKind::Method(ref item) => {
                    self.vis.to_tokens(tokens);
                    self.defaultness.to_tokens(tokens);
                    item.sig.constness.to_tokens(tokens);
                    item.sig.unsafety.to_tokens(tokens);
                    item.sig.abi.to_tokens(tokens);
                    tokens.append("fn");
                    self.ident.to_tokens(tokens);
                    item.sig.generics.to_tokens(tokens);
                    tokens.append("(");
                    tokens.append_separated(&item.sig.decl.inputs, ",");
                    tokens.append(")");
                    if let FunctionRetTy::Ty(ref ty) = item.sig.decl.output {
                        tokens.append("->");
                        ty.to_tokens(tokens);
                    }
                    item.sig.generics.where_clause.to_tokens(tokens);
                    tokens.append("{");
                    tokens.append_all(self.attrs.inner());
                    tokens.append_all(&item.block.stmts);
                    tokens.append("}");
                }
                ImplItemKind::Type(ref item) => {
                    self.vis.to_tokens(tokens);
                    self.defaultness.to_tokens(tokens);
                    tokens.append("type");
                    self.ident.to_tokens(tokens);
                    tokens.append("=");
                    item.ty.to_tokens(tokens);
                    tokens.append(";");
                }
                ImplItemKind::Macro(ref mac) => {
                    mac.to_tokens(tokens);
                    match mac.tts.last() {
                        Some(&TokenTree::Delimited(Delimited { delim: DelimToken::Brace, .. })) => {
                            // no semicolon
                        }
                        _ => tokens.append(";"),
                    }
                }
            }
        }
    }

    impl ToTokens for ForeignItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            match self.node {
                ForeignItemKind::Fn(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("fn");
                    self.ident.to_tokens(tokens);
                    item.generics.to_tokens(tokens);
                    tokens.append("(");
                    tokens.append_separated(&item.decl.inputs, ",");
                    if item.decl.variadic {
                        if !item.decl.inputs.is_empty() {
                            tokens.append(",");
                        }
                        tokens.append("...");
                    }
                    tokens.append(")");
                    if let FunctionRetTy::Ty(ref ty) = item.decl.output {
                        tokens.append("->");
                        ty.to_tokens(tokens);
                    }
                    item.generics.where_clause.to_tokens(tokens);
                    tokens.append(";");
                }
                ForeignItemKind::Static(ref item) => {
                    self.vis.to_tokens(tokens);
                    tokens.append("static");
                    item.mutbl.to_tokens(tokens);
                    self.ident.to_tokens(tokens);
                    tokens.append(":");
                    item.ty.to_tokens(tokens);
                    tokens.append(";");
                }
            }
        }
    }

    impl ToTokens for ArgSelfRef {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append("&");
            self.lifetime.to_tokens(tokens);
            self.mutbl.to_tokens(tokens);
            tokens.append("self");
        }
    }

    impl ToTokens for ArgSelf {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.mutbl.to_tokens(tokens);
            tokens.append("self");
        }
    }

    impl ToTokens for ArgCaptured {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.pat.to_tokens(tokens);
            tokens.append(":");
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for Constness {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Constness::Const => tokens.append("const"),
                Constness::NotConst => {
                    // nothing
                }
            }
        }
    }

    impl ToTokens for Defaultness {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                Defaultness::Default => tokens.append("default"),
                Defaultness::Final => {
                    // nothing
                }
            }
        }
    }

    impl ToTokens for ImplPolarity {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                ImplPolarity::Negative => tokens.append("!"),
                ImplPolarity::Positive => {
                    // nothing
                }
            }
        }
    }
}
