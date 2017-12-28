use super::*;
use delimited::Delimited;
use proc_macro2::TokenTree;

#[cfg(feature = "extra-traits")]
use mac::TokenTreeHelper;
#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};

ast_enum_of_structs! {
    /// Things that can appear directly inside of a module.
    pub enum Item {
        /// An `extern crate` item, with optional original crate name.
        ///
        /// E.g. `extern crate foo` or `extern crate foo_bar as foo`
        pub ExternCrate(ItemExternCrate {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub extern_token: Token![extern],
            pub crate_token: Token![crate],
            pub ident: Ident,
            pub rename: Option<(Token![as], Ident)>,
            pub semi_token: Token![;],
        }),
        /// A use declaration (`use` or `pub use`) item.
        ///
        /// E.g. `use foo;`, `use foo::bar;` or `use foo::bar as FooBar;`
        pub Use(ItemUse {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub use_token: Token![use],
            pub leading_colon: Option<Token![::]>,
            pub prefix: Delimited<Ident, Token![::]>,
            pub tree: UseTree,
            pub semi_token: Token![;],
        }),
        /// A static item (`static` or `pub static`).
        ///
        /// E.g. `static FOO: i32 = 42;` or `static FOO: &'static str = "bar";`
        pub Static(ItemStatic {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub static_token: Token![static],
            pub mutbl: Mutability,
            pub ident: Ident,
            pub colon_token: Token![:],
            pub ty: Box<Type>,
            pub eq_token: Token![=],
            pub expr: Box<Expr>,
            pub semi_token: Token![;],
        }),
        /// A constant item (`const` or `pub const`).
        ///
        /// E.g. `const FOO: i32 = 42;`
        pub Const(ItemConst {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub const_token: Token![const],
            pub ident: Ident,
            pub colon_token: Token![:],
            pub ty: Box<Type>,
            pub eq_token: Token![=],
            pub expr: Box<Expr>,
            pub semi_token: Token![;],
        }),
        /// A function declaration (`fn` or `pub fn`).
        ///
        /// E.g. `fn foo(bar: usize) -> usize { .. }`
        pub Fn(ItemFn {
            pub attrs: Vec<Attribute>,
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
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub mod_token: Token![mod],
            pub ident: Ident,
            pub content: Option<(token::Brace, Vec<Item>)>,
            pub semi: Option<Token![;]>,
        }),
        /// An external module (`extern` or `pub extern`).
        ///
        /// E.g. `extern {}` or `extern "C" {}`
        pub ForeignMod(ItemForeignMod {
            pub attrs: Vec<Attribute>,
            pub abi: Abi,
            pub brace_token: token::Brace,
            pub items: Vec<ForeignItem>,
        }),
        /// A type alias (`type` or `pub type`).
        ///
        /// E.g. `type Foo = Bar<u8>;`
        pub Type(ItemType {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub type_token: Token![type],
            pub ident: Ident,
            pub generics: Generics,
            pub eq_token: Token![=],
            pub ty: Box<Type>,
            pub semi_token: Token![;],
        }),
        /// An enum definition (`enum` or `pub enum`).
        ///
        /// E.g. `enum Foo<A, B> { C<A>, D<B> }`
        pub Enum(ItemEnum {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub enum_token: Token![enum],
            pub ident: Ident,
            pub generics: Generics,
            pub brace_token: token::Brace,
            pub variants: Delimited<Variant, Token![,]>,
        }),
        /// A struct definition (`struct` or `pub struct`).
        ///
        /// E.g. `struct Foo<A> { x: A }`
        pub Struct(ItemStruct {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub struct_token: Token![struct],
            pub ident: Ident,
            pub generics: Generics,
            pub data: VariantData,
            pub semi_token: Option<Token![;]>,
        }),
        /// A union definition (`union` or `pub union`).
        ///
        /// E.g. `union Foo<A, B> { x: A, y: B }`
        pub Union(ItemUnion {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub union_token: Token![union],
            pub ident: Ident,
            pub generics: Generics,
            pub data: VariantData,
        }),
        /// A Trait declaration (`trait` or `pub trait`).
        ///
        /// E.g. `trait Foo { .. }` or `trait Foo<T> { .. }`
        pub Trait(ItemTrait {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub unsafety: Unsafety,
            pub auto_token: Option<Token![auto]>,
            pub trait_token: Token![trait],
            pub ident: Ident,
            pub generics: Generics,
            pub colon_token: Option<Token![:]>,
            pub supertraits: Delimited<TypeParamBound, Token![+]>,
            pub brace_token: token::Brace,
            pub items: Vec<TraitItem>,
        }),
        /// Default trait implementation.
        ///
        /// E.g. `impl Trait for .. {}` or `impl<T> Trait<T> for .. {}`
        pub DefaultImpl(ItemDefaultImpl {
            pub attrs: Vec<Attribute>,
            pub unsafety: Unsafety,
            pub impl_token: Token![impl],
            pub path: Path,
            pub for_token: Token![for],
            pub dot2_token: Token![..],
            pub brace_token: token::Brace,
        }),
        /// An implementation.
        ///
        /// E.g. `impl<A> Foo<A> { .. }` or `impl<A> Trait for Foo<A> { .. }`
        pub Impl(ItemImpl {
            pub attrs: Vec<Attribute>,
            pub defaultness: Defaultness,
            pub unsafety: Unsafety,
            pub impl_token: Token![impl],
            pub generics: Generics,
            /// Trait this impl implements.
            pub trait_: Option<(ImplPolarity, Path, Token![for])>,
            /// The Self type of the impl.
            pub self_ty: Box<Type>,
            pub brace_token: token::Brace,
            pub items: Vec<ImplItem>,
        }),
        /// A macro invocation (which includes macro definition).
        ///
        /// E.g. `macro_rules! foo { .. }` or `foo!(..)`
        pub Macro(ItemMacro {
            pub attrs: Vec<Attribute>,
            /// The `example` in `macro_rules! example { ... }`.
            pub ident: Option<Ident>,
            pub mac: Macro,
            pub semi_token: Option<Token![;]>,
        }),
        /// FIXME will need to revisit what this looks like in the AST.
        pub Macro2(ItemMacro2 #manual_extra_traits {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub macro_token: Token![macro],
            pub ident: Ident,
            pub args: TokenTree,
            pub body: TokenTree,
        }),
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for ItemMacro2 {}

#[cfg(feature = "extra-traits")]
impl PartialEq for ItemMacro2 {
    fn eq(&self, other: &Self) -> bool {
        self.attrs == other.attrs && self.vis == other.vis && self.macro_token == other.macro_token
            && self.ident == other.ident
            && TokenTreeHelper(&self.args) == TokenTreeHelper(&other.args)
            && TokenTreeHelper(&self.body) == TokenTreeHelper(&other.body)
    }
}

#[cfg(feature = "extra-traits")]
impl Hash for ItemMacro2 {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.attrs.hash(state);
        self.vis.hash(state);
        self.macro_token.hash(state);
        self.ident.hash(state);
        TokenTreeHelper(&self.args).hash(state);
        TokenTreeHelper(&self.body).hash(state);
    }
}

impl From<DeriveInput> for Item {
    fn from(input: DeriveInput) -> Item {
        match input.body {
            Body::Enum(data) => Item::Enum(ItemEnum {
                attrs: input.attrs,
                vis: input.vis,
                enum_token: data.enum_token,
                ident: input.ident,
                generics: input.generics,
                brace_token: data.brace_token,
                variants: data.variants,
            }),
            Body::Struct(data) => Item::Struct(ItemStruct {
                attrs: input.attrs,
                vis: input.vis,
                struct_token: data.struct_token,
                ident: input.ident,
                generics: input.generics,
                data: data.data,
                semi_token: data.semi_token,
            }),
        }
    }
}

ast_enum_of_structs! {
    /// Things that can appear directly inside of a module.
    pub enum UseTree {
        /// `use prefix::Ty` or `use prefix::Ty as Renamed`
        pub Path(UsePath {
            pub ident: Ident,
            pub rename: Option<(Token![as], Ident)>,
        }),
        /// `use prefix::*`
        pub Glob(UseGlob {
            pub star_token: Token![*],
        }),
        /// `use prefix::{a, b, c}`
        pub List(UseList {
            pub brace_token: token::Brace,
            pub items: Delimited<UseTree, Token![,]>,
        }),
    }
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum Constness {
        Const(Token![const]),
        NotConst,
    }
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum Defaultness {
        Default(Token![default]),
        Final,
    }
}

ast_enum_of_structs! {
    /// An item within an `extern` block
    pub enum ForeignItem {
        /// A foreign function
        pub Fn(ForeignItemFn {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub ident: Ident,
            pub decl: Box<FnDecl>,
            pub semi_token: Token![;],
        }),
        /// A foreign static item (`static ext: u8`)
        pub Static(ForeignItemStatic {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub static_token: Token![static],
            pub mutbl: Mutability,
            pub ident: Ident,
            pub colon_token: Token![:],
            pub ty: Box<Type>,
            pub semi_token: Token![;],
        }),
        /// A foreign type
        pub Type(ForeignItemType {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub type_token: Token![type],
            pub ident: Ident,
            pub semi_token: Token![;],
        }),
    }
}

ast_enum_of_structs! {
    /// Represents an item declaration within a trait declaration,
    /// possibly including a default implementation. A trait item is
    /// either required (meaning it doesn't have an implementation, just a
    /// signature) or provided (meaning it has a default implementation).
    pub enum TraitItem {
        pub Const(TraitItemConst {
            pub attrs: Vec<Attribute>,
            pub const_token: Token![const],
            pub ident: Ident,
            pub colon_token: Token![:],
            pub ty: Type,
            pub default: Option<(Token![=], Expr)>,
            pub semi_token: Token![;],
        }),
        pub Method(TraitItemMethod {
            pub attrs: Vec<Attribute>,
            pub sig: MethodSig,
            pub default: Option<Block>,
            pub semi_token: Option<Token![;]>,
        }),
        pub Type(TraitItemType {
            pub attrs: Vec<Attribute>,
            pub type_token: Token![type],
            pub ident: Ident,
            pub generics: Generics,
            pub colon_token: Option<Token![:]>,
            pub bounds: Delimited<TypeParamBound, Token![+]>,
            pub default: Option<(Token![=], Type)>,
            pub semi_token: Token![;],
        }),
        pub Macro(TraitItemMacro {
            pub attrs: Vec<Attribute>,
            pub mac: Macro,
            pub semi_token: Option<Token![;]>,
        }),
    }
}

ast_enum! {
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum ImplPolarity {
        /// `impl Trait for Type`
        Positive,
        /// `impl !Trait for Type`
        Negative(Token![!]),
    }
}

ast_enum_of_structs! {
    pub enum ImplItem {
        pub Const(ImplItemConst {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub defaultness: Defaultness,
            pub const_token: Token![const],
            pub ident: Ident,
            pub colon_token: Token![:],
            pub ty: Type,
            pub eq_token: Token![=],
            pub expr: Expr,
            pub semi_token: Token![;],
        }),
        pub Method(ImplItemMethod {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub defaultness: Defaultness,
            pub sig: MethodSig,
            pub block: Block,
        }),
        pub Type(ImplItemType {
            pub attrs: Vec<Attribute>,
            pub vis: Visibility,
            pub defaultness: Defaultness,
            pub type_token: Token![type],
            pub ident: Ident,
            pub generics: Generics,
            pub eq_token: Token![=],
            pub ty: Type,
            pub semi_token: Token![;],
        }),
        pub Macro(ImplItemMacro {
            pub attrs: Vec<Attribute>,
            pub mac: Macro,
            pub semi_token: Option<Token![;]>,
        }),
    }
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
        pub fn_token: Token![fn],
        pub paren_token: token::Paren,
        pub inputs: Delimited<FnArg, Token![,]>,
        pub output: ReturnType,
        pub generics: Generics,
        pub variadic: Option<Token![...]>,
    }
}

ast_enum_of_structs! {
    /// An argument in a function header.
    ///
    /// E.g. `bar: usize` as in `fn foo(bar: usize)`
    pub enum FnArg {
        pub SelfRef(ArgSelfRef {
            pub and_token: Token![&],
            pub self_token: Token![self],
            pub lifetime: Option<Lifetime>,
            pub mutbl: Mutability,
        }),
        pub SelfValue(ArgSelf {
            pub mutbl: Mutability,
            pub self_token: Token![self],
        }),
        pub Captured(ArgCaptured {
            pub pat: Pat,
            pub colon_token: Token![:],
            pub ty: Type,
        }),
        pub Inferred(Pat),
        pub Ignored(Type),
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;

    use synom::Synom;
    use proc_macro2::{TokenNode, Delimiter};

    impl_synom!(Item "item" alt!(
        syn!(ItemExternCrate) => { Item::ExternCrate }
        |
        syn!(ItemUse) => { Item::Use }
        |
        syn!(ItemStatic) => { Item::Static }
        |
        syn!(ItemConst) => { Item::Const }
        |
        syn!(ItemFn) => { Item::Fn }
        |
        syn!(ItemMod) => { Item::Mod }
        |
        syn!(ItemForeignMod) => { Item::ForeignMod }
        |
        syn!(ItemType) => { Item::Type }
        |
        syn!(ItemStruct) => { Item::Struct }
        |
        syn!(ItemEnum) => { Item::Enum }
        |
        syn!(ItemUnion) => { Item::Union }
        |
        syn!(ItemTrait) => { Item::Trait }
        |
        syn!(ItemDefaultImpl) => { Item::DefaultImpl }
        |
        syn!(ItemImpl) => { Item::Impl }
        |
        syn!(ItemMacro) => { Item::Macro }
        |
        syn!(ItemMacro2) => { Item::Macro2 }
    ));

    impl_synom!(ItemMacro "macro item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        what: syn!(Path) >>
        bang: punct!(!) >>
        ident: option!(syn!(Ident)) >>
        body: call!(tt::delimited) >>
        semi: cond!(!is_braced(&body), punct!(;)) >>
        (ItemMacro {
            attrs: attrs,
            ident: ident,
            mac: Macro {
                path: what,
                bang_token: bang,
                tokens: body,
            },
            semi_token: semi,
        })
    ));

    // TODO: figure out the actual grammar; is body required to be braced?
    impl_synom!(ItemMacro2 "macro2 item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        macro_: keyword!(macro) >>
        ident: syn!(Ident) >>
        args: call!(tt::parenthesized) >>
        body: call!(tt::braced) >>
        (ItemMacro2 {
            attrs: attrs,
            vis: vis,
            macro_token: macro_,
            ident: ident,
            args: args,
            body: body,
        })
    ));

    impl_synom!(ItemExternCrate "extern crate item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        extern_: keyword!(extern) >>
        crate_: keyword!(crate) >>
        ident: syn!(Ident) >>
        rename: option!(tuple!(keyword!(as), syn!(Ident))) >>
        semi: punct!(;) >>
        (ItemExternCrate {
            attrs: attrs,
            vis: vis,
            extern_token: extern_,
            crate_token: crate_,
            ident: ident,
            rename: rename,
            semi_token: semi,
        })
    ));

    impl_synom!(ItemUse "use item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        use_: keyword!(use) >>
        leading_colon: option!(punct!(::)) >>
        mut prefix: call!(Delimited::parse_terminated_with, use_prefix) >>
        tree: switch!(value!(prefix.empty_or_trailing()),
            true => syn!(UseTree)
            |
            false => alt!(
                tuple!(keyword!(as), syn!(Ident)) => {
                    |rename| UseTree::Path(UsePath {
                        ident: prefix.pop().unwrap().into_item(),
                        rename: Some(rename),
                    })
                }
                |
                epsilon!() => {
                    |_| UseTree::Path(UsePath {
                        ident: prefix.pop().unwrap().into_item(),
                        rename: None,
                    })
                }
            )
        ) >>
        semi: punct!(;) >>
        (ItemUse {
            attrs: attrs,
            vis: vis,
            use_token: use_,
            leading_colon: leading_colon,
            prefix: prefix,
            tree: tree,
            semi_token: semi,
        })
    ));

    named!(use_prefix -> Ident, alt!(
        syn!(Ident)
        |
        keyword!(self) => { Into::into }
        |
        keyword!(super) => { Into::into }
        |
        keyword!(crate) => { Into::into }
    ));

    impl_synom!(UseTree "use tree" alt!(
        syn!(UsePath) => { UseTree::Path }
        |
        syn!(UseGlob) => { UseTree::Glob }
        |
        syn!(UseList) => { UseTree::List }
    ));

    impl_synom!(UsePath "use path" do_parse!(
        ident: alt!(
            syn!(Ident)
            |
            keyword!(self) => { Into::into }
        ) >>
        rename: option!(tuple!(keyword!(as), syn!(Ident))) >>
        (UsePath {
            ident: ident,
            rename: rename,
        })
    ));

    impl_synom!(UseGlob "use glob" do_parse!(
        star: punct!(*) >>
        (UseGlob {
            star_token: star,
        })
    ));

    impl_synom!(UseList "use list" do_parse!(
        list: braces!(Delimited::parse_terminated) >>
        (UseList {
            brace_token: list.1,
            items: list.0,
        })
    ));

    impl_synom!(ItemStatic "static item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        static_: keyword!(static) >>
        mutability: syn!(Mutability) >>
        ident: syn!(Ident) >>
        colon: punct!(:) >>
        ty: syn!(Type) >>
        eq: punct!(=) >>
        value: syn!(Expr) >>
        semi: punct!(;) >>
        (ItemStatic {
            attrs: attrs,
            vis: vis,
            static_token: static_,
            mutbl: mutability,
            ident: ident,
            colon_token: colon,
            ty: Box::new(ty),
            eq_token: eq,
            expr: Box::new(value),
            semi_token: semi,
        })
    ));

    impl_synom!(ItemConst "const item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        const_: keyword!(const) >>
        ident: syn!(Ident) >>
        colon: punct!(:) >>
        ty: syn!(Type) >>
        eq: punct!(=) >>
        value: syn!(Expr) >>
        semi: punct!(;) >>
        (ItemConst {
            attrs: attrs,
            vis: vis,
            const_token: const_,
            ident: ident,
            colon_token: colon,
            ty: Box::new(ty),
            eq_token: eq,
            expr: Box::new(value),
            semi_token: semi,
        })
    ));

    impl_synom!(ItemFn "fn item" do_parse!(
        outer_attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        constness: syn!(Constness) >>
        unsafety: syn!(Unsafety) >>
        abi: option!(syn!(Abi)) >>
        fn_: keyword!(fn) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(Delimited::parse_terminated) >>
        ret: syn!(ReturnType) >>
        where_clause: option!(syn!(WhereClause)) >>
        inner_attrs_stmts: braces!(tuple!(
            many0!(Attribute::parse_inner),
            call!(Block::parse_within)
        )) >>
        (ItemFn {
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend((inner_attrs_stmts.0).0);
                attrs
            },
            vis: vis,
            constness: constness,
            unsafety: unsafety,
            abi: abi,
            decl: Box::new(FnDecl {
                fn_token: fn_,
                paren_token: inputs.1,
                inputs: inputs.0,
                output: ret,
                variadic: None,
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
        })
    ));

    impl Synom for FnArg {
        named!(parse -> Self, alt!(
            do_parse!(
                and: punct!(&) >>
                lt: option!(syn!(Lifetime)) >>
                mutability: syn!(Mutability) >>
                self_: keyword!(self) >>
                not!(punct!(:)) >>
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
                self_: keyword!(self) >>
                not!(punct!(:)) >>
                (ArgSelf {
                    mutbl: mutability,
                    self_token: self_,
                }.into())
            )
            |
            do_parse!(
                pat: syn!(Pat) >>
                colon: punct!(:) >>
                ty: syn!(Type) >>
                (ArgCaptured {
                    pat: pat,
                    ty: ty,
                    colon_token: colon,
                }.into())
            )
            |
            syn!(Type) => { FnArg::Ignored }
        ));
    }

    impl_synom!(ItemMod "mod item" do_parse!(
        outer_attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        mod_: keyword!(mod) >>
        ident: syn!(Ident) >>
        content_semi: alt!(
            punct!(;) => {|semi| (
                Vec::new(),
                None,
                Some(semi),
            )}
            |
            braces!(
                tuple!(
                    many0!(Attribute::parse_inner),
                    many0!(Item::parse)
                )
            ) => {|((inner_attrs, items), brace)| (
                inner_attrs,
                Some((brace, items)),
                None,
            )}
        ) >>
        (ItemMod {
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend(content_semi.0);
                attrs
            },
            vis: vis,
            mod_token: mod_,
            ident: ident,
            content: content_semi.1,
            semi: content_semi.2,
        })
    ));

    impl_synom!(ItemForeignMod "foreign mod item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        abi: syn!(Abi) >>
        items: braces!(many0!(ForeignItem::parse)) >>
        (ItemForeignMod {
            attrs: attrs,
            abi: abi,
            brace_token: items.1,
            items: items.0,
        })
    ));

    impl_synom!(ForeignItem "foreign item" alt!(
        syn!(ForeignItemFn) => { ForeignItem::Fn }
        |
        syn!(ForeignItemStatic) => { ForeignItem::Static }
        |
        syn!(ForeignItemType) => { ForeignItem::Type }
    ));

    impl_synom!(ForeignItemFn "foreign function" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        fn_: keyword!(fn) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(do_parse!(
            args: call!(Delimited::parse_terminated) >>
            variadic: cond!(args.is_empty() || args.trailing_delim(),
                            option!(punct!(...))) >>
            (args, variadic)
        )) >>
        ret: syn!(ReturnType) >>
        where_clause: option!(syn!(WhereClause)) >>
        semi: punct!(;) >>
        ({
            let ((inputs, variadic), parens) = inputs;
            let variadic = variadic.and_then(|v| v);
            ForeignItemFn {
                ident: ident,
                attrs: attrs,
                semi_token: semi,
                decl: Box::new(FnDecl {
                    fn_token: fn_,
                    paren_token: parens,
                    inputs: inputs,
                    variadic: variadic,
                    output: ret,
                    generics: Generics {
                        where_clause: where_clause,
                        .. generics
                    },
                }),
                vis: vis,
            }
        })
    ));

    impl_synom!(ForeignItemStatic "foreign static" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        static_: keyword!(static) >>
        mutability: syn!(Mutability) >>
        ident: syn!(Ident) >>
        colon: punct!(:) >>
        ty: syn!(Type) >>
        semi: punct!(;) >>
        (ForeignItemStatic {
            ident: ident,
            attrs: attrs,
            semi_token: semi,
            ty: Box::new(ty),
            mutbl: mutability,
            static_token: static_,
            colon_token: colon,
            vis: vis,
        })
    ));

    impl_synom!(ForeignItemType "foreign type" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        type_: keyword!(type) >>
        ident: syn!(Ident) >>
        semi: punct!(;) >>
        (ForeignItemType {
            attrs: attrs,
            vis: vis,
            type_token: type_,
            ident: ident,
            semi_token: semi,
        })
    ));

    impl_synom!(ItemType "type item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        type_: keyword!(type) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        where_clause: option!(syn!(WhereClause)) >>
        eq: punct!(=) >>
        ty: syn!(Type) >>
        semi: punct!(;) >>
        (ItemType {
            attrs: attrs,
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
        })
    ));

    impl_synom!(ItemStruct "struct item" switch!(
        map!(syn!(DeriveInput), Into::into),
        Item::Struct(item) => value!(item)
        |
        _ => reject!()
    ));

    impl_synom!(ItemEnum "enum item" switch!(
        map!(syn!(DeriveInput), Into::into),
        Item::Enum(item) => value!(item)
        |
        _ => reject!()
    ));

    impl_synom!(ItemUnion "union item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        union_: keyword!(union) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        where_clause: option!(syn!(WhereClause)) >>
        fields: braces!(call!(Delimited::parse_terminated_with,
                              Field::parse_struct)) >>
        (ItemUnion {
            attrs: attrs,
            vis: vis,
            union_token: union_,
            ident: ident,
            generics: Generics {
                where_clause: where_clause,
                .. generics
            },
            data: VariantData::Struct(fields.0, fields.1),
        })
    ));

    impl_synom!(ItemTrait "trait item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        unsafety: syn!(Unsafety) >>
        auto_: option!(keyword!(auto)) >>
        trait_: keyword!(trait) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        colon: option!(punct!(:)) >>
        bounds: cond!(colon.is_some(),
            call!(Delimited::parse_separated_nonempty)
        ) >>
        where_clause: option!(syn!(WhereClause)) >>
        body: braces!(many0!(TraitItem::parse)) >>
        (ItemTrait {
            attrs: attrs,
            vis: vis,
            unsafety: unsafety,
            auto_token: auto_,
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
        })
    ));

    impl_synom!(ItemDefaultImpl "default impl item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        unsafety: syn!(Unsafety) >>
        impl_: keyword!(impl) >>
        path: syn!(Path) >>
        for_: keyword!(for) >>
        dot2: punct!(..) >>
        braces: braces!(epsilon!()) >>
        (ItemDefaultImpl {
            attrs: attrs,
            unsafety: unsafety,
            impl_token: impl_,
            path: path,
            for_token: for_,
            dot2_token: dot2,
            brace_token: braces.1,
        })
    ));

    impl_synom!(TraitItem "trait item" alt!(
        syn!(TraitItemConst) => { TraitItem::Const }
        |
        syn!(TraitItemMethod) => { TraitItem::Method }
        |
        syn!(TraitItemType) => { TraitItem::Type }
        |
        syn!(TraitItemMacro) => { TraitItem::Macro }
    ));

    impl_synom!(TraitItemConst "const trait item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        const_: keyword!(const) >>
        ident: syn!(Ident) >>
        colon: punct!(:) >>
        ty: syn!(Type) >>
        default: option!(tuple!(punct!(=), syn!(Expr))) >>
        semi: punct!(;) >>
        (TraitItemConst {
            attrs: attrs,
            const_token: const_,
            ident: ident,
            colon_token: colon,
            ty: ty,
            default: default,
            semi_token: semi,
        })
    ));

    impl_synom!(TraitItemMethod "method trait item" do_parse!(
        outer_attrs: many0!(Attribute::parse_outer) >>
        constness: syn!(Constness) >>
        unsafety: syn!(Unsafety) >>
        abi: option!(syn!(Abi)) >>
        fn_: keyword!(fn) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(call!(Delimited::parse_terminated)) >>
        ret: syn!(ReturnType) >>
        where_clause: option!(syn!(WhereClause)) >>
        body: option!(braces!(
            tuple!(many0!(Attribute::parse_inner),
                   call!(Block::parse_within))
        )) >>
        semi: cond!(body.is_none(), punct!(;)) >>
        ({
            let (inner_attrs, stmts) = match body {
                Some(((inner_attrs, stmts), b)) => (inner_attrs, Some((stmts, b))),
                None => (Vec::new(), None),
            };
            TraitItemMethod {
                attrs: {
                    let mut attrs = outer_attrs;
                    attrs.extend(inner_attrs);
                    attrs
                },
                sig: MethodSig {
                    constness: constness,
                    unsafety: unsafety,
                    abi: abi,
                    ident: ident,
                    decl: FnDecl {
                        inputs: inputs.0,
                        output: ret,
                        fn_token: fn_,
                        paren_token: inputs.1,
                        variadic: None,
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
            }
        })
    ));

    impl_synom!(TraitItemType "trait item type" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        type_: keyword!(type) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        colon: option!(punct!(:)) >>
        bounds: cond!(colon.is_some(),
            call!(Delimited::parse_separated_nonempty)
        ) >>
        where_clause: option!(syn!(WhereClause)) >>
        default: option!(tuple!(punct!(=), syn!(Type))) >>
        semi: punct!(;) >>
        (TraitItemType {
            attrs: attrs,
            type_token: type_,
            ident: ident,
            generics: Generics {
                where_clause: where_clause,
                .. generics
            },
            colon_token: colon,
            bounds: bounds.unwrap_or_default(),
            default: default,
            semi_token: semi,
        })
    ));

    impl_synom!(TraitItemMacro "trait item macro" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        mac: syn!(Macro) >>
        semi: cond!(!is_braced(&mac.tokens), punct!(;)) >>
        (TraitItemMacro {
            attrs: attrs,
            mac: mac,
            semi_token: semi,
        })
    ));

    impl_synom!(ItemImpl "impl item" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        defaultness: syn!(Defaultness) >>
        unsafety: syn!(Unsafety) >>
        impl_: keyword!(impl) >>
        generics: syn!(Generics) >>
        polarity_path: alt!(
            do_parse!(
                polarity: syn!(ImplPolarity) >>
                path: syn!(Path) >>
                for_: keyword!(for) >>
                (Some((polarity, path, for_)))
            )
            |
            epsilon!() => { |_| None }
        ) >>
        self_ty: syn!(Type) >>
        where_clause: option!(syn!(WhereClause)) >>
        body: braces!(many0!(ImplItem::parse)) >>
        (ItemImpl {
            attrs: attrs,
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
        })
    ));

    impl_synom!(ImplItem "item in impl block" alt!(
        syn!(ImplItemConst) => { ImplItem::Const }
        |
        syn!(ImplItemMethod) => { ImplItem::Method }
        |
        syn!(ImplItemType) => { ImplItem::Type }
        |
        syn!(ImplItemMacro) => { ImplItem::Macro }
    ));

    impl_synom!(ImplItemConst "const item in impl block" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        defaultness: syn!(Defaultness) >>
        const_: keyword!(const) >>
        ident: syn!(Ident) >>
        colon: punct!(:) >>
        ty: syn!(Type) >>
        eq: punct!(=) >>
        value: syn!(Expr) >>
        semi: punct!(;) >>
        (ImplItemConst {
            attrs: attrs,
            vis: vis,
            defaultness: defaultness,
            const_token: const_,
            ident: ident,
            colon_token: colon,
            ty: ty,
            eq_token: eq,
            expr: value,
            semi_token: semi,
        })
    ));

    impl_synom!(ImplItemMethod "method in impl block" do_parse!(
        outer_attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        defaultness: syn!(Defaultness) >>
        constness: syn!(Constness) >>
        unsafety: syn!(Unsafety) >>
        abi: option!(syn!(Abi)) >>
        fn_: keyword!(fn) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        inputs: parens!(call!(Delimited::parse_terminated)) >>
        ret: syn!(ReturnType) >>
        where_clause: option!(syn!(WhereClause)) >>
        inner_attrs_stmts: braces!(tuple!(
            many0!(Attribute::parse_inner),
            call!(Block::parse_within)
        )) >>
        (ImplItemMethod {
            attrs: {
                let mut attrs = outer_attrs;
                attrs.extend((inner_attrs_stmts.0).0);
                attrs
            },
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
                    generics: Generics {
                        where_clause: where_clause,
                        .. generics
                    },
                    variadic: None,
                },
            },
            block: Block {
                brace_token: inner_attrs_stmts.1,
                stmts: (inner_attrs_stmts.0).1,
            },
        })
    ));

    impl_synom!(ImplItemType "type in impl block" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        vis: syn!(Visibility) >>
        defaultness: syn!(Defaultness) >>
        type_: keyword!(type) >>
        ident: syn!(Ident) >>
        generics: syn!(Generics) >>
        eq: punct!(=) >>
        ty: syn!(Type) >>
        semi: punct!(;) >>
        (ImplItemType {
            attrs: attrs,
            vis: vis,
            defaultness: defaultness,
            type_token: type_,
            ident: ident,
            generics: generics,
            eq_token: eq,
            ty: ty,
            semi_token: semi,
        })
    ));

    impl_synom!(ImplItemMacro "macro in impl block" do_parse!(
        attrs: many0!(Attribute::parse_outer) >>
        mac: syn!(Macro) >>
        semi: cond!(!is_braced(&mac.tokens), punct!(;)) >>
        (ImplItemMacro {
            attrs: attrs,
            mac: mac,
            semi_token: semi,
        })
    ));

    impl Synom for ImplPolarity {
        named!(parse -> Self, alt!(
            punct!(!) => { ImplPolarity::Negative }
            |
            epsilon!() => { |_| ImplPolarity::Positive }
        ));
    }

    impl Synom for Constness {
        named!(parse -> Self, alt!(
            keyword!(const) => { Constness::Const }
            |
            epsilon!() => { |_| Constness::NotConst }
        ));
    }

    impl Synom for Defaultness {
        named!(parse -> Self, alt!(
            keyword!(default) => { Defaultness::Default }
            |
            epsilon!() => { |_| Defaultness::Final }
        ));
    }

    fn is_braced(tt: &TokenTree) -> bool {
        match tt.kind {
            TokenNode::Group(Delimiter::Brace, _) => true,
            _ => false,
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use attr::FilterAttrs;
    use data::VariantData;
    use quote::{ToTokens, Tokens};

    impl ToTokens for ItemExternCrate {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.extern_token.to_tokens(tokens);
            self.crate_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            if let Some((ref as_token, ref rename)) = self.rename {
                as_token.to_tokens(tokens);
                rename.to_tokens(tokens);
            }
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ItemUse {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.use_token.to_tokens(tokens);
            self.leading_colon.to_tokens(tokens);
            self.prefix.to_tokens(tokens);
            self.tree.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ItemStatic {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.static_token.to_tokens(tokens);
            self.mutbl.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ItemConst {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.const_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ItemFn {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.constness.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.abi.to_tokens(tokens);
            NamedDecl(&self.decl, self.ident).to_tokens(tokens);
            self.block.brace_token.surround(tokens, |tokens| {
                tokens.append_all(self.attrs.inner());
                tokens.append_all(&self.block.stmts);
            });
        }
    }

    impl ToTokens for ItemMod {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.mod_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            if let Some((ref brace, ref items)) = self.content {
                brace.surround(tokens, |tokens| {
                    tokens.append_all(self.attrs.inner());
                    tokens.append_all(items);
                });
            } else {
                TokensOrDefault(&self.semi).to_tokens(tokens);
            }
        }
    }

    impl ToTokens for ItemForeignMod {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.abi.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.items);
            });
        }
    }

    impl ToTokens for ItemType {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.type_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ItemEnum {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.enum_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                self.variants.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for ItemStruct {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.struct_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            match self.data {
                VariantData::Struct(..) => {
                    self.generics.where_clause.to_tokens(tokens);
                    self.data.to_tokens(tokens);
                }
                VariantData::Tuple(..) => {
                    self.data.to_tokens(tokens);
                    self.generics.where_clause.to_tokens(tokens);
                    TokensOrDefault(&self.semi_token).to_tokens(tokens);
                }
                VariantData::Unit => {
                    self.generics.where_clause.to_tokens(tokens);
                    TokensOrDefault(&self.semi_token).to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for ItemUnion {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.union_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            // XXX: Should we handle / complain when using a
            // non-VariantData::Struct Variant in Union?
            self.data.to_tokens(tokens);
        }
    }

    impl ToTokens for ItemTrait {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.auto_token.to_tokens(tokens);
            self.trait_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            if !self.supertraits.is_empty() {
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
                self.supertraits.to_tokens(tokens);
            }
            self.generics.where_clause.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.items);
            });
        }
    }

    impl ToTokens for ItemDefaultImpl {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.unsafety.to_tokens(tokens);
            self.impl_token.to_tokens(tokens);
            self.path.to_tokens(tokens);
            self.for_token.to_tokens(tokens);
            self.dot2_token.to_tokens(tokens);
            self.brace_token.surround(tokens, |_tokens| {});
        }
    }

    impl ToTokens for ItemImpl {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.defaultness.to_tokens(tokens);
            self.unsafety.to_tokens(tokens);
            self.impl_token.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            if let Some((ref polarity, ref path, ref for_token)) = self.trait_ {
                polarity.to_tokens(tokens);
                path.to_tokens(tokens);
                for_token.to_tokens(tokens);
            }
            self.self_ty.to_tokens(tokens);
            self.generics.where_clause.to_tokens(tokens);
            self.brace_token.surround(tokens, |tokens| {
                tokens.append_all(&self.items);
            });
        }
    }

    impl ToTokens for ItemMacro {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.mac.path.to_tokens(tokens);
            self.mac.bang_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.mac.tokens.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ItemMacro2 {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.macro_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.args.to_tokens(tokens);
            self.body.to_tokens(tokens);
        }
    }

    impl ToTokens for UsePath {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            if let Some((ref as_token, ref rename)) = self.rename {
                as_token.to_tokens(tokens);
                rename.to_tokens(tokens);
            }
        }
    }

    impl ToTokens for UseGlob {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.star_token.to_tokens(tokens);
        }
    }

    impl ToTokens for UseList {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.brace_token.surround(tokens, |tokens| {
                self.items.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for TraitItemConst {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.const_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            if let Some((ref eq_token, ref default)) = self.default {
                eq_token.to_tokens(tokens);
                default.to_tokens(tokens);
            }
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TraitItemMethod {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.sig.to_tokens(tokens);
            match self.default {
                Some(ref block) => {
                    block.brace_token.surround(tokens, |tokens| {
                        tokens.append_all(self.attrs.inner());
                        tokens.append_all(&block.stmts);
                    });
                }
                None => {
                    TokensOrDefault(&self.semi_token).to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for TraitItemType {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.type_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            if !self.bounds.is_empty() {
                TokensOrDefault(&self.colon_token).to_tokens(tokens);
                self.bounds.to_tokens(tokens);
            }
            self.generics.where_clause.to_tokens(tokens);
            if let Some((ref eq_token, ref default)) = self.default {
                eq_token.to_tokens(tokens);
                default.to_tokens(tokens);
            }
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for TraitItemMacro {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.mac.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ImplItemConst {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            self.const_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.expr.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ImplItemMethod {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            self.sig.to_tokens(tokens);
            self.block.brace_token.surround(tokens, |tokens| {
                tokens.append_all(self.attrs.inner());
                tokens.append_all(&self.block.stmts);
            });
        }
    }

    impl ToTokens for ImplItemType {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.defaultness.to_tokens(tokens);
            self.type_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.generics.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ImplItemMacro {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.mac.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ForeignItemFn {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            NamedDecl(&self.decl, self.ident).to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ForeignItemStatic {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.static_token.to_tokens(tokens);
            self.mutbl.to_tokens(tokens);
            self.ident.to_tokens(tokens);
            self.colon_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
            self.semi_token.to_tokens(tokens);
        }
    }

    impl ToTokens for ForeignItemType {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append_all(self.attrs.outer());
            self.vis.to_tokens(tokens);
            self.type_token.to_tokens(tokens);
            self.ident.to_tokens(tokens);
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
                if self.0.variadic.is_some() && !self.0.inputs.empty_or_trailing() {
                    <Token![,]>::default().to_tokens(tokens);
                }
                self.0.variadic.to_tokens(tokens);
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
