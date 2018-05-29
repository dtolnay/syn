//! This crate automatically generates the definition of the `Visit`,
//! `VisitMut`, and `Fold` traits in `syn` based on the `syn` source. It
//! discovers structs and enums declared with the `ast_*` macros and generates
//! the functions for those types.
//!
//! It makes a few assumptions about the target crate:
//! 1. All structs which are discovered must be re-exported in the root of the
//!    crate, even if they were declared in a submodule.
//! 2. This code cannot discover submodules which are located in subdirectories
//!    - only submodules located in the same directory.
//! 3. The path to `syn` is hardcoded.

#![cfg_attr(feature = "cargo-clippy", allow(redundant_closure))]

#[macro_use]
extern crate failure;
extern crate inflections;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
#[macro_use]
extern crate syn;

use failure::{err_msg, Error};
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::{Attribute, Data, DataStruct, DeriveInput, Ident, Item};

use std::collections::BTreeMap;
use std::fmt::{self, Debug};
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

const SYN_CRATE_ROOT: &str = "../src/lib.rs";

const FOLD_SRC: &str = "../src/gen/fold.rs";
const VISIT_SRC: &str = "../src/gen/visit.rs";
const VISIT_MUT_SRC: &str = "../src/gen/visit_mut.rs";

const IGNORED_MODS: &[&str] = &["fold", "visit", "visit_mut"];

const EXTRA_TYPES: &[&str] = &["Lifetime"];

const TERMINAL_TYPES: &[&str] = &["Span", "Ident"];

fn path_eq(a: &syn::Path, b: &str) -> bool {
    if a.global() {
        return false;
    }
    if a.segments.len() != 1 {
        return false;
    }
    a.segments[0].ident == b
}

fn get_features(attrs: &[Attribute], mut features: TokenStream) -> TokenStream {
    for attr in attrs {
        if path_eq(&attr.path, "cfg") {
            attr.to_tokens(&mut features);
        }
    }
    features
}

#[derive(Clone)]
pub struct AstItem {
    ast: DeriveInput,
    features: TokenStream,
    // True if this is an ast_enum_of_structs! item with a #full annotation.
    eos_full: bool,
}

impl Debug for AstItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("AstItem")
            .field("ast", &self.ast)
            .field("features", &self.features.to_string())
            .finish()
    }
}

// NOTE: BTreeMap is used here instead of HashMap to have deterministic output.
type Lookup = BTreeMap<Ident, AstItem>;

fn load_file<P: AsRef<Path>>(
    name: P,
    features: &TokenStream,
    lookup: &mut Lookup,
) -> Result<(), Error> {
    let name = name.as_ref();
    let parent = name.parent().ok_or_else(|| err_msg("no parent path"))?;

    let mut f = File::open(name)?;
    let mut src = String::new();
    f.read_to_string(&mut src)?;

    // Parse the file
    let file =
        syn::parse_file(&src).map_err(|_| format_err!("failed to parse {}", name.display()))?;

    // Collect all of the interesting AstItems declared in this file or submodules.
    'items: for item in file.items {
        match item {
            Item::Mod(item) => {
                // Don't inspect inline modules.
                if item.content.is_some() {
                    continue;
                }

                // We don't want to try to load the generated rust files and
                // parse them, so we ignore them here.
                for name in IGNORED_MODS {
                    if item.ident == name {
                        continue 'items;
                    }
                }

                // Lookup any #[cfg()] attributes on the module and add them to
                // the feature set.
                //
                // The derive module is weird because it is built with either
                // `full` or `derive` but exported only under `derive`.
                let features = if item.ident == "derive" {
                    quote!(#[cfg(feature = "derive")])
                } else {
                    get_features(&item.attrs, features.clone())
                };

                // Look up the submodule file, and recursively parse it.
                // XXX: Only handles same-directory .rs file submodules.
                let path = parent.join(&format!("{}.rs", item.ident));
                load_file(path, &features, lookup)?;
            }
            Item::Macro(item) => {
                // Lookip any #[cfg()] attributes directly on the macro
                // invocation, and add them to the feature set.
                let features = get_features(&item.attrs, features.clone());

                // Try to parse the AstItem declaration out of the item.
                let tts = &item.mac.tts;
                let found = if path_eq(&item.mac.path, "ast_struct") {
                    syn::parse_str::<parsing::AstStruct>(&quote!(#tts).to_string())
                        .map_err(|_| err_msg("failed to parse ast_struct"))?
                        .0
                } else if path_eq(&item.mac.path, "ast_enum") {
                    syn::parse_str::<parsing::AstEnum>(&quote!(#tts).to_string())
                        .map_err(|_| err_msg("failed to parse ast_enum"))?
                        .0
                } else if path_eq(&item.mac.path, "ast_enum_of_structs") {
                    syn::parse_str::<parsing::AstEnumOfStructs>(&quote!(#tts).to_string())
                        .map_err(|_| err_msg("failed to parse ast_enum_of_structs"))?
                        .0
                } else {
                    continue;
                };

                // Record our features on the parsed AstItems.
                for mut item in found {
                    features.to_tokens(&mut item.features);
                    lookup.insert(item.ast.ident.clone(), item);
                }
            }
            Item::Struct(item) => {
                let ident = item.ident;
                if EXTRA_TYPES.contains(&&ident.to_string()[..]) {
                    lookup.insert(
                        ident.clone(),
                        AstItem {
                            ast: DeriveInput {
                                ident: ident,
                                vis: item.vis,
                                attrs: item.attrs,
                                generics: item.generics,
                                data: Data::Struct(DataStruct {
                                    fields: item.fields,
                                    struct_token: item.struct_token,
                                    semi_token: item.semi_token,
                                }),
                            },
                            features: features.clone(),
                            eos_full: false,
                        },
                    );
                }
            }
            _ => {}
        }
    }
    Ok(())
}

mod parsing {
    use super::AstItem;

    use proc_macro2::TokenStream;
    use syn;
    use syn::synom::*;
    use syn::*;

    // Parses #full - returns #[cfg(feature = "full")] if it is present, and
    // nothing otherwise.
    named!(full -> (TokenStream, bool), map!(option!(do_parse!(
        punct!(#) >>
        id: syn!(Ident) >>
        cond_reduce!(id == "full") >>
        ()
    )), |s| if s.is_some() {
        (quote!(#[cfg(feature = "full")]), true)
    } else {
        (quote!(), false)
    }));

    named!(manual_extra_traits -> (), do_parse!(
        punct!(#) >>
        id: syn!(Ident) >>
        cond_reduce!(id == "manual_extra_traits") >>
        ()
    ));

    // Parses a simple AstStruct without the `pub struct` prefix.
    named!(ast_struct_inner -> AstItem, do_parse!(
        id: syn!(Ident) >>
        features: full >>
        option!(manual_extra_traits) >>
        rest: syn!(TokenStream) >>
        (AstItem {
            ast: syn::parse_str(&quote! {
                pub struct #id #rest
            }.to_string())?,
            features: features.0,
            eos_full: features.1,
        })
    ));

    // ast_struct! parsing
    pub struct AstStruct(pub Vec<AstItem>);
    impl Synom for AstStruct {
        named!(parse -> Self, do_parse!(
            many0!(Attribute::parse_outer) >>
            keyword!(pub) >>
            keyword!(struct) >>
            res: call!(ast_struct_inner) >>
            (AstStruct(vec![res]))
        ));
    }

    named!(no_visit -> (), do_parse!(
        punct!(#) >>
        id: syn!(Ident) >>
        cond_reduce!(id == "no_visit") >>
        ()
    ));

    // ast_enum! parsing
    pub struct AstEnum(pub Vec<AstItem>);
    impl Synom for AstEnum {
        named!(parse -> Self, do_parse!(
            many0!(Attribute::parse_outer) >>
            keyword!(pub) >>
            keyword!(enum) >>
            id: syn!(Ident) >>
            no_visit: option!(no_visit) >>
            rest: syn!(TokenStream) >>
            (AstEnum(if no_visit.is_some() {
                vec![]
            } else {
                vec![AstItem {
                    ast: syn::parse_str(&quote! {
                        pub enum #id #rest
                    }.to_string())?,
                    features: quote!(),
                    eos_full: false,
                }]
            }))
        ));
    }

    // A single variant of an ast_enum_of_structs!
    struct EosVariant {
        name: Ident,
        member: Option<Path>,
        inner: Option<AstItem>,
    }
    named!(eos_variant -> EosVariant, do_parse!(
        many0!(Attribute::parse_outer) >>
        keyword!(pub) >>
        variant: syn!(Ident) >>
        member: option!(map!(parens!(alt!(
            call!(ast_struct_inner) => { |x: AstItem| (Path::from(x.ast.ident.clone()), Some(x)) }
            |
            syn!(Path) => { |x| (x, None) }
        )), |x| x.1)) >>
        punct!(,) >>
        (EosVariant {
            name: variant,
            member: member.clone().map(|x| x.0),
            inner: member.map(|x| x.1).unwrap_or_default(),
        })
    ));

    // ast_enum_of_structs! parsing
    pub struct AstEnumOfStructs(pub Vec<AstItem>);
    impl Synom for AstEnumOfStructs {
        named!(parse -> Self, do_parse!(
            many0!(Attribute::parse_outer) >>
            keyword!(pub) >>
            keyword!(enum) >>
            id: syn!(Ident) >>
            variants: braces!(many0!(eos_variant)) >>
            option!(syn!(Ident)) >> // do_not_generate_to_tokens
            ({
                // XXX: This is really gross - we shouldn't have to convert the
                // tokens to strings to re-parse them.
                let enum_item = {
                    let variants = variants.1.iter().map(|v| {
                        let name = v.name.clone();
                        match v.member {
                            Some(ref member) => quote!(#name(#member)),
                            None => quote!(#name),
                        }
                    });
                    syn::parse_str(&quote! {
                        pub enum #id { #(#variants),* }
                    }.to_string())?
                };
                let mut items = vec![AstItem {
                    ast: enum_item,
                    features: quote!(),
                    eos_full:  false,
                }];
                items.extend(variants.1.into_iter().filter_map(|v| v.inner));
                AstEnumOfStructs(items)
            })
        ));
    }
}

mod codegen {
    use super::{AstItem, Lookup};
    use proc_macro2::{Span, TokenStream};
    use quote::ToTokens;
    use std::fmt::{self, Display};
    use syn::punctuated::Punctuated;
    use syn::*;

    #[derive(Default)]
    pub struct State {
        pub visit_trait: String,
        pub visit_impl: String,
        pub visit_mut_trait: String,
        pub visit_mut_impl: String,
        pub fold_trait: String,
        pub fold_impl: String,
    }

    fn under_name(name: Ident) -> Ident {
        use inflections::Inflect;
        Ident::new(&name.to_string().to_snake_case(), Span::call_site())
    }

    enum RelevantType<'a> {
        Box(&'a Type),
        Vec(&'a Type),
        Punctuated(&'a Type),
        Option(&'a Type),
        Tuple(&'a Punctuated<Type, Token![,]>),
        Simple(&'a AstItem),
        Token(TokenStream),
        Pass,
    }

    fn classify<'a>(ty: &'a Type, lookup: &'a Lookup) -> RelevantType<'a> {
        match *ty {
            Type::Path(TypePath {
                qself: None,
                ref path,
            }) => {
                let last = path.segments.last().unwrap().into_value();
                match &last.ident.to_string()[..] {
                    "Box" => RelevantType::Box(first_arg(&last.arguments)),
                    "Vec" => RelevantType::Vec(first_arg(&last.arguments)),
                    "Punctuated" => RelevantType::Punctuated(first_arg(&last.arguments)),
                    "Option" => RelevantType::Option(first_arg(&last.arguments)),
                    "Brace" | "Bracket" | "Paren" | "Group" => {
                        RelevantType::Token(last.ident.clone().into_token_stream())
                    }
                    _ => {
                        if let Some(item) = lookup.get(&last.ident) {
                            RelevantType::Simple(item)
                        } else {
                            RelevantType::Pass
                        }
                    }
                }
            }
            Type::Tuple(TypeTuple { ref elems, .. }) => RelevantType::Tuple(elems),
            Type::Macro(TypeMacro { ref mac })
                if mac.path.segments.last().unwrap().into_value().ident == "Token" =>
            {
                RelevantType::Token(mac.into_token_stream())
            }
            _ => RelevantType::Pass,
        }
    }

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    enum Kind {
        Visit,
        VisitMut,
        Fold,
    }

    enum Operand {
        Borrowed(TokenStream),
        Owned(TokenStream),
    }

    use self::Kind::*;
    use self::Operand::*;

    impl Operand {
        fn tokens(&self) -> &TokenStream {
            match *self {
                Borrowed(ref n) | Owned(ref n) => n,
            }
        }

        fn ref_tokens(&self) -> TokenStream {
            match *self {
                Borrowed(ref n) => n.clone(),
                Owned(ref n) => quote!(&#n),
            }
        }

        fn ref_mut_tokens(&self) -> TokenStream {
            match *self {
                Borrowed(ref n) => n.clone(),
                Owned(ref n) => quote!(&mut #n),
            }
        }

        fn owned_tokens(&self) -> TokenStream {
            match *self {
                Borrowed(ref n) => quote!(*#n),
                Owned(ref n) => n.clone(),
            }
        }
    }

    impl Display for Operand {
        fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            Display::fmt(self.tokens(), formatter)
        }
    }

    fn first_arg(params: &PathArguments) -> &Type {
        let data = match *params {
            PathArguments::AngleBracketed(ref data) => data,
            _ => panic!("Expected at least 1 type argument here"),
        };

        match **data
            .args
            .first()
            .expect("Expected at least 1 type argument here")
            .value()
        {
            GenericArgument::Type(ref ty) => ty,
            _ => panic!("Expected at least 1 type argument here"),
        }
    }

    fn simple_visit(item: &AstItem, kind: Kind, name: &Operand) -> String {
        match kind {
            Visit => format!(
                "_visitor.visit_{under_name}({name})",
                under_name = under_name(item.ast.ident.clone()),
                name = name.ref_tokens(),
            ),
            VisitMut => format!(
                "_visitor.visit_{under_name}_mut({name})",
                under_name = under_name(item.ast.ident.clone()),
                name = name.ref_mut_tokens(),
            ),
            Fold => format!(
                "_visitor.fold_{under_name}({name})",
                under_name = under_name(item.ast.ident.clone()),
                name = name.owned_tokens(),
            ),
        }
    }

    fn box_visit(elem: &Type, lookup: &Lookup, kind: Kind, name: &Operand) -> Option<String> {
        let name = name.owned_tokens();
        let res = visit(elem, lookup, kind, &Owned(quote!(*#name)))?;
        Some(match kind {
            Fold => format!("Box::new({})", res),
            Visit | VisitMut => res,
        })
    }

    fn vec_visit(elem: &Type, lookup: &Lookup, kind: Kind, name: &Operand) -> Option<String> {
        let operand = match kind {
            Visit | VisitMut => Borrowed(quote!(it)),
            Fold => Owned(quote!(it)),
        };
        let val = visit(elem, lookup, kind, &operand)?;
        Some(match kind {
            Visit => format!(
                "for it in {name} {{ {val} }}",
                name = name.ref_tokens(),
                val = val,
            ),
            VisitMut => format!(
                "for it in {name} {{ {val} }}",
                name = name.ref_mut_tokens(),
                val = val,
            ),
            Fold => format!(
                "FoldHelper::lift({name}, |it| {{ {val} }})",
                name = name.owned_tokens(),
                val = val,
            ),
        })
    }

    fn punctuated_visit(
        elem: &Type,
        lookup: &Lookup,
        kind: Kind,
        name: &Operand,
    ) -> Option<String> {
        let operand = match kind {
            Visit | VisitMut => Borrowed(quote!(it)),
            Fold => Owned(quote!(it)),
        };
        let val = visit(elem, lookup, kind, &operand)?;
        Some(match kind {
            Visit => format!(
                "for el in Punctuated::pairs({name}) {{ \
                 let it = el.value(); \
                 {val} \
                 }}",
                name = name.ref_tokens(),
                val = val,
            ),
            VisitMut => format!(
                "for mut el in Punctuated::pairs_mut({name}) {{ \
                 let it = el.value_mut(); \
                 {val} \
                 }}",
                name = name.ref_mut_tokens(),
                val = val,
            ),
            Fold => format!(
                "FoldHelper::lift({name}, |it| {{ {val} }})",
                name = name.owned_tokens(),
                val = val,
            ),
        })
    }

    fn option_visit(elem: &Type, lookup: &Lookup, kind: Kind, name: &Operand) -> Option<String> {
        let it = match kind {
            Visit | VisitMut => Borrowed(quote!(it)),
            Fold => Owned(quote!(it)),
        };
        let val = visit(elem, lookup, kind, &it)?;
        Some(match kind {
            Visit => format!(
                "if let Some(ref it) = {name} {{ {val} }}",
                name = name.owned_tokens(),
                val = val,
            ),
            VisitMut => format!(
                "if let Some(ref mut it) = {name} {{ {val} }}",
                name = name.owned_tokens(),
                val = val,
            ),
            Fold => format!(
                "({name}).map(|it| {{ {val} }})",
                name = name.owned_tokens(),
                val = val,
            ),
        })
    }

    fn tuple_visit(
        elems: &Punctuated<Type, Token![,]>,
        lookup: &Lookup,
        kind: Kind,
        name: &Operand,
    ) -> Option<String> {
        let mut code = String::new();
        for (i, elem) in elems.iter().enumerate() {
            let name = name.tokens();
            let i = Index::from(i);
            let it = Owned(quote!((#name).#i));
            let val = visit(elem, lookup, kind, &it).unwrap_or_else(|| noop_visit(kind, &it));
            code.push_str(&format!("            {}", val));
            match kind {
                Fold => code.push(','),
                Visit | VisitMut => code.push(';'),
            }
            code.push('\n');
        }
        if code.is_empty() {
            None
        } else {
            Some(match kind {
                Fold => format!("(\n{}        )", code),
                Visit | VisitMut => format!("\n{}        ", code),
            })
        }
    }

    fn token_visit(ty: TokenStream, kind: Kind, name: &Operand) -> String {
        match kind {
            Fold => format!(
                "{ty}(tokens_helper(_visitor, &({name}).0))",
                ty = ty,
                name = name.owned_tokens(),
            ),
            Visit => format!(
                "tokens_helper(_visitor, &({name}).0)",
                name = name.ref_tokens(),
            ),
            VisitMut => format!(
                "tokens_helper(_visitor, &mut ({name}).0)",
                name = name.ref_mut_tokens(),
            ),
        }
    }

    fn noop_visit(kind: Kind, name: &Operand) -> String {
        match kind {
            Fold => name.owned_tokens().to_string(),
            Visit | VisitMut => format!("// Skipped field {}", name),
        }
    }

    fn visit(ty: &Type, lookup: &Lookup, kind: Kind, name: &Operand) -> Option<String> {
        match classify(ty, lookup) {
            RelevantType::Box(elem) => box_visit(elem, lookup, kind, name),
            RelevantType::Vec(elem) => vec_visit(elem, lookup, kind, name),
            RelevantType::Punctuated(elem) => punctuated_visit(elem, lookup, kind, name),
            RelevantType::Option(elem) => option_visit(elem, lookup, kind, name),
            RelevantType::Tuple(elems) => tuple_visit(elems, lookup, kind, name),
            RelevantType::Simple(item) => {
                let mut res = simple_visit(item, kind, name);
                Some(if item.eos_full {
                    format!("full!({res})", res = res)
                } else {
                    res
                })
            }
            RelevantType::Token(ty) => Some(token_visit(ty, kind, name)),
            RelevantType::Pass => None,
        }
    }

    pub fn generate(state: &mut State, lookup: &Lookup, s: &AstItem) {
        let under_name = under_name(s.ast.ident.clone());

        state.visit_trait.push_str(&format!(
            "{features}\n\
             fn visit_{under_name}(&mut self, i: &'ast {ty}) {{ \
             visit_{under_name}(self, i) \
             }}\n",
            features = s.features,
            under_name = under_name,
            ty = s.ast.ident,
        ));
        state.visit_mut_trait.push_str(&format!(
            "{features}\n\
             fn visit_{under_name}_mut(&mut self, i: &mut {ty}) {{ \
             visit_{under_name}_mut(self, i) \
             }}\n",
            features = s.features,
            under_name = under_name,
            ty = s.ast.ident,
        ));
        state.fold_trait.push_str(&format!(
            "{features}\n\
             fn fold_{under_name}(&mut self, i: {ty}) -> {ty} {{ \
             fold_{under_name}(self, i) \
             }}\n",
            features = s.features,
            under_name = under_name,
            ty = s.ast.ident,
        ));

        state.visit_impl.push_str(&format!(
            "{features}\n\
             pub fn visit_{under_name}<'ast, V: Visit<'ast> + ?Sized>(\
             _visitor: &mut V, _i: &'ast {ty}) {{\n",
            features = s.features,
            under_name = under_name,
            ty = s.ast.ident,
        ));
        state.visit_mut_impl.push_str(&format!(
            "{features}\n\
             pub fn visit_{under_name}_mut<V: VisitMut + ?Sized>(\
             _visitor: &mut V, _i: &mut {ty}) {{\n",
            features = s.features,
            under_name = under_name,
            ty = s.ast.ident,
        ));
        let before_fold_impl_len = state.fold_impl.len();
        state.fold_impl.push_str(&format!(
            "{features}\n\
             pub fn fold_{under_name}<V: Fold + ?Sized>(\
             _visitor: &mut V, _i: {ty}) -> {ty} {{\n",
            features = s.features,
            under_name = under_name,
            ty = s.ast.ident,
        ));

        // XXX:  This part is a disaster - I'm not sure how to make it cleaner though :'(
        match s.ast.data {
            Data::Enum(ref e) => {
                state.visit_impl.push_str("    match *_i {\n");
                state.visit_mut_impl.push_str("    match *_i {\n");
                state.fold_impl.push_str("    match _i {\n");
                for variant in &e.variants {
                    let fields: Vec<(&Field, TokenStream)> = match variant.fields {
                        Fields::Named(..) => panic!("Doesn't support enum struct variants"),
                        Fields::Unnamed(ref fields) => {
                            let binding = format!("        {}::{}(", s.ast.ident, variant.ident);
                            state.visit_impl.push_str(&binding);
                            state.visit_mut_impl.push_str(&binding);
                            state.fold_impl.push_str(&binding);

                            let res = fields
                                .unnamed
                                .iter()
                                .enumerate()
                                .map(|(idx, el)| {
                                    let name = format!("_binding_{}", idx);

                                    state.visit_impl.push_str("ref ");
                                    state.visit_mut_impl.push_str("ref mut ");

                                    state.visit_impl.push_str(&name);
                                    state.visit_mut_impl.push_str(&name);
                                    state.fold_impl.push_str(&name);
                                    state.visit_impl.push_str(", ");
                                    state.visit_mut_impl.push_str(", ");
                                    state.fold_impl.push_str(", ");

                                    let mut tokens = quote!();
                                    Ident::new(&name, Span::call_site()).to_tokens(&mut tokens);

                                    (el, tokens)
                                })
                                .collect();

                            state.visit_impl.push_str(") => {\n");
                            state.visit_mut_impl.push_str(") => {\n");
                            state.fold_impl.push_str(") => {\n");

                            res
                        }
                        Fields::Unit => {
                            state.visit_impl.push_str(&format!(
                                "        {0}::{1} => {{ }}\n",
                                s.ast.ident, variant.ident
                            ));
                            state.visit_mut_impl.push_str(&format!(
                                "        {0}::{1} => {{ }}\n",
                                s.ast.ident, variant.ident
                            ));
                            state.fold_impl.push_str(&format!(
                                "        {0}::{1} => {{ {0}::{1} }}\n",
                                s.ast.ident, variant.ident
                            ));
                            continue;
                        }
                    };

                    if fields.is_empty() {
                        state.visit_impl.push_str("            {}");
                        state.visit_mut_impl.push_str(") => {\n");
                        state.fold_impl.push_str(") => {\n");
                    }
                    state.fold_impl.push_str(&format!(
                        "            {}::{} (\n",
                        s.ast.ident, variant.ident,
                    ));
                    for (field, binding) in fields {
                        state.visit_impl.push_str(&format!(
                            "            {};\n",
                            visit(&field.ty, lookup, Visit, &Borrowed(binding.clone()))
                                .unwrap_or_else(|| noop_visit(Visit, &Borrowed(binding.clone()))),
                        ));
                        state.visit_mut_impl.push_str(&format!(
                            "            {};\n",
                            visit(&field.ty, lookup, VisitMut, &Borrowed(binding.clone()))
                                .unwrap_or_else(|| noop_visit(
                                    VisitMut,
                                    &Borrowed(binding.clone())
                                )),
                        ));
                        state.fold_impl.push_str(&format!(
                            "                {},\n",
                            visit(&field.ty, lookup, Fold, &Owned(binding.clone()))
                                .unwrap_or_else(|| noop_visit(Fold, &Owned(binding))),
                        ));
                    }
                    state.fold_impl.push_str("            )\n");

                    state.visit_impl.push_str("        }\n");
                    state.visit_mut_impl.push_str("        }\n");
                    state.fold_impl.push_str("        }\n");
                }
                state.visit_impl.push_str("    }\n");
                state.visit_mut_impl.push_str("    }\n");
                state.fold_impl.push_str("    }\n");
            }
            Data::Struct(ref v) => {
                let fields: Vec<(&Field, TokenStream)> = match v.fields {
                    Fields::Named(ref fields) => {
                        state
                            .fold_impl
                            .push_str(&format!("    {} {{\n", s.ast.ident));
                        fields
                            .named
                            .iter()
                            .map(|el| {
                                let id = el.ident.clone();
                                (el, quote!(_i.#id))
                            })
                            .collect()
                    }
                    Fields::Unnamed(ref fields) => {
                        state
                            .fold_impl
                            .push_str(&format!("    {} (\n", s.ast.ident));
                        fields
                            .unnamed
                            .iter()
                            .enumerate()
                            .map(|(idx, el)| {
                                let id = Index::from(idx);
                                (el, quote!(_i.#id))
                            })
                            .collect()
                    }
                    Fields::Unit => {
                        if s.ast.ident == "Ident" {
                            state.fold_impl.push_str("    let mut _i = _i;\n");
                            state.fold_impl.push_str("    let span = _visitor.fold_span(_i.span());\n");
                            state.fold_impl.push_str("    _i.set_span(span);\n");
                        }
                        state.fold_impl.push_str("    _i\n");
                        vec![]
                    }
                };

                for (field, ref_toks) in fields {
                    let ref_toks = Owned(ref_toks);
                    state.visit_impl.push_str(&format!(
                        "    {};\n",
                        visit(&field.ty, lookup, Visit, &ref_toks)
                            .unwrap_or_else(|| noop_visit(Visit, &ref_toks,))
                    ));
                    state.visit_mut_impl.push_str(&format!(
                        "    {};\n",
                        visit(&field.ty, lookup, VisitMut, &ref_toks)
                            .unwrap_or_else(|| noop_visit(VisitMut, &ref_toks,))
                    ));
                    let fold = visit(&field.ty, lookup, Fold, &ref_toks)
                        .unwrap_or_else(|| noop_visit(Fold, &ref_toks));
                    if let Some(ref name) = field.ident {
                        state
                            .fold_impl
                            .push_str(&format!("        {}: {},\n", name, fold));
                    } else {
                        state.fold_impl.push_str(&format!("        {},\n", fold));
                    }
                }

                match v.fields {
                    Fields::Named(..) => state.fold_impl.push_str("    }\n"),
                    Fields::Unnamed(..) => state.fold_impl.push_str("    )\n"),
                    Fields::Unit => {}
                };
            }
            Data::Union(..) => panic!("Union not supported"),
        }

        // Close the impl body
        state.visit_impl.push_str("}\n");
        state.visit_mut_impl.push_str("}\n");
        state.fold_impl.push_str("}\n");

        if let Data::Struct(ref data) = s.ast.data {
            if let Fields::Named(ref fields) = data.fields {
                if fields
                    .named
                    .iter()
                    .any(|field| field.vis == Visibility::Inherited)
                {
                    // Discard the generated impl if there are private fields.
                    // These have to be handwritten.
                    state.fold_impl.truncate(before_fold_impl_len);
                }
            }
        }
    }
}

fn main() {
    let mut lookup = BTreeMap::new();
    load_file(SYN_CRATE_ROOT, &quote!(), &mut lookup).unwrap();

    // Load in any terminal types
    for &tt in TERMINAL_TYPES {
        use syn::*;
        lookup.insert(
            Ident::new(&tt, Span::call_site()),
            AstItem {
                ast: DeriveInput {
                    ident: Ident::new(tt, Span::call_site()),
                    vis: Visibility::Public(VisPublic {
                        pub_token: Default::default(),
                    }),
                    attrs: vec![],
                    generics: Default::default(),
                    data: Data::Struct(DataStruct {
                        fields: Fields::Unit,
                        struct_token: Default::default(),
                        semi_token: None,
                    }),
                },
                features: TokenStream::new(),
                eos_full: false,
            },
        );
    }

    let mut state = Default::default();
    for s in lookup.values() {
        codegen::generate(&mut state, &lookup, s);
    }

    let full_macro = "
#[cfg(feature = \"full\")]
macro_rules! full {
    ($e:expr) => { $e }
}

#[cfg(all(feature = \"derive\", not(feature = \"full\")))]
macro_rules! full {
    ($e:expr) => { unreachable!() }
}
";

    let mut fold_file = File::create(FOLD_SRC).unwrap();
    write!(
        fold_file,
        "\
// THIS FILE IS AUTOMATICALLY GENERATED; DO NOT EDIT

#![cfg_attr(rustfmt, rustfmt_skip)]

// Unreachable code is generated sometimes without the full feature.
#![allow(unreachable_code)]
#![cfg_attr(feature = \"cargo-clippy\", allow(needless_pass_by_value))]

use *;
#[cfg(any(feature = \"full\", feature = \"derive\"))]
use token::{{Brace, Bracket, Paren, Group}};
use proc_macro2::Span;
#[cfg(any(feature = \"full\", feature = \"derive\"))]
use gen::helper::fold::*;

{full_macro}

/// Syntax tree traversal to transform the nodes of an owned syntax tree.
///
/// See the [module documentation] for details.
///
/// [module documentation]: index.html
///
/// *This trait is available if Syn is built with the `\"fold\"` feature.*
pub trait Fold {{
{fold_trait}
}}

#[cfg(any(feature = \"full\", feature = \"derive\"))]
macro_rules! fold_span_only {{
    ($f:ident : $t:ident) => {{
        pub fn $f<V: Fold + ?Sized>(_visitor: &mut V, mut _i: $t) -> $t {{
            let span = _visitor.fold_span(_i.span());
            _i.set_span(span);
            _i
        }}
    }}
}}

#[cfg(any(feature = \"full\", feature = \"derive\"))]
fold_span_only!(fold_lit_byte: LitByte);
#[cfg(any(feature = \"full\", feature = \"derive\"))]
fold_span_only!(fold_lit_byte_str: LitByteStr);
#[cfg(any(feature = \"full\", feature = \"derive\"))]
fold_span_only!(fold_lit_char: LitChar);
#[cfg(any(feature = \"full\", feature = \"derive\"))]
fold_span_only!(fold_lit_float: LitFloat);
#[cfg(any(feature = \"full\", feature = \"derive\"))]
fold_span_only!(fold_lit_int: LitInt);
#[cfg(any(feature = \"full\", feature = \"derive\"))]
fold_span_only!(fold_lit_str: LitStr);

{fold_impl}
",
        full_macro = full_macro,
        fold_trait = state.fold_trait,
        fold_impl = state.fold_impl
    ).unwrap();

    let mut visit_file = File::create(VISIT_SRC).unwrap();
    write!(
        visit_file,
        "\
// THIS FILE IS AUTOMATICALLY GENERATED; DO NOT EDIT

#![cfg_attr(rustfmt, rustfmt_skip)]

#![cfg_attr(feature = \"cargo-clippy\", allow(match_same_arms))]

use *;
#[cfg(any(feature = \"full\", feature = \"derive\"))]
use punctuated::Punctuated;
use proc_macro2::Span;
#[cfg(any(feature = \"full\", feature = \"derive\"))]
use gen::helper::visit::*;

{full_macro}

/// Syntax tree traversal to walk a shared borrow of a syntax tree.
///
/// See the [module documentation] for details.
///
/// [module documentation]: index.html
///
/// *This trait is available if Syn is built with the `\"visit\"` feature.*
pub trait Visit<'ast> {{
{visit_trait}
}}

{visit_impl}
",
        full_macro = full_macro,
        visit_trait = state.visit_trait,
        visit_impl = state.visit_impl
    ).unwrap();

    let mut visit_mut_file = File::create(VISIT_MUT_SRC).unwrap();
    write!(
        visit_mut_file,
        "\
// THIS FILE IS AUTOMATICALLY GENERATED; DO NOT EDIT

#![cfg_attr(rustfmt, rustfmt_skip)]

#![cfg_attr(feature = \"cargo-clippy\", allow(match_same_arms))]

use *;
#[cfg(any(feature = \"full\", feature = \"derive\"))]
use punctuated::Punctuated;
use proc_macro2::Span;
#[cfg(any(feature = \"full\", feature = \"derive\"))]
use gen::helper::visit_mut::*;

{full_macro}

/// Syntax tree traversal to mutate an exclusive borrow of a syntax tree in
/// place.
///
/// See the [module documentation] for details.
///
/// [module documentation]: index.html
///
/// *This trait is available if Syn is built with the `\"visit-mut\"` feature.*
pub trait VisitMut {{
{visit_mut_trait}
}}

{visit_mut_impl}
",
        full_macro = full_macro,
        visit_mut_trait = state.visit_mut_trait,
        visit_mut_impl = state.visit_mut_impl
    ).unwrap();
}
