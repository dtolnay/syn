use crate::error::Result;
use crate::version;

use indexmap::IndexMap;
use quote::quote;
use syn::parse::Parser;
use syn::{parse_quote, Data, DataStruct, DeriveInput, Ident, Item};
use syn_codegen as types;

use std::collections::BTreeMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

const SYN_CRATE_ROOT: &str = "../src/lib.rs";
const TOKEN_SRC: &str = "../src/token.rs";
const IGNORED_MODS: &[&str] = &["fold", "visit", "visit_mut"];
const EXTRA_TYPES: &[&str] = &["Lifetime"];

// NOTE: BTreeMap is used here instead of HashMap to have deterministic output.
type ItemLookup = BTreeMap<Ident, AstItem>;
type TokenLookup = BTreeMap<String, String>;

/// Parse the contents of `src` and return a list of AST types.
pub fn parse() -> Result<types::Definitions> {
    let mut item_lookup = BTreeMap::new();
    load_file(SYN_CRATE_ROOT, &[], &mut item_lookup)?;

    let token_lookup = load_token_file(TOKEN_SRC)?;

    let version = version::get()?;

    let types = item_lookup
        .values()
        .map(|item| introspect_item(item, &item_lookup, &token_lookup))
        .collect();

    let tokens = token_lookup
        .into_iter()
        .map(|(name, ty)| (ty, name))
        .collect();

    Ok(types::Definitions {
        version,
        types,
        tokens,
    })
}

/// Data extracted from syn source
#[derive(Clone)]
pub struct AstItem {
    ast: DeriveInput,
    features: Vec<syn::Attribute>,
}

fn introspect_item(item: &AstItem, items: &ItemLookup, tokens: &TokenLookup) -> types::Node {
    let features = introspect_features(&item.features);

    match &item.ast.data {
        Data::Enum(ref data) => types::Node {
            ident: item.ast.ident.to_string(),
            features,
            data: types::Data::Enum(introspect_enum(data, items, tokens)),
        },
        Data::Struct(ref data) => types::Node {
            ident: item.ast.ident.to_string(),
            features,
            data: {
                if data.fields.iter().all(|f| is_pub(&f.vis)) {
                    types::Data::Struct(introspect_struct(data, items, tokens))
                } else {
                    types::Data::Private
                }
            },
        },
        Data::Union(..) => panic!("Union not supported"),
    }
}

fn introspect_enum(
    item: &syn::DataEnum,
    items: &ItemLookup,
    tokens: &TokenLookup,
) -> types::Variants {
    item.variants
        .iter()
        .map(|variant| {
            let fields = match &variant.fields {
                syn::Fields::Unnamed(fields) => fields
                    .unnamed
                    .iter()
                    .map(|field| introspect_type(&field.ty, items, tokens))
                    .collect(),
                syn::Fields::Unit => vec![],
                _ => panic!("Enum representation not supported"),
            };

            (variant.ident.to_string(), fields)
        })
        .collect()
}

fn introspect_struct(
    item: &syn::DataStruct,
    items: &ItemLookup,
    tokens: &TokenLookup,
) -> types::Fields {
    match &item.fields {
        syn::Fields::Named(fields) => fields
            .named
            .iter()
            .map(|field| {
                (
                    field.ident.as_ref().unwrap().to_string(),
                    introspect_type(&field.ty, items, tokens),
                )
            })
            .collect(),
        syn::Fields::Unit => IndexMap::new(),
        _ => panic!("Struct representation not supported"),
    }
}

fn introspect_type(item: &syn::Type, items: &ItemLookup, tokens: &TokenLookup) -> types::Type {
    match item {
        syn::Type::Path(syn::TypePath {
            qself: None,
            ref path,
        }) => {
            let last = path.segments.last().unwrap().into_value();
            let string = last.ident.to_string();

            match string.as_str() {
                "Option" => {
                    let nested = introspect_type(first_arg(&last.arguments), items, tokens);
                    types::Type::Option(Box::new(nested))
                }
                "Punctuated" => {
                    let nested = introspect_type(first_arg(&last.arguments), items, tokens);
                    let punct = match introspect_type(last_arg(&last.arguments), items, tokens) {
                        types::Type::Token(s) => s,
                        _ => panic!(),
                    };

                    types::Type::Punctuated(types::Punctuated {
                        element: Box::new(nested),
                        punct,
                    })
                }
                "Vec" => {
                    let nested = introspect_type(first_arg(&last.arguments), items, tokens);
                    types::Type::Vec(Box::new(nested))
                }
                "Box" => {
                    let nested = introspect_type(first_arg(&last.arguments), items, tokens);
                    types::Type::Box(Box::new(nested))
                }
                "Brace" | "Bracket" | "Paren" | "Group" => types::Type::Group(string),
                "TokenStream" | "Literal" | "Ident" | "Span" => types::Type::Ext(string),
                "String" | "u32" | "usize" | "bool" => types::Type::Std(string),
                _ => {
                    if items.get(&last.ident).is_some() {
                        types::Type::Syn(string)
                    } else {
                        unimplemented!("{}", string);
                    }
                }
            }
        }
        syn::Type::Tuple(syn::TypeTuple { ref elems, .. }) => {
            let tys = elems
                .iter()
                .map(|ty| introspect_type(&ty, items, tokens))
                .collect();
            types::Type::Tuple(tys)
        }
        syn::Type::Macro(syn::TypeMacro { ref mac })
            if mac.path.segments.last().unwrap().into_value().ident == "Token" =>
        {
            let content = mac.tts.to_string();
            let ty = tokens.get(&content).unwrap().to_string();

            types::Type::Token(ty)
        }
        _ => panic!("{}", quote!(#item).to_string()),
    }
}

fn introspect_features(attrs: &[syn::Attribute]) -> types::Features {
    let mut ret = types::Features::default();

    for attr in attrs {
        if !attr.path.is_ident("cfg") {
            continue;
        }

        let features = parsing::parse_features.parse2(attr.tts.clone()).unwrap();

        if ret.any.is_empty() {
            ret = features;
        } else if ret.any.len() < features.any.len() {
            assert!(ret.any.iter().all(|f| features.any.contains(f)));
        } else {
            assert!(features.any.iter().all(|f| ret.any.contains(f)));
            ret = features;
        }
    }

    ret
}

fn is_pub(vis: &syn::Visibility) -> bool {
    match vis {
        syn::Visibility::Public(_) => true,
        _ => false,
    }
}

fn first_arg(params: &syn::PathArguments) -> &syn::Type {
    let data = match *params {
        syn::PathArguments::AngleBracketed(ref data) => data,
        _ => panic!("Expected at least 1 type argument here"),
    };

    match **data
        .args
        .first()
        .expect("Expected at least 1 type argument here")
        .value()
    {
        syn::GenericArgument::Type(ref ty) => ty,
        _ => panic!("Expected at least 1 type argument here"),
    }
}

fn last_arg(params: &syn::PathArguments) -> &syn::Type {
    let data = match *params {
        syn::PathArguments::AngleBracketed(ref data) => data,
        _ => panic!("Expected at least 1 type argument here"),
    };

    match **data
        .args
        .last()
        .expect("Expected at least 1 type argument here")
        .value()
    {
        syn::GenericArgument::Type(ref ty) => ty,
        _ => panic!("Expected at least 1 type argument here"),
    }
}

mod parsing {
    use super::{AstItem, TokenLookup};

    use proc_macro2::TokenStream;
    use quote::quote;
    use syn;
    use syn::parse::{Parse, ParseStream, Result};
    use syn::*;
    use syn_codegen as types;

    use std::collections::{BTreeMap, BTreeSet};

    fn peek_tag(input: ParseStream, tag: &str) -> bool {
        let ahead = input.fork();
        ahead.parse::<Token![#]>().is_ok()
            && ahead
                .parse::<Ident>()
                .map(|ident| ident == tag)
                .unwrap_or(false)
    }

    // Parses #full - returns #[cfg(feature = "full")] if it is present, and
    // nothing otherwise.
    fn full(input: ParseStream) -> Vec<syn::Attribute> {
        if peek_tag(input, "full") {
            input.parse::<Token![#]>().unwrap();
            input.parse::<Ident>().unwrap();
            vec![parse_quote!(#[cfg(feature = "full")])]
        } else {
            vec![]
        }
    }

    fn skip_manual_extra_traits(input: ParseStream) {
        if peek_tag(input, "manual_extra_traits") {
            input.parse::<Token![#]>().unwrap();
            input.parse::<Ident>().unwrap();
        }
    }

    // Parses a simple AstStruct without the `pub struct` prefix.
    fn ast_struct_inner(input: ParseStream) -> Result<AstItem> {
        let ident: Ident = input.parse()?;
        let features = full(input);
        skip_manual_extra_traits(input);
        let rest: TokenStream = input.parse()?;
        Ok(AstItem {
            ast: syn::parse2(quote! {
                pub struct #ident #rest
            })?,
            features,
        })
    }

    // ast_struct! parsing
    pub struct AstStruct(pub(super) Vec<AstItem>);
    impl Parse for AstStruct {
        fn parse(input: ParseStream) -> Result<Self> {
            input.call(Attribute::parse_outer)?;
            input.parse::<Token![pub]>()?;
            input.parse::<Token![struct]>()?;
            let res = input.call(ast_struct_inner)?;
            Ok(AstStruct(vec![res]))
        }
    }

    fn no_visit(input: ParseStream) -> bool {
        if peek_tag(input, "no_visit") {
            input.parse::<Token![#]>().unwrap();
            input.parse::<Ident>().unwrap();
            true
        } else {
            false
        }
    }

    // ast_enum! parsing
    pub struct AstEnum(pub Vec<AstItem>);
    impl Parse for AstEnum {
        fn parse(input: ParseStream) -> Result<Self> {
            input.call(Attribute::parse_outer)?;
            input.parse::<Token![pub]>()?;
            input.parse::<Token![enum]>()?;
            let ident: Ident = input.parse()?;
            let no_visit = no_visit(input);
            let rest: TokenStream = input.parse()?;
            Ok(AstEnum(if no_visit {
                vec![]
            } else {
                vec![AstItem {
                    ast: syn::parse2(quote! {
                        pub enum #ident #rest
                    })?,
                    features: vec![],
                }]
            }))
        }
    }

    // A single variant of an ast_enum_of_structs!
    struct EosVariant {
        name: Ident,
        member: Option<Path>,
        inner: Option<AstItem>,
    }
    fn eos_variant(input: ParseStream) -> Result<EosVariant> {
        input.call(Attribute::parse_outer)?;
        input.parse::<Token![pub]>()?;
        let variant: Ident = input.parse()?;
        let (member, inner) = if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            if content.fork().call(ast_struct_inner).is_ok() {
                let item = content.call(ast_struct_inner)?;
                (Some(Path::from(item.ast.ident.clone())), Some(item))
            } else {
                let path: Path = content.parse()?;
                (Some(path), None)
            }
        } else {
            (None, None)
        };
        input.parse::<Token![,]>()?;
        Ok(EosVariant {
            name: variant,
            member,
            inner,
        })
    }

    // ast_enum_of_structs! parsing
    pub struct AstEnumOfStructs(pub Vec<AstItem>);
    impl Parse for AstEnumOfStructs {
        fn parse(input: ParseStream) -> Result<Self> {
            input.call(Attribute::parse_outer)?;
            input.parse::<Token![pub]>()?;
            input.parse::<Token![enum]>()?;
            let ident: Ident = input.parse()?;

            let content;
            braced!(content in input);
            let mut variants = Vec::new();
            while !content.is_empty() {
                variants.push(content.call(eos_variant)?);
            }

            if let Some(ident) = input.parse::<Option<Ident>>()? {
                assert_eq!(ident, "do_not_generate_to_tokens");
            }

            let enum_item = {
                let variants = variants.iter().map(|v| {
                    let name = v.name.clone();
                    match v.member {
                        Some(ref member) => quote!(#name(#member)),
                        None => quote!(#name),
                    }
                });
                parse_quote! {
                    pub enum #ident {
                        #(#variants),*
                    }
                }
            };
            let mut items = vec![AstItem {
                ast: enum_item,
                features: vec![],
            }];
            items.extend(variants.into_iter().filter_map(|v| v.inner));
            Ok(AstEnumOfStructs(items))
        }
    }

    pub struct TokenMacro(pub TokenLookup);
    impl Parse for TokenMacro {
        fn parse(input: ParseStream) -> Result<Self> {
            let mut tokens = BTreeMap::new();
            while !input.is_empty() {
                let content;
                parenthesized!(content in input);
                let token = content.parse::<TokenStream>()?.to_string();
                input.parse::<Token![=]>()?;
                input.parse::<Token![>]>()?;
                let content;
                braced!(content in input);
                input.parse::<Token![;]>()?;
                content.parse::<Token![$]>()?;
                let path: Path = content.parse()?;
                let ty = path.segments.last().unwrap().into_value().ident.to_string();
                tokens.insert(token, ty.to_string());
            }
            Ok(TokenMacro(tokens))
        }
    }

    fn parse_feature(input: ParseStream) -> Result<String> {
        let i: syn::Ident = input.parse()?;
        assert_eq!(i, "feature");

        input.parse::<Token![=]>()?;
        let s = input.parse::<syn::LitStr>()?;

        Ok(s.value())
    }

    pub fn parse_features(input: ParseStream) -> Result<types::Features> {
        let mut features = BTreeSet::new();

        let level_1;
        parenthesized!(level_1 in input);

        let i: syn::Ident = level_1.fork().parse()?;

        if i == "any" {
            level_1.parse::<syn::Ident>()?;

            let level_2;
            parenthesized!(level_2 in level_1);

            while !level_2.is_empty() {
                features.insert(parse_feature(&level_2)?);

                if !level_2.is_empty() {
                    level_2.parse::<Token![,]>()?;
                }
            }
        } else if i == "feature" {
            features.insert(parse_feature(&level_1)?);
            assert!(level_1.is_empty());
        } else {
            panic!("{:?}", i);
        }

        assert!(input.is_empty());

        Ok(types::Features { any: features })
    }
}

fn get_features(attrs: &[syn::Attribute], base: &[syn::Attribute]) -> Vec<syn::Attribute> {
    let mut ret = base.to_owned();

    for attr in attrs {
        if attr.path.is_ident("cfg") {
            ret.push(attr.clone());
        }
    }

    ret
}

fn load_file<P: AsRef<Path>>(
    name: P,
    features: &[syn::Attribute],
    lookup: &mut ItemLookup,
) -> Result<()> {
    let name = name.as_ref();
    let parent = name.parent().expect("no parent path");

    let mut f = File::open(name)?;
    let mut src = String::new();
    f.read_to_string(&mut src)?;

    // Parse the file
    let file = syn::parse_file(&src)?;

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
                    vec![parse_quote!(#[cfg(feature = "derive")])]
                } else {
                    get_features(&item.attrs, features)
                };

                // Look up the submodule file, and recursively parse it.
                // XXX: Only handles same-directory .rs file submodules.
                let path = parent.join(&format!("{}.rs", item.ident));
                load_file(path, &features, lookup)?;
            }
            Item::Macro(item) => {
                // Lookip any #[cfg()] attributes directly on the macro
                // invocation, and add them to the feature set.
                let features = get_features(&item.attrs, features);

                // Try to parse the AstItem declaration out of the item.
                let tts = &item.mac.tts;
                let found = if item.mac.path.is_ident("ast_struct") {
                    syn::parse2::<parsing::AstStruct>(quote!(#tts))?.0
                } else if item.mac.path.is_ident("ast_enum") {
                    syn::parse2::<parsing::AstEnum>(quote!(#tts))?.0
                } else if item.mac.path.is_ident("ast_enum_of_structs") {
                    syn::parse2::<parsing::AstEnumOfStructs>(quote!(#tts))?.0
                } else {
                    continue;
                };

                // Record our features on the parsed AstItems.
                for mut item in found {
                    item.features.extend(features.clone());
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
                                ident,
                                vis: item.vis,
                                attrs: item.attrs,
                                generics: item.generics,
                                data: Data::Struct(DataStruct {
                                    fields: item.fields,
                                    struct_token: item.struct_token,
                                    semi_token: item.semi_token,
                                }),
                            },
                            features: features.to_owned(),
                        },
                    );
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn load_token_file<P: AsRef<Path>>(name: P) -> Result<TokenLookup> {
    let name = name.as_ref();
    let mut f = File::open(name)?;
    let mut src = String::new();
    f.read_to_string(&mut src)?;
    let file = syn::parse_file(&src)?;
    for item in file.items {
        match item {
            Item::Macro(item) => {
                match item.ident {
                    Some(ref i) if i == "Token" => {}
                    _ => continue,
                }
                let tts = &item.mac.tts;
                let tokens = syn::parse2::<parsing::TokenMacro>(quote!(#tts))?.0;
                return Ok(tokens);
            }
            _ => {}
        }
    }

    panic!("failed to parse Token macro")
}
