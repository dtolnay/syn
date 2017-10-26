//! This crate automatically generates the definition of the Visitor,
//! VisitorMut, and Folder traits in `syn` based on the `syn` source. It
//! discovers structs and enums declared with the `ast_*` macros and generates
//! the functions for those types.
//!
//! It makes a few assumptions about the target crate:
//! 1. All structs which are discovered must be re-exported in the root of the
//!    crate, even if they were declared in a submodule.
//! 2. This code cannot discover submodules which are located in subdirectories
//!    - only submodules located in the same directory.
//! 3. The path to `syn` is hardcoded.

extern crate syn;
#[macro_use] extern crate synom;
#[macro_use] extern crate quote;
extern crate inflections;

use quote::{Tokens, ToTokens};
use syn::{ItemKind, Attribute, DeriveInput, Ident};

use std::io::{self, Read, Write};
use std::fs::File;
use std::path::Path;
use std::collections::BTreeMap;

const SYN_CRATE_ROOT: &str = "../src/lib.rs";

const FOLD_SRC: &str = "../src/gen/fold.rs";
const VISIT_SRC: &str = "../src/gen/visit.rs";
const VISIT_MUT_SRC: &str = "../src/gen/visit_mut.rs";

const IGNORED_MODS: &[&str] = &[
    "fold",
    "visit",
    "visit_mut",
];

fn path_eq(a: &syn::Path, b: &syn::Path) -> bool {
    if a.global() != b.global() || a.segments.len() != b.segments.len() {
        return false;
    }
    a.segments.iter().zip(b.segments.iter())
        .all(|(a, b)| a.item().ident.as_ref() == b.item().ident.as_ref())
}

fn get_features(attrs: &[Attribute], mut features: Tokens) -> Tokens {
    for attr in attrs {
        if path_eq(&attr.path, &"cfg".into()) {
            attr.to_tokens(&mut features);
        }
    }
    features
}

#[derive(Clone)]
pub struct AstItem {
    item: DeriveInput,
    features: Tokens,
    // True if this is an ast_enum_of_structs! item with a #full annotation.
    eos_full: bool,
}

use std::fmt;
impl fmt::Debug for AstItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("AstItem")
            .field("item", &self.item)
            .field("features", &self.features.to_string())
            .finish()
    }
}

// NOTE: BTreeMap is used here instead of HashMap to have deterministic output.
type Lookup<'a> = BTreeMap<Ident, AstItem>;

fn load_file<P: AsRef<Path>>(
    name: P,
    features: Tokens,
    lookup: &mut Lookup,
) -> Result<(), io::Error> {
    let name = name.as_ref();
    let parent = name.parent().ok_or(io::ErrorKind::Other)?;

    let mut f = File::open(name)?;
    let mut src = String::new();
    f.read_to_string(&mut src)?;

    // Parse the file
    let file = syn::parse_file(&src).map_err(|_| io::ErrorKind::Other)?;

    // Collect all of the interesting AstItems declared in this file or submodules.
    'items: for item in &file.items {
        match item.node {
            ItemKind::Mod(ref module) => {
                // Don't inspect inline modules.
                if module.content.is_some() {
                    continue;
                }

                // We don't want to try to load the generated rust files and
                // parse them, so we ignore them here.
                for name in IGNORED_MODS {
                    if module.ident.as_ref() == *name {
                        continue 'items;
                    }
                }

                // Lookup any #[cfg()] attributes on the module and add them to
                // the feature set.
                let features = get_features(&item.attrs, features.clone());

                // Look up the submodule file, and recursively parse it.
                // XXX: Only handles same-directory .rs file submodules.
                let path = parent.join(&format!("{}.rs", module.ident.as_ref()));
                load_file(path, features, lookup)?;
            }
            ItemKind::Mac(ref mac) => {
                // Lookip any #[cfg()] attributes directly on the macro
                // invocation, and add them to the feature set.
                let features = get_features(&item.attrs, features.clone());

                // Try to parse the AstItem declaration out of the item.
                let found = if path_eq(&mac.path, &"ast_struct".into()) {
                    syn::parse_tokens::<parsing::AstStruct>(
                        mac.tokens[0].clone().into_tokens()
                    ).map_err(|_| io::ErrorKind::Other)?.0
                } else if path_eq(&mac.path, &"ast_enum".into()) {
                    syn::parse_tokens::<parsing::AstEnum>(
                        mac.tokens[0].clone().into_tokens()
                    ).map_err(|_| io::ErrorKind::Other)?.0
                } else if path_eq(&mac.path, &"ast_enum_of_structs".into()) {
                    syn::parse_tokens::<parsing::AstEnumOfStructs>(
                        mac.tokens[0].clone().into_tokens()
                    ).map_err(|_| io::ErrorKind::Other)?.0
                } else {
                    continue
                };

                // Record our features on the parsed AstItems.
                for mut item in found {
                    features.to_tokens(&mut item.features);
                    lookup.insert(item.item.ident.clone(), item);
                }
            }
            _ => {}
        }
    }
    Ok(())
}

mod parsing {
    use super::AstItem;

    use synom::*;
    use synom::tokens::*;
    use syn::*;
    use syn::TokenTree;
    use quote::Tokens;

    // Parses #full - returns #[cfg(feature = "full")] if it is present, and
    // nothing otherwise.
    named!(full -> (Tokens, bool), map!(option!(do_parse!(
        syn!(Pound) >>
        id: syn!(Ident) >>
        cond_reduce!(id.as_ref() == "full", epsilon!()) >>
        (())
    )), |s| if s.is_some() {
        (quote!(#[cfg(feature = "full")]), true)
    } else {
        (quote!(), false)
    }));

    // Parses a simple AstStruct without the `pub struct` prefix.
    named!(ast_struct_inner -> AstItem, do_parse!(
        id: syn!(Ident) >>
        features: full >>
        rest: call!(TokenTree::parse_list) >>
        (AstItem {
            item: parse_tokens::<DeriveInput>(quote! {
                pub struct #id #(#rest)*
            })?,
            features: features.0,
            eos_full: features.1,
        })
    ));

    // ast_struct! parsing
    pub struct AstStruct(pub Vec<AstItem>);
    impl Synom for AstStruct {
        named!(parse -> Self, map!(braces!(do_parse!(
            many0!(call!(Attribute::parse_outer)) >>
            syn!(Pub) >>
            syn!(Struct) >>
            res: call!(ast_struct_inner) >>
            (res)
        )), |x| AstStruct(vec![x.0])));
    }

    // ast_enum! parsing
    pub struct AstEnum(pub Vec<AstItem>);
    impl Synom for AstEnum {
        named!(parse -> Self, map!(braces!(syn!(DeriveInput)), |x| {
            AstEnum(vec![AstItem {
                item: x.0,
                features: quote!(),
                eos_full: false,
            }])
        }));
    }

    // A single variant of an ast_enum_of_structs!
    struct EosVariant {
        name: Ident,
        member: Path,
        inner: Option<AstItem>,
    }
    named!(eos_variant -> EosVariant, do_parse!(
        many0!(call!(Attribute::parse_outer)) >>
        syn!(Pub) >>
        variant: syn!(Ident) >>
        member: map!(parens!(alt!(
            call!(ast_struct_inner) => { |x: AstItem| (Path::from(x.item.ident.clone()), Some(x)) }
            |
            syn!(Path) => { |x| (x, None) }
        )), |x| x.0) >>
        syn!(Comma) >>
        (EosVariant {
            name: variant,
            member: member.0,
            inner: member.1,
        })
    ));

    // ast_enum_of_structs! parsing
    pub struct AstEnumOfStructs(pub Vec<AstItem>);
    impl Synom for AstEnumOfStructs {
        named!(parse -> Self, map!(braces!(do_parse!(
            many0!(call!(Attribute::parse_outer)) >>
            syn!(Pub) >>
            syn!(Enum) >>
            id: syn!(Ident) >>
            body: braces!(many0!(call!(eos_variant))) >>
            option!(syn!(Ident)) >> // do_not_generate_to_tokens
            ({
                // XXX: This is really gross - we shouldn't have to convert the
                // tokens to strings to re-parse them.
                let enum_item = {
                    let variants = body.0.iter().map(|v| {
                        let name = v.name;
                        let member = &v.member;
                        quote!(#name(#member))
                    });
                    parse_tokens::<DeriveInput>(quote! {
                        pub enum #id { #(#variants),* }
                    })?
                };
                let mut items = vec![AstItem {
                    item: enum_item,
                    features: quote!(),
                    eos_full:  false,
                }];
                items.extend(body.0.into_iter().filter_map(|v| v.inner));
                AstEnumOfStructs(items)
            })
        )), |x| x.0));
    }
}

mod codegen {
    use super::{AstItem, Lookup};
    use syn::*;
    use quote::{Tokens, ToTokens};

    #[derive(Default)]
    pub struct State {
        pub visit_trait: String,
        pub visit_impl: String,
        pub visit_mut_trait: String,
        pub visit_mut_impl: String,
        pub fold_trait: String,
        pub fold_impl: String,
    }

    fn under_name(name: &Ident) -> Ident {
        use inflections::Inflect;
        name.as_ref().to_snake_case().into()
    }

    fn last_segment(ty: &Ty) -> Option<&PathSegment> {
        match *ty {
            Ty::Path(ref typath) => {
                if typath.qself.is_some() {
                    return None;
                }
                let name = if let Some(name) = typath.path.segments.last() { name } else {
                    return None;
                };

                Some(name.item())
            }
            _ => None,
        }
    }

    #[derive(Debug, Eq, PartialEq, Copy, Clone)]
    enum Kind {
        Visit,
        VisitMut,
        Fold,
    }

    fn first_param(params: &PathParameters) -> &Ty {
        let data = match *params {
            PathParameters::AngleBracketed(ref data) => data,
            _ => panic!("Expected at least 1 type parameter here"),
        };

        data.types.first().expect("Expected at least 1 type parameter here").item()
    }

    fn simple_visit(
        type_name: &Ident,
        lookup: &Lookup,
        kind: Kind,
        name: &Tokens,
        eos_full: &mut bool,
    ) -> Option<String> {
        if let Some(ref s) = lookup.get(type_name) {
            *eos_full = s.eos_full;
            Some(match kind {
                Kind::Visit => format!(
                    "_visitor.visit_{under_name}(&{name})",
                    under_name = under_name(type_name),
                    name = name,
                ),
                Kind::VisitMut => format!(
                    "_visitor.visit_{under_name}_mut(&mut {name})",
                    under_name = under_name(type_name),
                    name = name,
                ),
                Kind::Fold => format!(
                    "_visitor.fold_{under_name}({name})",
                    under_name = under_name(type_name),
                    name = name,
                )
            })
        } else {
            None
        }
    }

    fn box_visit(
        seg: &PathSegment,
        lookup: &Lookup,
        kind: Kind,
        name: &Tokens,
        eos_full: &mut bool,
    ) -> Option<String> {
        if let Some(res) = simple_visit(&seg.ident, lookup, kind, name, eos_full) {
            return Some(res);
        }

        if seg.ident == "Box" {
            let ty = first_param(&seg.parameters);
            if let Some(seg) = last_segment(ty) {
                if kind == Kind::Fold {
                    let name = quote!(*#name);
                    if let Some(val) = simple_visit(
                        &seg.ident,
                        lookup,
                        kind,
                        &name,
                        eos_full,
                    ) {
                        return Some(format!("Box::new({})", val));
                    }
                } else {
                    return simple_visit(&seg.ident, lookup, kind, name, eos_full);
                }
            }
        }

        None
    }

    fn vec_visit(
        seg: &PathSegment,
        lookup: &Lookup,
        kind: Kind,
        name: &Tokens,
        eos_full: &mut bool,
    ) -> Option<String> {
        if let Some(res) = box_visit(seg, lookup, kind, name, eos_full) {
            return Some(res);
        }

        if seg.ident == "Vec" || seg.ident == "Delimited" {
            let is_vec = seg.ident == "Vec";
            let ty = first_param(&seg.parameters);
            if let Some(seg) = last_segment(ty) {
                if let Some(val) = box_visit(seg, lookup, kind, &quote!(it), eos_full) {
                    return Some(match kind {
                        Kind::Visit => {
                            if is_vec {
                                format!(
                                    "for it in ({name}).iter() {{ {val} }}",
                                    name = name,
                                    val = val,
                                )
                            } else {
                                format!(
                                    "for el in ({name}).iter() {{ \
                                       let it = el.item(); \
                                       {val} \
                                    }}",
                                    name = name,
                                    val = val,
                                )
                            }
                        }
                        Kind::VisitMut => {
                            if is_vec {
                                format!(
                                    "for mut it in ({name}).iter_mut() {{ {val} }}",
                                    name = name,
                                    val = val,
                                )
                            } else {
                                format!(
                                    "for mut el in ({name}).iter_mut() {{ \
                                       let mut it = el.item_mut(); \
                                       {val} \
                                     }}",
                                    name = name,
                                    val = val,
                                )
                            }
                        }
                        Kind::Fold => {
                            format!(
                                "FoldHelper::lift({name}, |it| {{ {val} }})",
                                name = name,
                                val = val,
                            )
                        }
                    });
                }
            }
        }

        None
    }

    fn option_visit(
        seg: &PathSegment,
        lookup: &Lookup,
        kind: Kind,
        name: &Tokens,
        eos_full: &mut bool,
    ) -> Option<String> {
        if let Some(res) = vec_visit(seg, lookup, kind, name, eos_full) {
            return Some(res);
        }

        if seg.ident == "Option" {
            let ty = first_param(&seg.parameters);
            if let Some(seg) = last_segment(ty) {
                let it = match kind {
                    Kind::Fold => quote!(it),
                    _ => quote!(*it),
                };
                if let Some(val) = vec_visit(seg, lookup, kind, &it, eos_full) {
                    return Some(match kind {
                        Kind::Visit => format!(
                            "if let Some(ref it) = {name} {{ {val} }}",
                            name = name,
                            val = val,
                        ),
                        Kind::VisitMut => format!(
                            "if let Some(ref mut it) = {name} {{ {val} }}",
                            name = name,
                            val = val,
                        ),
                        Kind::Fold => format!(
                            "({name}).map(|it| {{ {val} }})",
                            name = name,
                            val = val,
                        ),
                    });
                }
            }
        }

        None
    }

    fn visit(ty: &Ty, lookup: &Lookup, kind: Kind, name: &Tokens) -> String {
        if let Some(seg) = last_segment(ty) {
            let mut eos_full = false;
            if let Some(res) = option_visit(seg, lookup, kind, name, &mut eos_full) {
                if eos_full {
                    return format!(
                        "full!({res})",
                        res = res);
                }
                return res;
            }
        }

        if kind == Kind::Fold {
            return name.to_string();
        }
        return format!("// Skipped field {}", name);
    }

    pub fn generate(state: &mut State, lookup: &Lookup, s: &AstItem) {
        let under_name = under_name(&s.item.ident);

        state.visit_trait.push_str(&format!(
            "{features}\n\
             fn visit_{under_name}(&mut self, i: &{ty}) {{ \
               visit_{under_name}(self, i) \
             }}\n",
            features = s.features,
            under_name = under_name,
            ty = s.item.ident,
        ));
        state.visit_mut_trait.push_str(&format!(
            "{features}\n\
             fn visit_{under_name}_mut(&mut self, i: &mut {ty}) {{ \
               visit_{under_name}_mut(self, i) \
             }}\n",
            features = s.features,
            under_name = under_name,
            ty = s.item.ident,
        ));
        state.fold_trait.push_str(&format!(
            "{features}\n\
             fn fold_{under_name}(&mut self, i: {ty}) -> {ty} {{ \
               fold_{under_name}(self, i) \
             }}\n",
            features = s.features,
            under_name = under_name,
            ty = s.item.ident,
        ));

        state.visit_impl.push_str(&format!(
            "{features}\n\
             pub fn visit_{under_name}<V: Visitor + ?Sized>(\
               _visitor: &mut V, _i: &{ty}) {{\n",
            features = s.features,
            under_name = under_name,
            ty = s.item.ident,
        ));
        state.visit_mut_impl.push_str(&format!(
            "{features}\n\
             pub fn visit_{under_name}_mut<V: VisitorMut + ?Sized>(\
               _visitor: &mut V, _i: &mut {ty}) {{\n",
            features = s.features,
            under_name = under_name,
            ty = s.item.ident,
        ));
        state.fold_impl.push_str(&format!(
            "{features}\n\
             pub fn fold_{under_name}<V: Folder + ?Sized>(\
               _visitor: &mut V, _i: {ty}) -> {ty} {{\n",
            features = s.features,
            under_name = under_name,
            ty = s.item.ident,
        ));

        // XXX:  This part is a disaster - I'm not sure how to make it cleaner though :'(
        match s.item.body {
            Body::Enum(ref e) => {
                let use_decl = format!("    use ::{}::*;\n", s.item.ident);
                state.visit_impl.push_str(&use_decl);
                state.visit_mut_impl.push_str(&use_decl);
                state.fold_impl.push_str(&use_decl);

                state.visit_impl.push_str("    match *_i {\n");
                state.visit_mut_impl.push_str("    match *_i {\n");
                state.fold_impl.push_str("    match _i {\n");
                for variant in &e.variants {
                    let fields: Vec<(&Field, Tokens)> = match variant.item().data {
                        VariantData::Struct(..) => {
                            panic!("Doesn't support enum struct variants");
                        }
                        VariantData::Tuple(ref fields, ..) => {
                            let binding = format!("        {}(", variant.item().ident);
                            state.visit_impl.push_str(&binding);
                            state.visit_mut_impl.push_str(&binding);
                            state.fold_impl.push_str(&binding);

                            let res = fields.iter().enumerate().map(|(idx, el)| {
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
                                Ident::from(name).to_tokens(&mut tokens);

                                (*el.item(), tokens)
                            }).collect();

                            state.visit_impl.push_str(") => {\n");
                            state.visit_mut_impl.push_str(") => {\n");
                            state.fold_impl.push_str(") => {\n");

                            res
                        }
                        VariantData::Unit => {
                            state.visit_impl.push_str(&format!(
                                "        {} => {{ }}\n",
                                variant.item().ident,
                            ));
                            state.visit_mut_impl.push_str(&format!(
                                "        {} => {{ }}\n",
                                variant.item().ident,
                            ));
                            state.fold_impl.push_str(&format!(
                                "        {0} => {{ {0} }}\n",
                                variant.item().ident
                            ));
                            continue
                        }
                    };

                    if fields.is_empty() {
                        state.visit_impl.push_str("            {}");
                        state.visit_mut_impl.push_str(") => {\n");
                        state.fold_impl.push_str(") => {\n");
                    }
                    state.fold_impl.push_str(&format!(
                        "            {} (\n",
                        variant.item().ident,
                    ));
                    for (field, binding) in fields {
                        state.visit_impl.push_str(&format!(
                            "            {};\n",
                            visit(&field.ty, lookup, Kind::Visit, &quote!(*#binding)),
                        ));
                        state.visit_mut_impl.push_str(&format!(
                            "            {};\n",
                            visit(&field.ty, lookup, Kind::VisitMut, &quote!(*#binding)),
                        ));
                        state.fold_impl.push_str(&format!(
                            "                {},\n",
                            visit(&field.ty, lookup, Kind::Fold, &binding),
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
            Body::Struct(ref v) => {
                let fields: Vec<(&Field, Tokens)> = match v.data {
                    VariantData::Struct(ref fields, ..) => {
                        state.fold_impl.push_str(&format!("    {} {{\n", s.item.ident));
                        fields.iter().map(|el| {
                            let id = el.item().ident;
                            (*el.item(), quote!(_i.#id))
                        }).collect()
                    }
                    VariantData::Tuple(ref fields, ..) => {
                        state.fold_impl.push_str(&format!("    {} (\n", s.item.ident));
                        fields.iter().enumerate().map(|(idx, el)| {
                            // XXX: Make sure we don't get the usize suffix!
                            let id = Ident::from(format!("{}", idx));
                            (*el.item(), quote!(_i.#id))
                        }).collect()
                    }
                    VariantData::Unit => vec![]
                };

                for (field, ref_toks) in fields {
                    state.visit_impl.push_str(&format!(
                        "    {};\n", visit(&field.ty, lookup, Kind::Visit, &ref_toks)
                    ));
                    state.visit_mut_impl.push_str(&format!(
                        "    {};\n", visit(&field.ty, lookup, Kind::VisitMut, &ref_toks)
                    ));
                    let fold = visit(&field.ty, lookup, Kind::Fold, &ref_toks);
                    if let Some(ref name) = field.ident {
                        state.fold_impl.push_str(&format!("        {}: {},\n", name, fold));
                    } else {
                        state.fold_impl.push_str(&format!("        {},\n", fold));
                    }
                }

                match v.data {
                    VariantData::Struct(..) => state.fold_impl.push_str("    }\n"),
                    VariantData::Tuple(..) => state.fold_impl.push_str("    )\n"),
                    VariantData::Unit => {}
                };
            }
        }

        // Close the impl body
        state.visit_impl.push_str("}\n");
        state.visit_mut_impl.push_str("}\n");
        state.fold_impl.push_str("}\n");
    }
}

fn main() {
    let mut lookup = BTreeMap::new();
    load_file(SYN_CRATE_ROOT, quote!(), &mut lookup).unwrap();

    let mut state = Default::default();
    for s in lookup.values() {
        codegen::generate(&mut state, &lookup, s);
    }

    let full_macro = "
#[cfg(feature = \"full\")]
macro_rules! full {
    ($e:expr) => { $e }
}

#[cfg(not(feature = \"full\"))]
macro_rules! full {
    ($e:expr) => { unreachable!() }
}
";

    let mut fold_file = File::create(FOLD_SRC).unwrap();
    write!(fold_file, "
// THIS FILE IS AUTOMATICALLY GENERATED; DO NOT EDIT

//! A Folder represents an AST->AST fold; it accepts an AST piece,
//! and returns a piece of the same type.

// Unreachable code is generated sometimes without the full feature.
#![allow(unreachable_code)]

use *;
use synom::delimited::Delimited;

trait FoldHelper {{
    type Item;
    fn lift<F>(self, f: F) -> Self where F: FnMut(Self::Item) -> Self::Item;
}}

impl<T> FoldHelper for Vec<T> {{
    type Item = T;
    fn lift<F>(self, f: F) -> Self where F: FnMut(Self::Item) -> Self::Item {{
        self.into_iter().map(f).collect()
    }}
}}

impl<T, U> FoldHelper for Delimited<T, U> {{
    type Item = T;
    fn lift<F>(self, mut f: F) -> Self where F: FnMut(Self::Item) -> Self::Item {{
        self.into_iter().map(|elem| {{
            let (t, u) = elem.into_tuple();
            (f(t), u)
        }}).collect::<Vec<(T, Option<U>)>>().into()
    }}
}}

{full_macro}

/// AST->AST fold.
///
/// Each method of the Folder trait is a hook to be potentially overridden. Each
/// method's default implementation recursively visits the substructure of the
/// input via the `walk` functions, which perform an \"identity fold\", that
/// is, they return the same structure that they are given (for example the
/// `fold_file` method by default calls `fold::walk_file`).
///
/// If you want to ensure that your code handles every variant
/// explicitly, you need to override each method.  (And you also need
/// to monitor future changes to `Folder` in case a new method with a
/// new default implementation gets introduced.)
pub trait Folder {{
{fold_trait}
}}

{fold_impl}
",
           full_macro = full_macro,
           fold_trait = state.fold_trait,
           fold_impl = state.fold_impl).unwrap();

    let mut visit_file = File::create(VISIT_SRC).unwrap();
    write!(visit_file, "
// THIS FILE IS AUTOMATICALLY GENERATED; DO NOT EDIT

//! AST walker. Each overridden visit method has full control over what
//! happens with its node, it can do its own traversal of the node's children,
//! call `visit::walk_*` to apply the default traversal algorithm, or prevent
//! deeper traversal by doing nothing.

use *;

{full_macro}

/// Each method of the Visitor trait is a hook to be potentially
/// overridden.  Each method's default implementation recursively visits
/// the substructure of the input via the corresponding `walk` method;
/// e.g. the `visit_mod` method by default calls `visit::walk_mod`.
///
/// If you want to ensure that your code handles every variant
/// explicitly, you need to override each method.  (And you also need
/// to monitor future changes to `Visitor` in case a new method with a
/// new default implementation gets introduced.)
pub trait Visitor {{
{visit_trait}
}}

{visit_impl}
",
           full_macro = full_macro,
           visit_trait = state.visit_trait,
           visit_impl = state.visit_impl).unwrap();

    let mut visit_mut_file = File::create(VISIT_MUT_SRC).unwrap();
    write!(visit_mut_file, "
// THIS FILE IS AUTOMATICALLY GENERATED; DO NOT EDIT

//! AST walker. Each overridden visit method has full control over what
//! happens with its node, it can do its own traversal of the node's children,
//! call `visit::walk_*` to apply the default traversal algorithm, or prevent
//! deeper traversal by doing nothing.

use *;

{full_macro}

/// Each method of the VisitorMut trait is a hook to be potentially
/// overridden.  Each method's default implementation recursively visits
/// the substructure of the input via the corresponding `walk` method;
/// e.g. the `visit_mod` method by default calls `visit::walk_mod`.
///
/// If you want to ensure that your code handles every variant
/// explicitly, you need to override each method.  (And you also need
/// to monitor future changes to `VisitorMut` in case a new method with a
/// new default implementation gets introduced.)
pub trait VisitorMut {{
{visit_mut_trait}
}}

{visit_mut_impl}
",
           full_macro = full_macro,
           visit_mut_trait = state.visit_mut_trait,
           visit_mut_impl = state.visit_mut_impl).unwrap();
}
